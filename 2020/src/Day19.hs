module Day19
  ( day19Part1
  , day19Part2
  ) where

import Data.Bifunctor
import Data.Foldable
import Data.Function
import Data.Functor
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Maybe
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

data Lex = Leaf Char
         | Seq [Lex]
         | Alt Lex Lex
         deriving (Show)

makeLexers :: Map String String -> Map String Lex
makeLexers rules = lexers
  where
    lexers = M.map expToLexer rules

    expToLexer :: String -> Lex
    expToLexer = fromJust . parseMaybe expr

    expr :: Parser Lex
    expr = try (string " \"a\"" $> Leaf 'a')
           <|> try (string " \"b\"" $> Leaf 'b')
           <|> try alternative
           <|> try nums

    nums :: Parser Lex
    nums = do
      ns <- some number
      pure $ Seq $ rulesLexer ns

    alternative :: Parser Lex
    alternative = do
      ns <- some number
      ms <- string " |" *> some number
      pure $ Alt (Seq $ rulesLexer ns) (Seq $ rulesLexer ms)

    rulesLexer :: [String] -> [Lex]
    rulesLexer = map (lexers!)

number :: Parser String
number = try (char ' ' *> some digitChar)

matchLexer :: Lex -> String -> [String]
matchLexer (Leaf _) [] = []
matchLexer (Leaf c) (x:xs) | c == x = [xs]
                      | otherwise = []
matchLexer (Seq []) s = [s]
matchLexer (Seq (l:ls)) s = matchLexer l s >>= matchLexer (Seq ls)
matchLexer (Alt l r) s = matchLexer l s ++ matchLexer r s

isMatching :: Lex -> String -> Bool
isMatching l s = isJust $ find (== "") $ matchLexer l s

day19Part1 :: String -> String
day19Part1 s = show $ length $ filter id $ map (isMatching lexer) (tail inp)
  where
    (prg, inp) = break (== "") $ lines s
    lexer = map (second tail . break (== ':')) prg
            & M.fromList
            & makeLexers
            & (! "0")

day19Part2 :: String -> String
day19Part2 s = show $ length $ filter id $ map (isMatching lexer) (tail inp)
  where
    (prg, inp) = break (== "") $ lines s
    lexer = map (second tail . break (== ':')) prg
            & M.fromList
            & M.insert "8" " 42 | 42 8"
            & M.insert "11" " 42 31 | 42 11 31"
            & makeLexers
            & (! "0")
