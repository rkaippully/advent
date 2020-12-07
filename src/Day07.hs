module Day07
  ( day07Part1
  , day07Part2
  ) where

import Data.Functor
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import Data.String
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

newtype Color = Color String
  deriving (Show, Eq, Ord, IsString) via String

parseLine :: String -> (Color, [(Color, Integer)])
parseLine = fromMaybe (error "parse fail") . parseMaybe line

type Parser = Parsec Void String

line :: Parser (Color, [(Color, Integer)])
line = do
  c <- color
  string "bags contain "
  ds <- destinations
  char '.'
  pure (c, ds)

color :: Parser Color
color = do
  c1 <- some letterChar <* space
  c2 <- some letterChar <* space
  pure $ Color $ c1 <> " " <> c2

destinations :: Parser [(Color, Integer)]
destinations = string "no other bags" $> []
               <|> destination `sepBy` string ", "

destination :: Parser (Color, Integer)
destination = do
  n <- number
  c <- color
  string "bag" <* optional (char 's')
  pure (c, n)

number :: Parser Integer
number = read <$> some digitChar <* space

day07Part1 :: String -> String
day07Part1 s = show $ length (filter canReachShinyGold $ M.keys m) - 1
  where
    m :: Map Color [(Color, Integer)]
    m = M.fromList $ map parseLine $ lines s

    canReachShinyGold :: Color -> Bool
    canReachShinyGold "shiny gold" = True
    canReachShinyGold c            = getAny $ mconcat $ Any . canReachShinyGold . fst <$> m!c

day07Part2 :: String -> String
day07Part2 s = show $ weightOf "shiny gold"
  where
    m :: Map Color [(Color, Integer)]
    m = M.fromList $ map parseLine $ lines s

    weightOf :: Color -> Integer
    weightOf c = sum $ map (\(c', w) -> w*(1 + weightOf c')) $ m!c
