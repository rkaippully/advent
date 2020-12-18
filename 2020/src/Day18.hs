module Day18
  ( day18Part1
  , day18Part2
  ) where

import Data.Char
import Data.Function
import Data.Maybe
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

data Expr = Lit Integer
          | Add Expr Expr
          | Mul Expr Expr
          deriving (Show)

parseExpr1 :: String -> Expr
parseExpr1 = fromJust . parseMaybe expr1 . filter (not . isSpace)

expr1 :: Parser Expr
expr1 = do
  v <- value1
  ts <- many exprTail1
  pure $ foldl (flip (.)) id ts v

exprTail1 :: Parser (Expr -> Expr)
exprTail1 = addTail1
           <|> mulTail1

addTail1 :: Parser (Expr -> Expr)
addTail1 = do
  v <- char '+' *> value1
  pure $ \x -> Add x v

mulTail1 :: Parser (Expr -> Expr)
mulTail1 = do
  v <- char '*' *> value1
  pure $ \x -> Mul x v

value1 :: Parser Expr
value1 = try (Lit <$> number)
        <|> char '(' *> expr1 <* char ')'

number :: Parser Integer
number = read <$> some digitChar

parseExpr2 :: String -> Expr
parseExpr2 = fromJust . parseMaybe expr2 . filter (not . isSpace)

expr2 :: Parser Expr
expr2 = do
  v <- mul2
  ts <- many mulTail2
  pure $ foldl (flip (.)) id ts v

mul2 :: Parser Expr
mul2 = do
  v <- add2
  ts <- many addTail2
  pure $ foldl (flip (.)) id ts v

mulTail2 :: Parser (Expr -> Expr)
mulTail2 = do
  v <- char '*' *> mul2
  pure $ \x -> Mul x v

add2 :: Parser Expr
add2 = do
  v <- value2
  ts <- many addTail2
  pure $ foldl (flip (.)) id ts v

addTail2 :: Parser (Expr -> Expr)
addTail2 = do
  v <- char '+' *> value2
  pure $ \x -> Add x v

value2 :: Parser Expr
value2 = try (Lit <$> number)
        <|> char '(' *> expr2 <* char ')'

evalExpr :: Expr -> Integer
evalExpr (Lit n)     = n
evalExpr (Add e1 e2) = evalExpr e1 + evalExpr e2
evalExpr (Mul e1 e2) = evalExpr e1 * evalExpr e2

day18Part1 :: String -> String
day18Part1 s = lines s
               & map (evalExpr . parseExpr1)
               & sum
               & show

day18Part2 :: String -> String
day18Part2 s = lines s
               & map (evalExpr . parseExpr2)
               & sum
               & show

