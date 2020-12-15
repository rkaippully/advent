module Day14
  ( day14Part1
  , day14Part2
  ) where

import Control.Monad
import Data.Bits
import Data.Foldable
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

type Addr = Integer
type Value = Integer

data Bit = X | One | Zero

data Insn = SetMem Addr Value
          | SetMask [(Int, Bit)]

parseProgram :: String -> [Insn]
parseProgram = map (fromJust . parseMaybe insn) . lines

type Parser = Parsec Void String

insn :: Parser Insn
insn = setMem <|> setMask

setMem :: Parser Insn
setMem = do
  addr <- string "mem[" *> number
  val <- string "] = " *> number
  pure $ SetMem addr val

setMask :: Parser Insn
setMask = do
  s <- string "mask = " *> many alphaNumChar
  pure $ SetMask $ fst $ foldl' f ([], 35) s

  where
    f :: ([(Int, Bit)], Int) -> Char -> ([(Int, Bit)], Int)
    f (bs, n) 'X' = ((n, X):bs, n-1)
    f (bs, n) '1' = ((n, One):bs, n-1)
    f (bs, n) '0' = ((n, Zero):bs, n-1)
    f _ _         = error "bad bitmask"

number :: Parser Integer
number = read <$> some digitChar <* space

runProgram1 :: [Insn] -> Integer
runProgram1 = getSum . foldMap Sum . fst . foldl' exec (M.empty, id)
  where
    exec :: (M.Map Addr Value, Value -> Value) -> Insn -> (M.Map Addr Value, Value -> Value)
    exec (m, g) (SetMem addr val) = (M.insert addr (g val) m, g)
    exec (m, _) (SetMask bs)      = (m, valueTrans bs)

    valueTrans :: [(Int, Bit)] -> (Value -> Value)
    valueTrans = appEndo . foldMap f

    f :: (Int, Bit) -> Endo Value
    f = Endo . \case
      (_, X)    -> id
      (i, One)  -> (`setBit` i)
      (i, Zero) -> (`clearBit` i)

runProgram2 :: [Insn] -> Integer
runProgram2 = getSum . foldMap Sum . fst . foldl' exec (M.empty, pure)
  where
    exec :: (M.Map Addr Value, Addr -> [Addr]) -> Insn -> (M.Map Addr Value, Addr -> [Addr])
    exec (m, g) (SetMem addr val) = (foldl' (\m' addr' -> M.insert addr' val m') m (g addr), g)
    exec (m, _) (SetMask bs)      = (m, addrTrans bs)

    addrTrans :: [(Int, Bit)] -> (Addr -> [Addr])
    addrTrans = foldl' f pure

    f :: (Addr -> [Addr]) -> (Int, Bit) -> (Addr -> [Addr])
    f g (_, Zero) = g
    f g (i, One)  = g >=> \a -> [a `setBit` i]
    f g (i, X)    = g >=> \a -> [a `setBit` i, a `clearBit` i]

day14Part1 :: String -> String
day14Part1 = show . runProgram1 . parseProgram

day14Part2 :: String -> String
day14Part2 = show . runProgram2 . parseProgram
