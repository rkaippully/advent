module Day10 (
  day10Part1,
  day10Part2,
) where

import Data.List (foldl', scanl')
import Data.Map ((!))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Insn = AddxStart | Addx Integer | Noop
  deriving stock (Show)

parseInput :: String -> [Insn]
parseInput = concatMap (f . words) . lines
  where
    f :: [String] -> [Insn]
    f ["addx", x] = [AddxStart, Addx (read x)]
    f ["noop"] = [Noop]
    f _ = error "Bad input"

type Cycle = Integer
type Register = Integer

runProgram1 :: [Insn] -> [Register]
runProgram1 = scanl' f 1
  where
    f :: Register -> Insn -> Register
    f r (Addx n) = r + n
    f r AddxStart = r
    f r Noop = r

signalStrength :: [Register] -> Integer
signalStrength = sum . zipWith f [1 ..]
  where
    f :: Cycle -> Register -> Integer
    f c r
      | c == 20 || (c - 20) `mod` 40 == 0 = c * r
      | otherwise = 0

day10Part1 :: String -> String
day10Part1 = show . signalStrength . runProgram1 . parseInput

type CRTPos = Integer
type State2 = (Register, CRTPos, Map CRTPos Char)

runProgram2 :: [Insn] -> State2
runProgram2 = foldl' f (1, 0, Map.empty)
  where
    f :: State2 -> Insn -> State2
    f (r, c, m) (Addx n) = (r + n, advance c, drawPixel c r m)
    f (r, c, m) AddxStart = (r, advance c, drawPixel c r m)
    f (r, c, m) Noop = (r, advance c, drawPixel c r m)

    advance :: CRTPos -> CRTPos
    advance c = (c + 1) `mod` 240

    drawPixel :: CRTPos -> Register -> Map CRTPos Char -> Map CRTPos Char
    drawPixel c r = Map.insert c (if abs (r - x) <= 1 then '#' else '.')
      where
        x = c `mod` 40

display :: State2 -> String
display (_, _, m) = unlines $ map isLit [0 .. 5]
  where
    isLit :: Integer -> String
    isLit y = [m ! (y * 40 + x) | x <- [0 .. 39]]

day10Part2 :: String -> String
day10Part2 = display . runProgram2 . parseInput
