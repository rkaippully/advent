module Day02 (
  day02Part1,
  day02Part2,
) where

import Data.List (foldl')

data Move
  = Forward Integer
  | Down Integer
  | Up Integer

parse :: String -> [Move]
parse = map (mkMove . words) . lines
  where
    mkMove :: [String] -> Move
    mkMove ["forward", x] = Forward (read x)
    mkMove ["down", x] = Down (read x)
    mkMove ["up", x] = Up (read x)
    mkMove x = error ("Bad input: " <> show x)

type Horizontal = Integer
type Vertical = Integer

move1 :: (Horizontal, Vertical) -> Move -> (Horizontal, Vertical)
move1 (h, v) (Forward x) = (h + x, v)
move1 (h, v) (Down x) = (h, v + x)
move1 (h, v) (Up x) = (h, v - x)

day02Part1 :: String -> String
day02Part1 = show . uncurry (*) . foldl' move1 (0, 0) . parse

type Aim = Integer

move2 :: ((Horizontal, Vertical), Aim) -> Move -> ((Horizontal, Vertical), Aim)
move2 ((h, v), a) (Forward x) = ((h + x, v + a * x), a)
move2 ((h, v), a) (Down x) = ((h, v), a + x)
move2 ((h, v), a) (Up x) = ((h, v), a - x)

day02Part2 :: String -> String
day02Part2 = show . uncurry (*) . fst . foldl' move2 ((0, 0), 0) . parse
