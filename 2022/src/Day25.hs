module Day25 (
  day25Part1,
  day25Part2,
) where

import Data.List (foldl')

snafuToInt :: String -> Int
snafuToInt = foldl' f 0
  where
    f :: Int -> Char -> Int
    f acc c =
      acc * 5
        + case c of
          '2' -> 2
          '1' -> 1
          '0' -> 0
          '-' -> -1
          '=' -> -2
          _ -> error "Bad input"

intToSnafu :: Int -> String
intToSnafu 0 = "0"
intToSnafu x = go x
  where
    go = \case
      0 -> ""
      n -> case n `divMod` 5 of
        (q, 0) -> go q ++ "0"
        (q, 1) -> go q ++ "1"
        (q, 2) -> go q ++ "2"
        (q, 3) -> go (q + 1) ++ "="
        (q, 4) -> go (q + 1) ++ "-"
        _ -> error "Can't happen"

day25Part1 :: String -> String
day25Part1 = intToSnafu . sum . map snafuToInt . lines

day25Part2 :: String -> String
day25Part2 = error "not implemented"
