module Day01 (
  day01Part1,
  day01Part2,
) where

import Data.List (groupBy, sort)
import Data.Ord (Down (..))

day01Part1 :: String -> String
day01Part1 = show . head . sortedCalories

day01Part2 :: String -> String
day01Part2 = show . sum . take 3 . sortedCalories

sortedCalories :: String -> [Integer]
sortedCalories =
  map getDown
    . sort @(Down Integer)
    . map (sum . map (Down . read))
    . filter (/= [""])
    . groupBy (\a b -> a /= "" && b /= "")
    . lines
