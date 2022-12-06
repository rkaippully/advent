module Day06 (
  day06Part1,
  day06Part2,
) where

import qualified Data.Set as Set

uniqueCharIdx :: Int -> [Char] -> Int
uniqueCharIdx count = go . zip [0 ..]
  where
    go :: [(Int, Char)] -> Int
    go [] = error "Marker not found"
    go xs@((n, _) : _) =
      let
        chars = Set.fromList $ map snd $ take count xs
       in
        if Set.size chars == count
          then n + count
          else go (tail xs)

day06Part1 :: String -> String
day06Part1 = show . uniqueCharIdx 4

day06Part2 :: String -> String
day06Part2 = show . uniqueCharIdx 14
