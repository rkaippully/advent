module Day06 (
  day06Part1,
  day06Part2,
) where

import Data.Function ((&))
import Data.List (tails)
import qualified Data.Set as Set

uniqueCharIdx :: Int -> [Char] -> Int
uniqueCharIdx count s =
  tails s
    & zipWith (\i cs -> (i, Set.fromList (take count cs))) [count ..]
    & filter (\(_, cs) -> Set.size cs == count)
    & head
    & fst

day06Part1 :: String -> String
day06Part1 = show . uniqueCharIdx 4

day06Part2 :: String -> String
day06Part2 = show . uniqueCharIdx 14
