module Day10
  ( day10Part1
  , day10Part2
  ) where

import Data.Function
import Data.List

day10Part1 :: String -> String
day10Part1 s = sort diffs
               & group
               & map length
               & product
               & show
  where
    ns = adapterArray s
    diffs = zipWith (-) (tail ns) ns

adapterArray :: String -> [Integer]
adapterArray s = lines s
                 & map read
                 & sort
                 & (\xs -> 0 : xs ++ [maximum xs + 3])

day10Part2 :: String -> String
day10Part2 s = group diffs
               & filter ((== 1) . head)
               & map ((ways !!) . length)
               & (product :: [Integer] -> Integer)
               & show
  where
    ns = adapterArray s
    diffs = zipWith (-) (tail ns) ns

    -- ways[n] is the number of ways to arrange a sequence of n
    -- adapters each with a difference of 1 between them.
    -- ways[n] = ways[n-1] + ways[n-2] + ways[n-3]
    ways = 1 : 1 : 2 : zipWith3 (\x y z -> x + y + z) ways (drop 1 ways) (drop 2 ways)
