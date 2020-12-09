module Day09
  ( day09Part1
  , day09Part2
  ) where

import Data.Function
import Data.List
import qualified Data.Set as S

solve1 :: [Integer] -> Integer
solve1 ns = zip [0..] ns
            & map (\(idx, n) -> (n, S.notMember n (sums idx)))
            & drop 25
            & filter snd
            & head
            & fst
  where
    sums :: Int -> S.Set Integer
    sums idx = let ns' = take 25 (drop (idx - 25) ns) in S.fromList [x+y | x<-ns', y<-ns', x /= y]

day09Part1 :: String -> String
day09Part1 s = show $ solve1 (read <$> lines s)

day09Part2 :: String -> String
day09Part2 s = filter ((== target) . sum) ns'
               & head
               & (\xs -> minimum xs + maximum xs)
               & show
  where
    ns :: [Integer]
    ns = read <$> lines s

    target :: Integer
    target = solve1 ns

    ns' :: [[Integer]]
    ns' = filter ((> 1) . length) $ concatMap inits $ tails ns
