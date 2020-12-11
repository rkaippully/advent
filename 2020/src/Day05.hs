module Day05
  ( day05Part1
  , day05Part2
  ) where

import Control.Arrow
import Data.Foldable
import Data.List (groupBy, sort)

toSeatId :: String -> Int
toSeatId = foldl' f 0
  where
    f :: Int -> Char -> Int
    f acc c | c == 'F' || c == 'L' = acc*2
            | c == 'B' || c == 'R' = acc*2 + 1
            | otherwise            = error "Invalid char"

day05Part1 :: String -> String
day05Part1 = lines
             >>> map toSeatId
             >>> maximum
             >>> show

day05Part2 :: String -> String
day05Part2 = lines
             >>> map toSeatId
             >>> sort
             >>> zip [0..]
             >>> groupBy (\(x1, y1) (x2, y2) -> y1-x1 == y2-x2)
             >>> emptySeatId
             >>> show
  where
    emptySeatId :: [[(Int, Int)]] -> Int
    emptySeatId [_, (_, sid):_] = sid - 1
    emptySeatId _               = undefined
