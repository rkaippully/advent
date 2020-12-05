module Day05
  ( day05Part1
  , day05Part2
  ) where

import Data.List (groupBy, maximum)
import Relude

toSeatId :: String -> Int
toSeatId = foldl' f 0
  where
    f :: Int -> Char -> Int
    f acc c | c == 'F' || c == 'L' = acc*2
            | c == 'B' || c == 'R' = acc*2 + 1
            | otherwise            = error "Invalid char"

day05Part1 :: Text -> Text
day05Part1 = lines
             >>> map (toSeatId . toString)
             >>> maximum
             >>> show

day05Part2 :: Text -> Text
day05Part2 = lines
             >>> map (toSeatId . toString)
             >>> sort
             >>> zip [0..]
             >>> groupBy (\(x1, y1) (x2, y2) -> y1-x1 == y2-x2)
             >>> emptySeatId
             >>> show
  where
    emptySeatId :: [[(Int, Int)]] -> Int
    emptySeatId [_, (_, id):_] = id - 1
