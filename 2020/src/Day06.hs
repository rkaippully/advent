module Day06
  ( day06Part1
  , day06Part2
  ) where

import Control.Arrow
import qualified Data.Set as S

toGroups :: [String] -> [[String]]
toGroups = foldr go []
  where
    go :: String -> [[String]] -> [[String]]
    go "" xs      = []:xs
    go x (xs:xss) = (x:xs):xss
    go x []       = [[x]]

linesToSets :: [String] -> [S.Set Char]
linesToSets = map S.fromList

day06Part1 :: String -> String
day06Part1 = lines
             >>> toGroups
             >>> map linesToSets
             >>> map (S.size . foldl1 S.union)
             >>> sum
             >>> show

day06Part2 :: String -> String
day06Part2 = lines
             >>> toGroups
             >>> map linesToSets
             >>> map (S.size . foldl1 S.intersection)
             >>> sum
             >>> show
