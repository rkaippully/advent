module Day06
  ( day06Part1
  , day06Part2
  ) where

import Data.List (foldl1)
import qualified Data.Set as S
import qualified Data.Text as T
import Relude

linesToSets :: [Text] -> [Set Char]
linesToSets = map (S.fromList . toString)

day06Part1 :: Text -> Text
day06Part1 = T.splitOn "\n\n"
             >>> map (linesToSets . lines)
             >>> map (S.size . foldl1 S.union)
             >>> sum
             >>> show

day06Part2 :: Text -> Text
day06Part2 = T.splitOn "\n\n"
             >>> map (linesToSets . lines)
             >>> map (S.size . foldl1 S.intersection)
             >>> sum
             >>> show
