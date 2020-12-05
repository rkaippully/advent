module Day03
  ( day03Part1
  , day03Part2
  ) where

import Relude

newtype Forest = Forest { unForest :: [[Bool]] }
  deriving (Show)

makeForest :: Text -> Forest
makeForest = Forest . fromMaybe (error "parse failed") . mapM makeTreeRow . lines

makeTreeRow :: Text -> Maybe [Bool]
makeTreeRow = fmap cycle . mapM f . toString
  where
    f '.' = Just False
    f '#' = Just True
    f _   = Nothing

takeStep :: Int -> Int -> Forest -> Forest
takeStep right down = Forest . map (drop right) . drop down . unForest

isAtTree :: Forest -> Bool
isAtTree (Forest ((True:_):_)) = True
isAtTree _                     = False

hasMoreToGo :: Forest -> Bool
hasMoreToGo = not . null . unForest

checkSlope :: Text -> (Int, Int) -> Integer
checkSlope s (right, down) = makeForest s
                             & iterate (takeStep right down)
                             & takeWhile hasMoreToGo
                             & map isAtTree
                             & filter identity
                             & length
                             & toInteger

day03Part1 :: Text -> Text
day03Part1 s = show $ checkSlope s (3, 1)

day03Part2 :: Text -> Text
day03Part2 s = show $ product $ map (checkSlope s) [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
