module Day03
  ( day03Part1
  , day03Part2
  ) where

import Data.Function
newtype Forest = Forest { unForest :: [[Bool]] }
  deriving (Show)

makeForest :: String -> Forest
makeForest = Forest . map makeTreeRow . lines

makeTreeRow :: String -> [Bool]
makeTreeRow = cycle . map f
  where
    f '.' = False
    f '#' = True
    f _   = error "invalid input"

takeStep :: Int -> Int -> Forest -> Forest
takeStep right down = Forest . map (drop right) . drop down . unForest

isAtTree :: Forest -> Bool
isAtTree (Forest ((True:_):_)) = True
isAtTree _                     = False

hasMoreToGo :: Forest -> Bool
hasMoreToGo = not . null . unForest

checkSlope :: String -> (Int, Int) -> Integer
checkSlope s (right, down) = makeForest s
                             & iterate (takeStep right down)
                             & takeWhile hasMoreToGo
                             & filter isAtTree
                             & length
                             & toInteger

day03Part1 :: String -> String
day03Part1 s = show $ checkSlope s (3, 1)

day03Part2 :: String -> String
day03Part2 s = show $ product $ map (checkSlope s) [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
