module Day03 (
  day03Part1,
  day03Part2,
) where

import Data.Char (isAsciiLower, isAsciiUpper, ord)
import Data.List (groupBy)
import qualified Data.Set as Set

solve :: ([String] -> [[String]]) -> String -> String
solve groupStrings =
  show . sum . map (priority . commonItem) . groupStrings . lines

commonItem :: [String] -> Char
commonItem = head . Set.elems . foldl1 Set.intersection . map Set.fromList

priority :: Char -> Integer
priority c
  | isAsciiLower c = toInteger $ ord c - ord 'a' + 1
  | isAsciiUpper c = toInteger $ ord c - ord 'A' + 27
  | otherwise = error "Invalid input"

day03Part1 :: String -> String
day03Part1 =
  let
    compartmentalise :: String -> [String]
    compartmentalise s =
      let n = length s
          (s1, s2) = splitAt (n `div` 2) s
       in [s1, s2]
   in
    solve (map compartmentalise)

day03Part2 :: String -> String
day03Part2 =
  let
    f :: [String] -> [[String]]
    f = map (map snd) . groupBy groupOfThree . zip [0 ..]

    groupOfThree :: (Int, a) -> (Int, a) -> Bool
    groupOfThree (n1, _) (n2, _) = n1 `div` 3 == n2 `div` 3
   in
    solve f
