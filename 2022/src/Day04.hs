module Day04 (
  day04Part1,
  day04Part2,
) where

import Data.IntSet (IntSet, disjoint, isSubsetOf)
import Data.Void (Void)
import Text.Megaparsec (Parsec, runParser, some)
import Text.Megaparsec.Char (char, digitChar)

type Range = IntSet

isContained :: (Range, Range) -> Bool
isContained (r1, r2) = r1 `isSubsetOf` r2 || r2 `isSubsetOf` r1

isOverlapping :: (Range, Range) -> Bool
isOverlapping (r1, r2) = not $ disjoint r1 r2

lineToRanges :: String -> (Range, Range)
lineToRanges s =
  let parser :: Parsec Void String (Range, Range)
      parser = do
        a <- read <$> (some digitChar <* char '-')
        b <- read <$> (some digitChar <* char ',')
        c <- read <$> (some digitChar <* char '-')
        d <- read <$> some digitChar
        pure ([a .. b], [c .. d])
   in case runParser parser "" s of
        Left e -> error (show e)
        Right x -> x

day04Part1 :: String -> String
day04Part1 = show . length . filter isContained . map lineToRanges . lines

day04Part2 :: String -> String
day04Part2 = show . length . filter isOverlapping . map lineToRanges . lines
