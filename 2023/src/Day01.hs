module Day01 (
  part1,
  part2,
) where

import qualified Data.Text as Text
import Relude

part1 :: String -> String
part1 = show . sum . map (calibration onlyDigits) . lines . toText

part2 :: String -> String
part2 = show . sum . map (calibration digitsAndWords) . lines . toText

calibration :: [(Text, Int)] -> Text -> Int
calibration nums s = leadingDigit nums s * 10 + trailingDigit nums s

leadingDigit :: [(Text, Int)] -> Text -> Int
leadingDigit _ "" = error "No digit found"
leadingDigit nums s =
  case find (\(numStr, _) -> numStr `Text.isPrefixOf` s) nums of
    Nothing -> leadingDigit nums (Text.tail s)
    Just (_, n) -> n

trailingDigit :: [(Text, Int)] -> Text -> Int
trailingDigit _ "" = error "No digit found"
trailingDigit nums s =
  case find (\(numStr, _) -> numStr `Text.isSuffixOf` s) nums of
    Nothing -> trailingDigit nums (Text.init s)
    Just (_, n) -> n

onlyDigits :: [(Text, Int)]
onlyDigits = [("1", 1), ("2", 2), ("3", 3), ("4", 4), ("5", 5), ("6", 6), ("7", 7), ("8", 8), ("9", 9)]

digitsAndWords :: [(Text, Int)]
digitsAndWords = onlyDigits ++ [("one", 1), ("two", 2), ("three", 3), ("four", 4), ("five", 5), ("six", 6), ("seven", 7), ("eight", 8), ("nine", 9)]
