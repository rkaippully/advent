module Day01 (
  part1,
  part2,
) where

part1 :: String -> String
part1 = show . sum . map (toMass . read) . lines

toMass :: Int -> Int
toMass x = (x `div` 3) - 2

part2 :: String -> String
part2 = show . sum . map (toRecMass . read) . lines

toRecMass :: Int -> Int
toRecMass x = sum $ takeWhile (> 0) $ iterate toMass (toMass x)
