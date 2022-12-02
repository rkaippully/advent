module Day01 (
  day01Part1,
  day01Part2,
) where

day01Part1 :: String -> String
day01Part1 s =
  let depths = map read (lines s) :: [Integer]
      depths' = tail depths
   in show $ length $ filter (uncurry (>)) $ zip depths' depths

day01Part2 :: String -> String
day01Part2 s =
  let depths = map read (lines s) :: [Integer]
      depths' = tail depths
      depths'' = tail depths'
      sums = zipWith3 (\a b c -> a + b + c) depths'' depths' depths
      sums' = tail sums
   in show $ length $ filter (uncurry (>)) $ zip sums' sums
