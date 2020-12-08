module Day01
  ( day01Part1
  , day01Part2
  ) where

toAmounts :: String -> [Word]
toAmounts = map read . words

day01Part1 :: String -> String
day01Part1 s =
  let amts = toAmounts s
  in case [(amt1, amt2) | amt1 <- amts, amt2 <- amts, amt1 + amt2 == 2020] of
       (amt1, amt2):_ -> show $ amt1 * amt2
       _              -> error "No matching amounts"

day01Part2 :: String -> String
day01Part2 s =
  let amts = toAmounts s
  in case [(amt1, amt2, amt3) | amt1 <- amts
                              , amt2 <- amts
                              , amt3 <- amts
                              , amt1 + amt2 + amt3 == 2020] of
       (amt1, amt2, amt3):_ -> show $ amt1 * amt2 * amt3
       _                    -> error "No matching amounts"
