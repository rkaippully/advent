module Day01
  ( day01Part1
  , day01Part2
  ) where

import Relude


toAmounts :: Text -> [Word]
toAmounts = fromMaybe (error "Invalid input") . traverse (readMaybe . toString) . words

day01Part1 :: Text -> Text
day01Part1 s =
  let amts = toAmounts s
  in case [(amt1, amt2) | amt1 <- amts, amt2 <- amts, amt1 + amt2 == 2020] of
       (amt1, amt2):_ -> show $ amt1 * amt2
       _              -> error "No matching amounts"

day01Part2 :: Text -> Text
day01Part2 s =
  let amts = toAmounts s
  in case [(amt1, amt2, amt3) | amt1 <- amts
                              , amt2 <- amts
                              , amt3 <- amts
                              , amt1 + amt2 + amt3 == 2020] of
       (amt1, amt2, amt3):_ -> show $ amt1 * amt2 * amt3
       _                    -> error "No matching amounts"
