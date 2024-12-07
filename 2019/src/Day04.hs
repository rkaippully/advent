module Day04 (part1, part2) where

import Data.List (group)
import Data.List.Extra (splitOn)

part1 :: String -> String
part1 = show . length . filter isValidPassword1 . toRange

type Digit = Word
type Number = [Digit] -- from right to left

toRange :: String -> [Number]
toRange s = case splitOn "-" s of
  [x, y] ->
    let
      num1, num2 :: Word
      num1 = read x
      num2 = read y

      toNumber :: Word -> Number
      toNumber n = take 6 $ drop 1 $ map snd $ iterate ((`divMod` 10) . fst) (n, 0)
     in
      map toNumber $ takeWhile (<= num2) [num1 .. num2]
  _ -> error "bad input"

isValidPassword1 :: Number -> Bool
isValidPassword1 n = hasAdjacent1 n && isDecreasing n

hasAdjacent1 :: Number -> Bool
hasAdjacent1 = any ((> 1) . length) . group

isDecreasing :: Number -> Bool
isDecreasing n = all (uncurry (>=)) $ zip n (drop 1 n)

part2 :: String -> String
part2 = show . length . filter isValidPassword2 . toRange

isValidPassword2 :: Number -> Bool
isValidPassword2 n = hasAdjacent2 n && isDecreasing n

hasAdjacent2 :: Number -> Bool
hasAdjacent2 = any ((== 2) . length) . group
