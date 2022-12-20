module Day20 (
  day20Part1,
  day20Part2,
) where

import Data.Int (Int64)
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.Vector (Vector, (!))
import qualified Data.Vector as Vector

type Idx = Int64
type Val = Int64
type Key = Int64

parseInput :: String -> Vector (Idx, Val)
parseInput = Vector.fromList . zipWith (\i n -> (i, read n)) [0 ..] . lines

mixOne :: Vector (Idx, Val) -> Idx -> Vector (Idx, Val)
mixOne v idx =
  let
    size = fromIntegral $ Vector.length v - 1
    currIdx = fromMaybe (error $ "not found: " <> show idx <> " in " <> show v) $ Vector.findIndex ((== idx) . fst) v

    (front, back) = Vector.splitAt currIdx v
    val = snd $ Vector.head back

    newIdx = case (fromIntegral currIdx + val) `mod` size of
      x
        | x <= 0 -> x + size
        | otherwise -> x
    (front', back') = Vector.splitAt (fromIntegral newIdx) (front <> Vector.tail back)
   in
    front' <> Vector.singleton (idx, val) <> back'

mix :: Key -> Int -> Vector (Idx, Val) -> Vector (Idx, Val)
mix key times v =
  let
    size = fromIntegral $ Vector.length v - 1
    v' = Vector.map (\(idx, val) -> (idx, val * key)) v
   in
    foldl' mixOne v' (concat $ replicate times [0 .. size])

findSum :: Vector (Idx, Val) -> Val
findSum v =
  let
    size = Vector.length v
    zeroIdx = fromMaybe (error $ "zero not found in " <> show v) $ Vector.findIndex ((== 0) . snd) v

    get offset = v ! ((zeroIdx + offset) `mod` size)
   in
    sum $ map (snd . get) [1000, 2000, 3000]

day20Part1 :: String -> String
day20Part1 = show . findSum . mix 1 1 . parseInput

day20Part2 :: String -> String
day20Part2 = show . findSum . mix 811589153 10 . parseInput
