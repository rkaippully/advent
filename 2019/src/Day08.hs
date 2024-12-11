module Day08 (part1, part2) where

import Control.Arrow ((&&&))
import Data.Foldable (minimumBy)
import Data.List.Extra (trim)
import Data.Map.Strict (Map, (!))
import Data.Map.Strict qualified as Map
import Data.Ord (comparing)

type Layer = Map Idx Color

type Idx = Word
type Color = Char

part1 :: String -> String
part1 = show . uncurry (*) . (countOf '1' &&& countOf '2') . minimumBy (comparing $ countOf '0') . toLayers . trim

part2 :: String -> String
part2 = display . mergeLayers . toLayers . trim

width, height :: Word
width = 25
height = 6

countOf :: Color -> Layer -> Int
countOf c = length . filter (== c) . Map.elems

toLayers :: String -> [Layer]
toLayers [] = []
toLayers s = layer : toLayers (drop (fromIntegral $ width * height) s)
  where
    layer :: Layer
    layer = Map.fromList [(idx, ch) | (idx, ch) <- zip [0 .. (width * height - 1)] s]

mergeLayers :: [Layer] -> Layer
mergeLayers = Map.unionsWith colorCombine

colorCombine :: Color -> Color -> Color
colorCombine '2' c = c
colorCombine c _ = c

display :: Layer -> String
display layer = unlines [toLine row | row <- [0 .. (height - 1)]]
  where
    toLine :: Word -> String
    toLine row = [toChar (layer ! (row * width + col)) | col <- [0 .. (width - 1)]]

    toChar :: Color -> Char
    toChar '1' = '@'
    toChar _ = ' '
