module Day03 (
    part1,
    part2,
) where

import AocLib (readFail, to2dArray)
import Data.Array.IArray (assocs, (!))
import Data.Array.Unboxed (UArray, bounds)
import Data.Char (isDigit)
import Data.Tuple.Extra (fst3)
import Relude

part1 :: String -> String
part1 = show . sum . map fst3 . partNumbers . toSchematic

part2 :: String -> String
part2 s =
    let schematic = toSchematic s
        ratios = gearRatios (partNumbers schematic) schematic
     in show $ sum ratios

--------------------------------------------------------------------------------

type Schematic = UArray (Int, Int) Char

toSchematic :: String -> Schematic
toSchematic = to2dArray

type Pos = (Int, Int)
type Len = Int
type Number = (Word, Pos, Len)

partNumbers :: Schematic -> [Number]
partNumbers s =
    let ((rowLow, colLow), (rowHigh, colHigh)) = bounds s
     in concatMap (filter (isPartNumber s) . scan s colLow colHigh) [rowLow .. rowHigh]

scan :: Schematic -> Int -> Int -> Int -> [Number]
scan s col colHigh row
    | col > colHigh = []
    | otherwise =
        case takeWhile isDigit [s ! (row, x) | x <- [col .. colHigh]] of
            [] -> scan s (col + 1) colHigh row
            cs ->
                let n = readFail cs
                    l = length cs
                 in (n, (row, col), l) : scan s (col + l) colHigh row

isPartNumber :: Schematic -> Number -> Bool
isPartNumber s (_, (r, c), l) = any isSymbol neighbors
    where
        neighbors =
            [(r - 1, x) | x <- [(c - 1) .. (c + l)]]
                ++ [(r + 1, x) | x <- [(c - 1) .. (c + l)]]
                ++ [(r, c - 1), (r, c + l)]

        isSymbol :: Pos -> Bool
        isSymbol p
            | inBounds p =
                let x = s ! p
                 in x /= '.' && not (isDigit x)
            | otherwise = False

        inBounds (x, y) =
            let ((rowLow, colLow), (rowHigh, colHigh)) = bounds s
             in x >= rowLow && x <= rowHigh && y >= colLow && y <= colHigh

--------------------------------------------------------------------------------

gearRatios :: [Number] -> Schematic -> [Word]
gearRatios nums s =
    map product $ [map fst3 ns | (pos, ch) <- assocs s, ch == '*', let ns = filter (isNeighbor pos) nums, length ns == 2]
    where
        isNeighbor :: (Int, Int) -> Number -> Bool
        isNeighbor (gearRow, gearCol) (_, (numRow, numCol), l) =
            (gearRow == numRow && (numCol + l == gearCol || numCol == gearCol + 1)) -- same row
                || ((gearRow == numRow + 1 || gearRow == numRow - 1) && (gearCol >= numCol - 1 && gearCol <= numCol + l)) -- above or below
