module Day11
  ( day11Part1
  , day11Part2
  ) where

import Control.Arrow
import Data.Maybe
import Data.Vector (Vector, (!))
import qualified Data.Vector as V

data Cell = Floor | Empty | Occupied
  deriving (Eq, Ord)

newtype Grid = Grid (Vector (Vector Cell))
  deriving (Eq)

toList :: Grid -> [Cell]
toList (Grid cells) = concat $ V.toList <$> V.toList cells

toGrid :: String -> Grid
toGrid = mkGrid . lines
  where
    mkGrid :: [String] -> Grid
    mkGrid = Grid . V.fromList . map toLine

    toLine :: String -> Vector Cell
    toLine = V.fromList . map toState

    toState :: Char -> Cell
    toState '.' = Floor
    toState 'L' = Empty
    toState '#' = Occupied
    toState _   = undefined

neighbours :: Vector (Vector Cell) -> Int -> Int -> [[Cell]]
neighbours cells row col = [ray (row+r, col+c) (\(r', c') -> (r'+r, c'+c))
                               | r <- [-1, 0, 1]
                               , c <- [-1, 0, 1]
                               , r /= 0 || c /= 0
                               ]
  where
    n = V.length cells
    m = V.length (cells!0)

    ray :: (Int, Int) -> ((Int, Int) -> (Int, Int)) -> [Cell]
    ray (r, c) f | r < 0 || r >= n || c < 0 || c >= m = [Floor]
                 | otherwise                          = cells!r!c : ray (f (r, c)) f

rule1 :: Grid -> Grid
rule1 (Grid cells) = Grid $ V.imap (V.imap . f ) cells
  where
    f :: Int -> Int -> Cell -> Cell
    f _   _   Floor    = Floor
    f row col Empty    = if neighbourCount row col == 0 then Occupied else Empty
    f row col Occupied = if neighbourCount row col >= 4 then Empty else Occupied

    neighbourCount :: Int -> Int -> Int
    neighbourCount row col = length (filter (== Occupied) $ head <$> neighbours cells row col)

solve :: (Grid -> Grid) -> String -> String
solve rule = toGrid
             >>> fixpoint rule
             >>> toList
             >>> filter (== Occupied)
             >>> length
             >>> show

fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f x = if x == f x then x else fixpoint f (f x)

day11Part1 :: String -> String
day11Part1 = solve rule1

rule2 :: Grid -> Grid
rule2 (Grid cells) = Grid $ V.imap (V.imap . f) cells
  where
    f :: Int -> Int -> Cell -> Cell
    f _   _   Floor    = Floor
    f row col Empty    = if neighbourCount row col == 0 then Occupied else Empty
    f row col Occupied = if neighbourCount row col >= 5 then Empty else Occupied

    neighbourCount :: Int -> Int -> Int
    neighbourCount row col = length (filter (== Just Occupied) $ firstNonFloor <$> neighbours cells row col)

    firstNonFloor :: [Cell] -> Maybe Cell
    firstNonFloor = listToMaybe . dropWhile (== Floor)

day11Part2 :: String -> String
day11Part2 = solve rule2
