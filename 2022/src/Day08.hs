module Day08 (
  day08Part1,
  day08Part2,
)
where

import Data.Char (ord)
import Data.List (foldl')
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map

type Height = Int
type Row = Int
type Col = Int
type Coord = (Int, Int)

parseInput :: String -> Map Coord Height
parseInput = foldl' f Map.empty . zip [0 ..] . map (zip [0 ..]) . lines
  where
    f :: Map Coord Height -> (Row, [(Col, Char)]) -> Map Coord Height
    f m (row, cs) = foldl' (\m' (col, ch) -> Map.insert (row, col) (ord ch - ord '0') m') m cs

type RayLength = Int

{- | Lengths of (top, right, bottom, left) rays. A ray starts at a
 neighbouring tree and follows all the way to the edge as long as the
 trees on the ray are all shorter than the current tree.
-}
rayLengths :: Map Coord Height -> Map Coord (Maybe (RayLength, RayLength, RayLength, RayLength))
rayLengths m = Map.mapWithKey findRays m
  where
    findRays :: Coord -> Height -> Maybe (RayLength, RayLength, RayLength, RayLength)
    findRays (row, col) height =
      (,,,) <$> top row col height <*> right row col height <*> bottom row col height <*> left row col height

    top :: Row -> Col -> Height -> Maybe RayLength
    top row col height
      | row == 0 = Nothing
      | otherwise = Just $ length (takeWhile (< height) [m ! (r, col) | r <- [row - 1, row - 2 .. 0]])

    right :: Row -> Col -> Height -> Maybe RayLength
    right row col height
      | col == maxCol = Nothing
      | otherwise = Just $ length (takeWhile (< height) [m ! (row, c) | c <- [col + 1 .. maxCol]])

    bottom :: Row -> Col -> Height -> Maybe RayLength
    bottom row col height
      | row == maxRow = Nothing
      | otherwise = Just $ length (takeWhile (< height) [m ! (r, col) | r <- [row + 1 .. maxRow]])

    left :: Row -> Col -> Height -> Maybe RayLength
    left row col height
      | col == 0 = Nothing
      | otherwise = Just $ length (takeWhile (< height) [m ! (row, c) | c <- [col - 1, col - 2 .. 0]])

    (maxRow, maxCol) = fst $ Map.findMax m

visibleTrees :: Map Coord (Maybe (RayLength, RayLength, RayLength, RayLength)) -> [Coord]
visibleTrees m = Map.keys $ Map.filterWithKey f m
  where
    f _ Nothing = True
    f (row, col) (Just (top, right, bottom, left)) =
      top == row || left == col || bottom == maxRow - row || right == maxCol - col

    (maxRow, maxCol) = fst $ Map.findMax m

day08Part1 :: String -> String
day08Part1 = show . length . visibleTrees . rayLengths . parseInput

type ScenicScore = Int

scenicScores :: Map Coord (Maybe (RayLength, RayLength, RayLength, RayLength)) -> Map Coord ScenicScore
scenicScores m = Map.mapWithKey f m
  where
    f _ Nothing = 0
    f (row, col) (Just (top, right, bottom, left)) =
      let
        top' = if top == row then top else top + 1
        right' = if right == maxCol - col then right else right + 1
        bottom' = if bottom == maxRow - row then bottom else bottom + 1
        left' = if left == col then left else left + 1
       in
        top' * right' * bottom' * left'

    (maxRow, maxCol) = fst $ Map.findMax m

day08Part2 :: String -> String
day08Part2 = show . maximum . scenicScores . rayLengths . parseInput
