module Day09 (
  day09Part1,
  day09Part2,
) where

import Data.List (foldl')
import Data.Map ((!))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

data Move = R | U | L | D
  deriving stock (Show, Read)

parseInput :: String -> [Move]
parseInput = concatMap (moveToSteps . words) . lines
  where
    moveToSteps = \case
      ["R", n] -> replicate (read n) R
      ["U", n] -> replicate (read n) U
      ["L", n] -> replicate (read n) L
      ["D", n] -> replicate (read n) D
      _ -> error "Bad input"

type KnotId = Int
type Position = (Int, Int)
type Knots = Map KnotId Position

simulate :: Knots -> [Move] -> Set Position
simulate knots = snd . foldl' moveRope (knots, Set.singleton (0, 0))

moveRope :: (Knots, Set (Int, Int)) -> Move -> (Knots, Set (Int, Int))
moveRope (knots, visited) m =
  let
    tailIdx = Map.size knots - 1
    head' = moveHead (knots ! 0) m
    knots' = foldl' moveKnots (Map.insert 0 head' knots) [1 .. tailIdx]
   in
    (knots', visited <> Set.singleton (knots' ! tailIdx))

moveHead :: Position -> Move -> Position
moveHead (x, y) = \case
  R -> (x + 1, y)
  U -> (x, y + 1)
  L -> (x - 1, y)
  D -> (x, y - 1)

moveKnots :: Map KnotId Position -> KnotId -> Map KnotId Position
moveKnots knots idx = Map.insert idx (x', y') knots
  where
    (prevx, prevy) = knots ! (idx - 1)
    (x, y) = knots ! idx
    (x', y') = moveToPrevKnot (x, y) (prevx, prevy)

moveToPrevKnot :: Position -> Position -> Position
moveToPrevKnot (x, y) (prevx, prevy)
  | isAdjacent (x, y) (prevx, prevy) = (x, y)
  | otherwise =
      let x' = case compare prevx x of
            LT -> x - 1
            GT -> x + 1
            EQ -> x
          y' = case compare prevy y of
            LT -> y - 1
            GT -> y + 1
            EQ -> y
       in (x', y')

isAdjacent :: Position -> Position -> Bool
isAdjacent (x1, y1) (x2, y2) = abs (x1 - x2) <= 1 && abs (y1 - y2) <= 1

day09Part1 :: String -> String
day09Part1 =
  let
    knots = Map.fromList [(0, (0, 0)), (1, (0, 0))]
   in
    show . Set.size . simulate knots . parseInput

day09Part2 :: String -> String
day09Part2 =
  let
    knots = Map.fromList [(i, (0, 0)) | i <- [0 .. 9]]
   in
    show . Set.size . simulate knots . parseInput
