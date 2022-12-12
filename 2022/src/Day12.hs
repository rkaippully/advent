module Day12 (
  day12Part1,
  day12Part2,
) where

import Control.Monad (guard)
import Data.Char (ord)
import Data.List (foldl')
import Data.Map.Strict (Map, (!), (!?))
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq

type Coord = (Int, Int)
type Grid a = Map Coord a

data Input = Input
  { start :: Coord
  , end :: Coord
  , grid :: Grid Char
  }
  deriving stock (Show)

parseInput :: String -> Input
parseInput = foldl' doLine (Input undefined undefined Map.empty) . zip [0 ..] . lines
  where
    doLine :: Input -> (Int, String) -> Input
    doLine input (y, ln) = foldl' (doChar y) input $ zip [0 ..] ln

    doChar :: Int -> Input -> (Int, Char) -> Input
    doChar y input = \case
      (x, 'S') -> input{start = (x, y), grid = Map.insert (x, y) 'a' (grid input)}
      (x, 'E') -> input{end = (x, y), grid = Map.insert (x, y) 'z' (grid input)}
      (x, c) -> input{grid = Map.insert (x, y) c (grid input)}

type Distance = Integer

findDistance1 :: Input -> Distance
findDistance1 input@Input{start, end} = bfs start input ! end

findDistance2 :: Input -> Distance
findDistance2 input@Input{end, grid} =
  let
    distances = map (`bfs` input) $ Map.keys $ Map.filter (== 'a') grid
   in
    minimum $ mapMaybe (!? end) distances

bfs :: Coord -> Input -> Grid Distance
bfs start Input{end, grid} = go (Seq.singleton start) (Map.singleton start 0)
  where
    go :: Seq Coord -> Grid Distance -> Grid Distance
    go Seq.Empty visited = visited
    go (curr :<| rest) visited =
      let
        adjacents = filter (not . (`Map.member` visited)) $ neighbours grid curr
        dist' = (visited ! curr) + 1
        visited' = visited `Map.union` Map.fromList [(a, dist') | a <- adjacents]
       in
        if curr == end
          then visited
          else go (rest <> Seq.fromList adjacents) visited'

neighbours :: Grid Char -> Coord -> [Coord]
neighbours grid (x, y) = do
  (x', y') <- [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
  guard $ Map.member (x', y') grid

  let val = grid ! (x, y)
      val' = grid ! (x', y')
  guard $ ord val' <= ord val + 1

  pure (x', y')

day12Part1 :: String -> String
day12Part1 = show . findDistance1 . parseInput

day12Part2 :: String -> String
day12Part2 = show . findDistance2 . parseInput
