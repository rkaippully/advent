module Day14 (
  day14Part1,
  day14Part2,
) where

import Data.List (foldl')
import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Tile = Rock | Sand
  deriving stock (Show, Eq)

type Coord = (Int, Int)
type Grid = Map Coord Tile

parseInput :: String -> Grid
parseInput s = foldMap (\cs -> foldl' populate Map.empty $ zip cs (tail cs)) coords
  where
    coords :: [[Coord]]
    coords = map (map parseSegment . splitOn " -> ") $ lines s

    parseSegment :: String -> Coord
    parseSegment seg = case splitOn "," seg of
      [x, y] -> (read x, read y)
      xs -> error $ "Bad segment: " ++ show xs

    populate :: Grid -> (Coord, Coord) -> Grid
    populate m cs@((x1, y1), (x2, y2))
      | x1 == x2 && y1 <= y2 = m <> Map.fromList [((x1, y), Rock) | y <- [y1 .. y2]]
      | x1 == x2 = m <> Map.fromList [((x1, y), Rock) | y <- [y2 .. y1]]
      | y1 == y2 && x1 <= x2 = m <> Map.fromList [((x, y1), Rock) | x <- [x1 .. x2]]
      | y1 == y2 = m <> Map.fromList [((x, y1), Rock) | x <- [x2 .. x1]]
      | otherwise = error $ "Bad segment: " <> show cs

startCoord :: Coord
startCoord = (500, 0)

dropSand :: Int -> Int -> Grid -> Grid
dropSand thresholdY floorY = dropOneUnit startCoord
  where
    dropOneUnit :: Coord -> Grid -> Grid
    dropOneUnit (x, y) grid
      | y > thresholdY = grid
      | y + 1 == floorY = dropOneUnit startCoord $ Map.insert (x, y) Sand grid
      | Map.notMember (x, y + 1) grid = dropOneUnit (x, y + 1) grid
      | Map.notMember (x - 1, y + 1) grid = dropOneUnit (x - 1, y + 1) grid
      | Map.notMember (x + 1, y + 1) grid = dropOneUnit (x + 1, y + 1) grid
      | (x, y) == startCoord = Map.insert (x, y) Sand grid
      | otherwise = dropOneUnit startCoord $ Map.insert (x, y) Sand grid

sandTrap :: Int -> Int -> Grid -> String
sandTrap thresholdY floorY = show . Map.size . Map.filter (== Sand) . dropSand thresholdY floorY

day14Part1 :: String -> String
day14Part1 s = sandTrap thresholdY floorY grid
  where
    grid = parseInput s
    thresholdY = maximum $ map snd $ Map.keys grid
    floorY = 0

day14Part2 :: String -> String
day14Part2 s = sandTrap thresholdY floorY grid
  where
    grid = parseInput s
    thresholdY = maxBound :: Int
    floorY = maximum (map snd $ Map.keys grid) + 2
