module Day15 (
  day15Part1,
  day15Part2,
) where

import Data.Char (isDigit)
import Data.Int (Int64)
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Monoid (All (..))
import Data.Set (Set)
import qualified Data.Set as Set

type Coord = (Int64, Int64)

data Grid = Grid
  { distances :: Map Coord Int64
  -- ^ map from a sensor to its coverage distance
  , beacons :: Set Coord
  -- ^ All beacons
  }

parseInput :: String -> Grid
parseInput = foldl' parseLine (Grid Map.empty Set.empty) . lines
  where
    parseLine :: Grid -> String -> Grid
    parseLine Grid{..} s =
      let
        numChar c = c == '-' || isDigit c

        (sensorX, rest) = span numChar $ drop 12 s
        (sensorY, rest') = span numChar $ drop 4 rest
        (beaconX, rest'') = span numChar $ drop 25 rest'
        (beaconY, _) = span numChar $ drop 4 rest''

        sensor = (read sensorX, read sensorY)
        beacon = (read beaconX, read beaconY)
        distance = manhattan sensor beacon
       in
        Grid (Map.insert sensor distance distances) (Set.insert beacon beacons)

manhattan :: Coord -> Coord -> Int64
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

coverageArea :: Coord -> Int64 -> Set Coord
coverageArea (sensorX, sensorY) distance =
  let
    targetY = 2000000
    deltaY = abs (sensorY - targetY)
   in
    if deltaY >= distance
      then Set.empty
      else Set.fromList [(x, targetY) | x <- [(sensorX - distance + deltaY) .. (sensorX + distance - deltaY)]]

day15Part1 :: String -> String
day15Part1 s =
  let
    Grid{..} = parseInput s
    coverages = Map.foldMapWithKey coverageArea distances
   in
    show $ Set.size $ Set.difference coverages beacons

-- Since there is a unique solution for part 2, it must lie just
-- outside the range of a sensor. If it is further away, there will be
-- another point with the same manhattan distance.
justOutOfRange :: Int64 -> Coord -> Int64 -> Set Coord
justOutOfRange maxScan (sensorX, sensorY) distance =
  let
    d = distance + 1
   in
    Set.fromList
      [ (x, y)
      | x <- [(sensorX - d) .. (sensorX + d)]
      , x >= 0 && x <= maxScan
      , let deltaX = abs (x - sensorX)
      , y <- [sensorY - d + deltaX, sensorY + d - deltaX]
      , y >= 0 && y <= maxScan
      ]

day15Part2 :: String -> String
day15Part2 str =
  let
    Grid{..} = parseInput str

    maxScan = 4000000

    canHazBeacon :: Coord -> Bool
    canHazBeacon pos = getAll $ Map.foldMapWithKey (\sensor distance -> All $ distance < manhattan sensor pos) distances

    unknownSensors :: Set Coord
    unknownSensors = (`Set.difference` beacons) $ Map.foldMapWithKey (\s d -> Set.filter canHazBeacon $ justOutOfRange maxScan s d) distances
   in
    case Set.toList unknownSensors of
      [(x, y)] -> show $ x * 4000000 + y
      a -> error $ "No unique result found: " <> show a
