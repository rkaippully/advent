module Day18 (
  day18Part1,
  day18Part2,
) where

import Control.Arrow ((&&&))
import Data.Sequence (Seq ((:<|)))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Exts (IsList, Item, fromList)

type Cube = (Int, Int, Int)

parseInput :: String -> Set Cube
parseInput = Set.fromList . map parse . lines
  where
    parse s = read $ "(" ++ s ++ ")"

adjacents :: (IsList l, Item l ~ Cube) => Cube -> l
adjacents (x, y, z) = fromList [(x - 1, y, z), (x + 1, y, z), (x, y - 1, z), (x, y + 1, z), (x, y, z - 1), (x, y, z + 1)]

exposedSides :: Set Cube -> Cube -> Int
exposedSides cubes cube = 6 - Set.size (Set.intersection cubes (adjacents cube))

day18Part1 :: String -> String
day18Part1 s =
  let cubes = parseInput s
   in show $ sum $ map (exposedSides cubes) $ Set.toList cubes

floodFill :: Set Cube -> Set Cube
floodFill lavaCubes = go Set.empty (Seq.singleton (0, 0, 0))
  where
    mn, mx :: Int
    (mn, mx) = (subtract 1 . minimum) &&& ((+ 1) . maximum) $ concatMap (\(x, y, z) -> [x, y, z]) lavaCubes

    go :: Set Cube -> Seq Cube -> Set Cube
    go visited Seq.Empty = visited
    go visited (cube@(x, y, z) :<| cubes)
      | x < mn || x > mx || y < mn || y > mx || z < mn || z > mx = go visited cubes
      | Set.member cube visited = go visited cubes
      | Set.member cube lavaCubes = go visited cubes
      | otherwise = go (Set.insert cube visited) (cubes <> adjacents cube)

exteriorSides :: Set Cube -> Cube -> Int
exteriorSides exteriorCubes = Set.size . Set.intersection exteriorCubes . adjacents

day18Part2 :: String -> String
day18Part2 s =
  let
    cubes = parseInput s
    exteriorCubes = floodFill cubes
   in
    show $ sum $ map (exteriorSides exteriorCubes) $ Set.toList cubes
