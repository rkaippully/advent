module Day17
  ( day17Part1
  , day17Part2
  ) where

import Data.Function
import Data.Set (Set)
import qualified Data.Set as Set

type Coord3D = (Int, Int, Int)
type Coord4D = (Int, Int, Int, Int)

class Coord a where
  toCoords :: String -> Set a
  neighbours :: a -> Set a

instance Coord Coord3D where
  toCoords :: String -> Set Coord3D
  toCoords = mconcat . zipWith (curry lineToCoords) [0..] . lines
    where
      lineToCoords :: (Int, String) -> Set Coord3D
      lineToCoords (x, s) = Set.fromList $ map (\(y, _) -> (x, y, 0)) $ filter ((== '#') . snd) $ zip [0..] s

  neighbours :: Coord3D -> Set Coord3D
  neighbours (x, y, z) = Set.fromList [(x', y', z') | x' <- [x-1,x,x+1]
                                                    , y' <- [y-1,y,y+1]
                                                    , z' <- [z-1,z,z+1]
                                                    , (x, y, z) /= (x', y', z')]

instance Coord Coord4D where
  toCoords :: String -> Set Coord4D
  toCoords = Set.map (\(x, y, z) -> (x, y, z, 0)) . toCoords

  neighbours :: Coord4D -> Set Coord4D
  neighbours (x, y, z, w) = Set.fromList [(x', y', z', w') | x' <- [x-1,x,x+1]
                                                           , y' <- [y-1,y,y+1]
                                                           , z' <- [z-1,z,z+1]
                                                           , w' <- [w-1,w,w+1]
                                                           , (x, y, z, w) /= (x', y', z', w')]

nextCycle :: (Ord a, Coord a) => Set a -> Set a
nextCycle world = Set.filter (isActive world) possibles
  where
    possibles = Set.unions $ Set.map (Set.insert <*> neighbours) world

isActive :: (Ord a, Coord a) => Set a -> a -> Bool
isActive world c | c `Set.member` world = cnt == 2 || cnt == 3
                 | otherwise = cnt == 3
  where
    cnt = Set.size $ Set.intersection (neighbours c) world

solve :: (Ord a, Coord a) => String -> Set a
solve s = toCoords s
          & iterate nextCycle
          & (!!6)

day17Part1 :: String -> String
day17Part1 = show . Set.size . solve @Coord3D

day17Part2 :: String -> String
day17Part2 = show . Set.size . solve @Coord4D
