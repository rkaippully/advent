module Day17
  ( day17Part1
  , day17Part2
  ) where

import Data.Function
import Data.Set (Set)
import qualified Data.Set as Set


type Coord3D = (Int, Int, Int)
type Coord4D = (Int, Int, Int, Int)

to3DCoords :: String -> Set Coord3D
to3DCoords = mconcat . map lineToCoords . zip [0..] . lines
  where
    lineToCoords :: (Int, String) -> Set Coord3D
    lineToCoords (x, s) = Set.fromList $ map (\(y, _) -> (x, y, 0)) $ filter ((== '#') . snd) $ zip [0..] s

to4DCoords :: String -> Set Coord4D
to4DCoords = Set.map (\(x, y, z) -> (x, y, z, 0)) . to3DCoords

nextCycle :: Ord a => (a -> Set a) -> Set a -> Set a
nextCycle neighbours world = Set.filter (isActive neighbours world) possibles
  where
    possibles = Set.unions $ Set.map (Set.insert <*> neighbours) world

neighbours4d :: Coord4D -> Set Coord4D
neighbours4d (x, y, z, w) = Set.fromList [(x', y', z', w') | x' <- [x-1,x,x+1]
                                                         , y' <- [y-1,y,y+1]
                                                         , z' <- [z-1,z,z+1]
                                                         , w' <- [w-1,w,w+1]
                                                         , (x, y, z, w) /= (x', y', z', w')]

neighbours3d :: Coord3D -> Set Coord3D
neighbours3d (x, y, z) = Set.fromList [(x', y', z') | x' <- [x-1,x,x+1]
                                                    , y' <- [y-1,y,y+1]
                                                    , z' <- [z-1,z,z+1]
                                                    , (x, y, z) /= (x', y', z')]

isActive :: Ord a => (a -> Set a) -> Set a -> a -> Bool
isActive neighbours world c | c `Set.member` world = cnt == 2 || cnt == 3
                            | otherwise = cnt == 3
  where
    cnt = Set.size $ Set.intersection (neighbours c) world

day17Part1 :: String -> String
day17Part1 s = to3DCoords s
               & iterate (nextCycle neighbours3d)
               & (!!6)
               & Set.size
               & show

day17Part2 :: String -> String
day17Part2 s = to4DCoords s
               & iterate (nextCycle neighbours4d)
               & (!!6)
               & Set.size
               & show
