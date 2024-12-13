module Day10 (part1, part2) where

import Data.Foldable (maximumBy)
import Data.List (unfoldr)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ord (comparing)
import Data.Set (Set)
import Data.Set qualified as Set

part1 :: String -> String
part1 = show . maximum . map (Map.size . snd) . toAngles . toMap

part2 :: String -> String
part2 s = show $ (\(x, y) -> x * 100 + y) $ (!! 199) $ allSweeps otherPoints
  where
    otherPoints :: Map Angle (Set PointWithDistance)
    otherPoints = snd $ maximumBy (comparing $ Map.size . snd) $ toAngles $ toMap s

    vaporizeOne :: Set PointWithDistance -> ([Point], Set PointWithDistance)
    vaporizeOne points
      | Set.null points = ([], points)
      | otherwise = let (PointWithDistance p _, rest) = Set.deleteFindMin points in ([p], rest)

    oneSweep :: Map Angle (Set PointWithDistance) -> ([Point], Map Angle (Set PointWithDistance))
    oneSweep = traverse vaporizeOne

    allSweeps :: Map Angle (Set PointWithDistance) -> [Point]
    allSweeps = concat . unfoldr f
      where
        f :: Map Angle (Set PointWithDistance) -> Maybe ([Point], Map Angle (Set PointWithDistance))
        f points = case oneSweep points of
          ([], _) -> Nothing
          (xs, rest) -> Just (xs, rest)

type AsteroidMap = [Point]

type Point = (Double, Double)

data PointWithDistance = PointWithDistance Point Double
  deriving stock (Show)

instance Eq PointWithDistance where
  (==) :: PointWithDistance -> PointWithDistance -> Bool
  PointWithDistance _ d1 == PointWithDistance _ d2 = d1 == d2

instance Ord PointWithDistance where
  compare :: PointWithDistance -> PointWithDistance -> Ordering
  compare (PointWithDistance _ d1) (PointWithDistance _ d2) = compare d1 d2

toMap :: String -> AsteroidMap
toMap s = concatMap f $ zip [0 ..] (lines s)
  where
    f :: (Double, String) -> AsteroidMap
    f (row, line) = [(col, row) | (col, ch) <- zip [0 ..] line, ch == '#']

type Angle = Double

toAngles :: AsteroidMap -> [(Point, Map Angle (Set PointWithDistance))]
toAngles points = [(p1, anglesFrom p1 points) | p1 <- points]

anglesFrom :: Point -> [Point] -> Map Angle (Set PointWithDistance)
anglesFrom p1@(x1, y1) points =
  Map.unionsWith
    (<>)
    [ Map.singleton angle' (Set.singleton $ PointWithDistance p2 distance)
    | p2@(x2, y2) <- points
    , p1 /= p2
    , -- 0 <= angle'< 2*pi
    let angle = atan2 (x2 - x1) (y1 - y2)
        angle' = if angle < 0 then angle + (2 * pi) else angle
        distance = (x2 - x1) ^ (2 :: Int) + (y2 - y1) ^ (2 :: Int)
    ]
