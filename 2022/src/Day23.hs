module Day23 (
  day23Part1,
  day23Part2,
) where

import Control.Monad (guard)
import Data.Foldable (asum)
import Data.Ix (range)
import Data.List (scanl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple.Extra (fst3)

type ElfPos = Set Coord

type Coord = (Int, Int)

data Dir = N | S | W | E
  deriving stock (Show)

parseInput :: String -> ElfPos
parseInput = Set.fromList . catMaybes . concat . zipWith (\y -> zipWith (\x c -> if c == '#' then Just (x, y) else Nothing) [0 ..]) [0 ..] . lines

simulate :: ElfPos -> [ElfPos]
simulate ep = scanl' oneRound ep moves
  where
    moves :: [[Dir]]
    moves = cycle [[N, S, W, E], [S, W, E, N], [W, E, N, S], [E, N, S, W]]

oneRound :: ElfPos -> [Dir] -> ElfPos
oneRound ep dirs = Map.foldMapWithKey (\to froms -> if Set.size froms > 1 then froms else Set.singleton to) proposals
  where
    proposals :: Map Coord (Set Coord)
    proposals = Set.foldl' (propose ep dirs) Map.empty ep

propose :: ElfPos -> [Dir] -> Map Coord (Set Coord) -> Coord -> Map Coord (Set Coord)
propose ep dirs m coord@(x, y) =
  let
    neighbours :: Dir -> [Coord]
    neighbours = \case
      N -> [(x - 1, y - 1), (x, y - 1), (x + 1, y - 1)]
      S -> [(x - 1, y + 1), (x, y + 1), (x + 1, y + 1)]
      E -> [(x + 1, y - 1), (x + 1, y), (x + 1, y + 1)]
      W -> [(x - 1, y - 1), (x - 1, y), (x - 1, y + 1)]

    checkDir :: Dir -> Maybe Coord
    checkDir dir = do
      let nbrs = neighbours dir
      guard $ Set.null $ Set.intersection ep (Set.fromList nbrs)
      Just $ nbrs !! 1
   in
    case map checkDir dirs of
      [Just _, Just _, Just _, Just _] -> Map.insert coord Set.empty m -- no neighbours in any direction
      xs -> case asum xs of
        Nothing -> Map.insert coord Set.empty m -- can't move
        Just coord' -> Map.alter (Just . (<> Set.singleton coord) . fromMaybe Set.empty) coord' m

emptyCount :: ElfPos -> Int
emptyCount ep = Set.size $ Set.difference allCoords ep
  where
    mn, mx :: Coord
    mn = (minimum (Set.map fst ep), minimum (Set.map snd ep))
    mx = (maximum (Set.map fst ep), maximum (Set.map snd ep))

    allCoords :: Set Coord
    allCoords = Set.fromList $ range (mn, mx)

day23Part1 :: String -> String
day23Part1 = show . emptyCount . (!! 10) . simulate . parseInput

day23Part2 :: String -> String
day23Part2 s =
  let
    elfPositions = simulate $ parseInput s

    notMoved :: (Int, ElfPos, ElfPos) -> Bool
    notMoved (_, before, after) = before == after
   in
    show $ fst3 $ head $ filter notMoved $ zip3 [1 ..] elfPositions (tail elfPositions)
