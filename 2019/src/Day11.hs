module Day11 (part1, part2) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Tuple.Extra (fst3)
import IntCode (Input, Output, newComputer, runComputer)
import Prelude hiding (Either (..))

part1 :: String -> String
part1 = show . Map.size . runRobot Map.empty

part2 :: String -> String
part2 = printPanel . runRobot (Map.singleton (Coord 0 0) White)

type Panel = Map Coord Color

data Coord = Coord Int Int
  deriving stock (Eq, Show)

instance Ord Coord where
  (<=) :: Coord -> Coord -> Bool
  Coord x1 y1 <= Coord x2 y2
    | y1 > y2 = True
    | y1 == y2 = x1 <= x2
    | otherwise = False

data Color = White | Black
  deriving stock (Eq)

type State = (Action, Coord, Dir, Panel)

data Dir = Up | Right | Down | Left
data Action = Paint | Move

runRobot :: Panel -> String -> Panel
runRobot initPanel s =
  let panel :: Panel
      (_, _, _, panel) = fst3 $ runComputer getColor moveRobot (Paint, Coord 0 0, Up, initPanel) $ newComputer s
   in panel

getColor :: State -> Maybe (Input, State)
getColor s@(_, loc, _, panel) =
  case Map.findWithDefault Black loc panel of
    Black -> Just (0, s)
    White -> Just (1, s)

moveRobot :: Output -> State -> State
moveRobot out (action, loc@(Coord x y), dir, panel) =
  case action of
    Paint -> (Move, loc, dir, Map.insert loc color panel)
    Move -> (Paint, loc', dir', panel)
  where
    color = if out == 0 then Black else White

    (loc', dir') =
      case (out, dir) of
        (0, Up) -> (Coord (x - 1) y, Left)
        (0, Right) -> (Coord x (y + 1), Up)
        (0, Down) -> (Coord (x + 1) y, Right)
        (0, Left) -> (Coord x (y - 1), Down)
        (_, Up) -> (Coord (x + 1) y, Right)
        (_, Right) -> (Coord x (y - 1), Down)
        (_, Down) -> (Coord (x - 1) y, Left)
        (_, Left) -> (Coord x (y + 1), Up)

printPanel :: Panel -> String
printPanel panel = unlines [toLine y | y <- reverse [maxY .. minY]]
  where
    -- top left point
    Coord minX minY = fst $ Map.findMin panel
    -- bottom right point
    Coord maxX maxY = fst $ Map.findMax panel

    toLine :: Int -> String
    toLine y = [if color == White then '#' else ' ' | x <- [minX .. maxX], let color = Map.findWithDefault Black (Coord x y) panel]
