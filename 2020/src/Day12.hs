module Day12
  ( day12Part1
  , day12Part2
  ) where

import Data.Foldable

data Turn = T90 | T180 | T270

toTurn :: String -> Turn
toTurn "90"  = T90
toTurn "180" = T180
toTurn "270" = T270
toTurn s     = error $ "Invalid direction " <> s

data Move = MNorth Int
          | MSouth Int
          | MEast Int
          | MWest Int
          | MForward Int
          | MLeft Turn
          | MRight Turn

data Dir = North | East | South | West
  deriving (Show, Enum)

toMove :: String -> Move
toMove = \case
  ('N':s) -> MNorth (read s)
  ('S':s) -> MSouth (read s)
  ('E':s) -> MEast (read s)
  ('W':s) -> MWest (read s)
  ('F':s) -> MForward (read s)
  ('L':s) -> MLeft (toTurn s)
  ('R':s) -> MRight (toTurn s)
  m       -> error $ "invalid move " <> m

toMoves :: String -> [Move]
toMoves = map toMove . lines

moveShip1 :: [Move] -> (Int, Int)
moveShip1 = fst . foldl' go ((0, 0), East)
  where
    go :: ((Int, Int), Dir) -> Move -> ((Int, Int), Dir)
    go ((x, y), d) (MNorth n)       = ((x, y+n), d)
    go ((x, y), d) (MSouth n)       = ((x, y-n), d)
    go ((x, y), d) (MEast n)        = ((x+n, y), d)
    go ((x, y), d) (MWest n)        = ((x-n, y), d)
    go ((x, y), North) (MForward n) = ((x, y+n), North)
    go ((x, y), South) (MForward n) = ((x, y-n), South)
    go ((x, y), East) (MForward n)  = ((x+n, y), East)
    go ((x, y), West) (MForward n)  = ((x-n, y), West)
    go ((x, y), d) (MLeft t)        = ((x, y), turnLeft d t)
    go ((x, y), d) (MRight t)       = ((x, y), turnRight d t)

    turnLeft :: Dir -> Turn -> Dir
    turnLeft North T90  = West
    turnLeft d T90      = pred d
    turnLeft North T180 = South
    turnLeft East T180  = West
    turnLeft d T180     = pred $ pred d
    turnLeft West T270  = North
    turnLeft d T270     = succ d

    turnRight :: Dir -> Turn -> Dir
    turnRight West T90   = North
    turnRight d T90      = succ d
    turnRight West T180  = East
    turnRight South T180 = North
    turnRight d T180     = succ $ succ d
    turnRight North T270 = West
    turnRight d T270     = pred d

manhattan :: (Int, Int) -> Int
manhattan (x, y) = abs x + abs y

day12Part1 :: String -> String
day12Part1 = show . manhattan . moveShip1 . toMoves

moveShip2 :: [Move] -> (Int, Int)
moveShip2 = fst . foldl' go ((0, 0), (10, 1))
  where
    go :: ((Int, Int), (Int, Int)) -> Move -> ((Int, Int), (Int, Int))
    go ((sx, sy), (wx, wy)) (MNorth n)   = ((sx, sy), (wx, wy+n))
    go ((sx, sy), (wx, wy)) (MSouth n)   = ((sx, sy), (wx, wy-n))
    go ((sx, sy), (wx, wy)) (MEast n)    = ((sx, sy), (wx+n, wy))
    go ((sx, sy), (wx, wy)) (MWest n)    = ((sx, sy), (wx-n, wy))
    go ((sx, sy), (wx, wy)) (MForward n) = ((sx+n*wx, sy+n*wy), (wx, wy))
    go ((sx, sy), (wx, wy)) (MLeft t)    = ((sx, sy), rotateLeft (wx, wy) t)
    go ((sx, sy), (wx, wy)) (MRight t)   = ((sx, sy), rotateRight (wx, wy) t)

    rotateLeft :: (Int, Int) -> Turn -> (Int, Int)
    rotateLeft (x, y) T90  = (-y, x)
    rotateLeft (x, y) T180 = (-x, -y)
    rotateLeft (x, y) T270 = (y, -x)

    rotateRight :: (Int, Int) -> Turn -> (Int, Int)
    rotateRight (x, y) T90  = (y, -x)
    rotateRight (x, y) T180 = (-x, -y)
    rotateRight (x, y) T270 = (-y, x)

day12Part2 :: String -> String
day12Part2 = show . manhattan . moveShip2 . toMoves
