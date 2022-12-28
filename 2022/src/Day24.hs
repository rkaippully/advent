module Day24 (
  day24Part1,
  day24Part2,
) where

import Data.List (foldl')
import Data.Set (Set)
import qualified Data.Set as Set

data State = State
  { lefts :: Set Coord
  , rights :: Set Coord
  , ups :: Set Coord
  , downs :: Set Coord
  , cols :: Int
  , rows :: Int
  , time :: Time
  }
  deriving stock (Show)

type Coord = (Int, Int)
type Time = Int

parseInput :: String -> State
parseInput s = foldl' f initState $ zip [0 ..] lns
  where
    lns = map (init . tail) $ init $ tail $ lines s

    initState =
      State
        { lefts = Set.empty
        , rights = Set.empty
        , ups = Set.empty
        , downs = Set.empty
        , cols = 0
        , rows = 0
        , time = 0
        }

    f :: State -> (Int, String) -> State
    f st (y, cs) = foldl' (g y) st $ zip [0 ..] cs

    g :: Int -> State -> (Int, Char) -> State
    g y State{..} (x, c) =
      let
        cols' = max cols (x + 1)
        rows' = max rows (y + 1)
        st = State{cols = cols', rows = rows', ..}
       in
        case c of
          '.' -> st
          '<' -> st{lefts = Set.insert (x, y) lefts}
          '>' -> st{rights = Set.insert (x, y) rights}
          '^' -> st{ups = Set.insert (x, y) ups}
          'v' -> st{downs = Set.insert (x, y) downs}
          _ -> error "Bad input"

bfs :: Coord -> Coord -> State -> State
bfs startPos endPos x = go (x, Set.singleton startPos)
  where
    go :: (State, Set Coord) -> State
    go (st@State{cols, rows}, locs)
      | endPos `Set.member` locs = st
      | otherwise =
          let
            st' = moveBlizzards st
            nextLocs (curX, curY) =
              Set.fromList
                [ loc
                | loc@(locX, locY) <-
                    [ (curX, curY)
                    , (curX - 1, curY)
                    , (curX + 1, curY)
                    , (curX, curY - 1)
                    , (curX, curY + 1)
                    ]
                , loc == startPos || loc == endPos || ((locX >= 0 && locX < cols) && (locY >= 0 && locY < rows))
                , loc `notElem` (lefts st' <> rights st' <> ups st' <> downs st')
                ]
           in
            go (st', foldMap nextLocs locs)

moveBlizzards :: State -> State
moveBlizzards State{..} = State{lefts = lefts', rights = rights', ups = ups', downs = downs', time = time + 1, ..}
  where
    lefts' = Set.map (\(x, y) -> ((x - 1) `mod` cols, y)) lefts
    rights' = Set.map (\(x, y) -> ((x + 1) `mod` cols, y)) rights
    ups' = Set.map (\(x, y) -> (x, (y - 1) `mod` rows)) ups
    downs' = Set.map (\(x, y) -> (x, (y + 1) `mod` rows)) downs

day24Part1 :: String -> String
day24Part1 s =
  let
    st = parseInput s
    entrance = (0, -1)
    exit = (cols st - 1, rows st)
   in
    show $ time $ bfs entrance exit st

day24Part2 :: String -> String
day24Part2 s =
  let
    st = parseInput s
    entrance = (0, -1)
    exit = (cols st - 1, rows st)
   in
    show $
      time $
        bfs entrance exit $
          bfs exit entrance $
            bfs entrance exit st
