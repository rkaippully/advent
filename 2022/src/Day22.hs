module Day22 (
  day22Part1,
  day22Part2,
) where

import Control.Monad (foldM, forM_)
import Control.Monad.ST (ST, runST)
import qualified Data.Array.MArray as Array
import Data.Array.ST (STArray)
import Data.Char (isDigit)

type Coord = (Int, Int)

data Tiles s = Tiles
  { tiles :: STArray s Coord Elem
  , start :: Coord
  , rows :: Int
  , cols :: Int
  }

data Elem = Open | Wall | Empty
  deriving stock (Show, Eq)

data Insn = Move Int | TurnLeft | TurnRight
  deriving stock (Show)

data Facing = FRight | FDown | FLeft | FUp
  deriving stock (Show, Enum)

parseInput :: String -> ST s (Tiles s, [Insn])
parseInput s =
  let
    lns = lines s

    elems :: [(Coord, Elem)]
    elems = concat $ zipWith (\row -> zipWith (toElem row) [0 ..]) [0 ..] (init lns)

    mx, start :: Coord
    mx = (maximum (map (fst . fst) elems), maximum (map (snd . fst) elems))
    start = fst $ head $ filter (\(_, e) -> e == Open) elems

    toElem row col = \case
      ' ' -> ((row, col), Empty)
      '.' -> ((row, col), Open)
      '#' -> ((row, col), Wall)
      c -> error $ "Bad input: " ++ show c

    toInsns = \case
      [] -> []
      ('L' : cs) -> TurnLeft : toInsns cs
      ('R' : cs) -> TurnRight : toInsns cs
      cs -> let (ns, cs') = span isDigit cs in Move (read ns) : toInsns cs'
   in
    do
      b <- Array.newArray ((0, 0), mx) Empty
      forM_ elems $ uncurry (Array.writeArray b)

      let board =
            Tiles
              { tiles = b
              , start
              , rows = fst mx + 1
              , cols = snd mx + 1
              }
      pure (board, toInsns (last lns))

simulate ::
  forall s.
  (Int -> Int -> (Coord, Facing) -> (Coord, Facing)) ->
  Tiles s ->
  [Insn] ->
  ST s (Coord, Facing)
simulate nextTile Tiles{..} = foldM go (start, FRight)
  where
    go :: (Coord, Facing) -> Insn -> ST s (Coord, Facing)
    go (coord, f) = \case
      Move n -> move coord f n
      TurnLeft -> pure (coord, toEnum ((fromEnum f - 1) `mod` 4))
      TurnRight -> pure (coord, toEnum ((fromEnum f + 1) `mod` 4))

    move :: Coord -> Facing -> Int -> ST s (Coord, Facing)
    move coord facing steps = do
      let ts = iterate (nextTile rows cols) (coord, facing)

          oneStep :: [(Coord, Facing)] -> Int -> ST s (Coord, Facing)
          oneStep (i : _) 0 = pure i
          oneStep (i : j@(jCoord, _) : is) n =
            Array.readArray tiles jCoord >>= \case
              Open -> oneStep (j : is) (n - 1)
              Wall -> pure i
              Empty -> oneStep (i : is) n
          oneStep _ _ = error "Can't happen"

      oneStep ts steps

face :: Coord -> Int
face (row, col) =
  if
      | row < 50 -> col `div` 50
      | row >= 50 && row < 100 -> 3
      | row >= 100 && row < 150 -> (col `div` 50) + 4
      | otherwise -> 6

nextCubeTile :: Int -> Int -> (Coord, Facing) -> (Coord, Facing)
nextCubeTile _ _ (coord@(row, col), facing) =
  case facing of
    FRight ->
      case (face coord, col) of
        (2, 149) -> ((149 - row, 99), FLeft)
        (3, 99) -> ((49, 100 + (row - 50)), FUp)
        (5, 99) -> ((49 - (row - 100), 149), FLeft)
        (6, 49) -> ((149, 50 + (row - 150)), FUp)
        _ -> ((row, col + 1), FRight)
    FDown ->
      case (face coord, row) of
        (2, 49) -> ((50 + (col - 100), 99), FLeft)
        (5, 149) -> ((150 + (col - 50), 49), FLeft)
        (6, 199) -> ((0, 100 + col), FDown)
        _ -> ((row + 1, col), FDown)
    FLeft ->
      case (face coord, col) of
        (1, 50) -> ((149 - row, 0), FRight)
        (3, 50) -> ((100, row - 50), FDown)
        (4, 0) -> ((49 - (row - 100), 50), FRight)
        (6, 0) -> ((0, 50 + (row - 150)), FDown)
        _ -> ((row, col - 1), FLeft)
    FUp ->
      case (face coord, row) of
        (1, 0) -> ((150 + (col - 50), 0), FRight)
        (2, 0) -> ((199, col - 100), FUp)
        (4, 100) -> ((50 + col, 50), FRight)
        _ -> ((row - 1, col), FUp)

next2DTile :: Int -> Int -> (Coord, Facing) -> (Coord, Facing)
next2DTile rows cols ((row, col), facing) =
  case facing of
    FRight -> ((row, (col + 1) `mod` cols), facing)
    FDown -> (((row + 1) `mod` rows, col), facing)
    FLeft -> ((row, (col - 1) `mod` cols), facing)
    FUp -> (((row - 1) `mod` rows, col), facing)

solve ::
  (Int -> Int -> (Coord, Facing) -> (Coord, Facing)) ->
  String ->
  String
solve nextTile s = runST $ do
  (tiles, insns) <- parseInput s
  ((row, col), facing) <- simulate nextTile tiles insns
  pure $ show $ (row + 1) * 1000 + (col + 1) * 4 + fromEnum facing

day22Part1 :: String -> String
day22Part1 = solve next2DTile

day22Part2 :: String -> String
day22Part2 = solve nextCubeTile
