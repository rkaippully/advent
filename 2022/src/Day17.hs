module Day17 (
  day17Part1,
  day17Part2,
) where

import Data.Function ((&))
import Data.Int (Int64)
import Data.List (find, isPrefixOf, tails)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

type Bitmap = Set (Int64, Int64)

data State = State
  { occupied :: Bitmap
  , nextBlocks :: [Bitmap]
  , currBlock :: Bitmap
  , blocksCount :: Int64
  , height :: Int64
  }

type Push = State -> State

allBlocks :: [Bitmap]
allBlocks =
  cycle
    [ -- Minus
      Set.fromList
        [(x, 0) | x <- [2 .. 5]]
    , -- Plus
      Set.fromList
        [(3, 0), (2, 1), (3, 1), (4, 1), (3, 2)]
    , -- L
      Set.fromList
        [(4, 2), (4, 1), (2, 0), (3, 0), (4, 0)]
    , -- I
      Set.fromList
        [(2, y) | y <- [0 .. 3]]
    , -- Box
      Set.fromList
        [(x, y) | x <- [2, 3], y <- [0, 1]]
    ]

initState :: State
initState =
  let
    occupied = Set.fromList [(x, 0) | x <- [0 .. 6]]
   in
    State
      { occupied
      , nextBlocks = tail allBlocks
      , currBlock = appear (head allBlocks) occupied
      , blocksCount = 0
      , height = 0
      }

simulate :: State -> [Push] -> [State]
simulate = scanl oneStep

oneStep :: State -> Push -> State
oneStep state push = doFall $ push state

doFall :: State -> State
doFall State{..} =
  let
    currBlock' = Set.map (\(x, y) -> (x, y - 1)) currBlock
    canFall = Set.null (Set.intersection currBlock' occupied)
   in
    if canFall
      then State{currBlock = currBlock', ..}
      else
        let
          occupied' = Set.union occupied currBlock
         in
          State
            { occupied = occupied'
            , nextBlocks = tail nextBlocks
            , currBlock = appear (head nextBlocks) occupied'
            , blocksCount = blocksCount + 1
            , height = maximum $ Set.map snd occupied'
            }

appear :: Bitmap -> Bitmap -> Bitmap
appear block occupied =
  let
    topY = maximum $ Set.map snd occupied
   in
    Set.map (\(x, y) -> (x, y + topY + 4)) block

doPush :: Int64 -> State -> State
doPush n State{..} =
  let
    currBlock' = Set.map (\(x, y) -> (x + n, y)) currBlock
    canMove =
      Set.null (Set.filter (\(x, _) -> x < 0 || x > 6) currBlock')
        && Set.null (Set.intersection currBlock' occupied)
   in
    State{currBlock = if canMove then currBlock' else currBlock, ..}

parseInput :: String -> [Push]
parseInput = cycle . map f . init
  where
    f = \case
      '<' -> doPush (-1)
      '>' -> doPush 1
      _ -> error "Bad input"

day17Part1 :: String -> String
day17Part1 =
  show . height . fromMaybe (error "wtf?") . find ((== 2022) . blocksCount) . simulate initState . parseInput

findCycleLength :: [Int64] -> Int
findCycleLength heights =
  let
    findIndex :: [Int64] -> [Int64] -> Maybe Int
    findIndex pfx xs = fmap fst $ find (\(_, ys) -> pfx `isPrefixOf` ys) $ zip [1 ..] (tails xs)
   in
    -- search a small prefix
    case findIndex (take 100 heights) (tail heights) of
      Nothing -> error "No cycle found"
      Just len ->
        -- ensure that the cycle is valid
        if take len heights == take len (drop len heights)
          then len
          else error "Invalid cycle"

day17Part2 :: String -> String
day17Part2 s =
  let
    states = simulate initState $ parseInput s
    heightDeltas =
      zip (tail states) states
        & filter (\(st', st) -> blocksCount st' > blocksCount st)
        & map (\(st', st) -> height st' - height st)

    offset = 300
    heightTillOffset = sum $ take offset heightDeltas

    c = drop offset heightDeltas
    cycleLen = findCycleLength c
    cycleHeight = sum $ take cycleLen c
    (numCycles, r) = (1000000000000 - fromIntegral offset :: Int64) `divMod` fromIntegral cycleLen
    remainingHeight = sum (take (fromIntegral r) c)
   in
    show $ heightTillOffset + (numCycles * cycleHeight) + remainingHeight
