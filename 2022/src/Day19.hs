module Day19 (
  day19Part1,
  day19Part2,
) where

import Control.Monad (guard)
import Data.Maybe (catMaybes)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set

type Ore = Int
type Clay = Int
type Obsidian = Int

data Blueprint = Blueprint
  { oreRobotCost :: Ore
  , clayRobotCost :: Ore
  , obsidianRobotOreCost :: Ore
  , obsidianRobotClayCost :: Clay
  , geodeRobotOreCost :: Ore
  , geodeRobotObsidianCost :: Obsidian
  }
  deriving stock (Show)

parseInput :: String -> [Blueprint]
parseInput = map parse . lines
  where
    parse :: String -> Blueprint
    parse s =
      let
        ws = words s
       in
        Blueprint
          { oreRobotCost = read (ws !! 6)
          , clayRobotCost = read (ws !! 12)
          , obsidianRobotOreCost = read (ws !! 18)
          , obsidianRobotClayCost = read (ws !! 21)
          , geodeRobotOreCost = read (ws !! 27)
          , geodeRobotObsidianCost = read (ws !! 30)
          }

data State = State
  { timeRemaining :: Int
  , robots :: (Int, Int, Int, Int)
  , resources :: (Int, Int, Int, Int)
  }
  deriving stock (Show, Eq, Ord)

initState :: State
initState = State 0 (1, 0, 0, 0) (0, 0, 0, 0)

type Cache = Set State

bfs :: Blueprint -> State -> Int
bfs Blueprint{..} s = fst $ go (Seq.singleton s) (0, Set.empty)
  where
    -- Maximum amount of ore we can use to build any robot in one minute
    maxOre :: Int
    maxOre = maximum [oreRobotCost, clayRobotCost, obsidianRobotOreCost, geodeRobotOreCost]

    go :: Seq State -> (Int, Cache) -> (Int, Cache)
    go Seq.Empty (best, seen) = (best, seen)
    go (State{timeRemaining, robots = (rOre, rClay, rObsidian, rGeode), resources = (resOre, resClay, resObsidian, resGeode)} :<| nextStates) (best, seen)
      | timeRemaining == 0 = go nextStates (max best resGeode, seen)
      -- Do not continue this branch if we can't beat the best even if we make a geode robot for the rest of the time
      | resGeode + (rGeode * timeRemaining + max 0 ((timeRemaining - 2) * (timeRemaining - 1) `div` 2)) < best = go nextStates (max best resGeode, seen)
      | otherwise =
          let
            best' = max best resGeode

            -- "kill" robots if we have too many
            rOre' = min maxOre rOre
            rClay' = min obsidianRobotClayCost rClay
            rObsidian' = min geodeRobotObsidianCost rObsidian

            -- "throw away" resources if there are more than we can spend in remaining time
            resOre' = min resOre (timeRemaining * maxOre - rOre' * (timeRemaining - 1))
            resClay' = min resClay (timeRemaining * obsidianRobotClayCost - rClay' * (timeRemaining - 1))
            resObsidian' = min resObsidian (timeRemaining * geodeRobotObsidianCost - rObsidian' * (timeRemaining - 1))

            st' = State{timeRemaining, robots = (rOre', rClay', rObsidian', rGeode), resources = (resOre', resClay', resObsidian', resGeode)}

            buildNothing =
              Just $
                State
                  { timeRemaining = timeRemaining - 1
                  , robots = (rOre', rClay', rObsidian', rGeode)
                  , resources = (resOre' + rOre', resClay' + rClay', resObsidian' + rObsidian', resGeode + rGeode)
                  }

            newOreRobot = do
              guard $ resOre' >= oreRobotCost
              Just $
                State
                  { timeRemaining = timeRemaining - 1
                  , robots = (rOre' + 1, rClay', rObsidian', rGeode)
                  , resources = (resOre' - oreRobotCost + rOre', resClay' + rClay', resObsidian' + rObsidian', resGeode + rGeode)
                  }

            newClayRobot = do
              guard $ resOre' >= clayRobotCost
              Just $
                State
                  { timeRemaining = timeRemaining - 1
                  , robots = (rOre', rClay' + 1, rObsidian', rGeode)
                  , resources = (resOre' - clayRobotCost + rOre', resClay' + rClay', resObsidian' + rObsidian', resGeode + rGeode)
                  }

            newObsidianRobot = do
              guard $ resOre' >= obsidianRobotOreCost && resClay' >= obsidianRobotClayCost
              Just $
                State
                  { timeRemaining = timeRemaining - 1
                  , robots = (rOre', rClay', rObsidian' + 1, rGeode)
                  , resources = (resOre' - obsidianRobotOreCost + rOre', resClay' - obsidianRobotClayCost + rClay', resObsidian' + rObsidian', resGeode + rGeode)
                  }

            newGeodeRobot = do
              guard $ resOre' >= geodeRobotOreCost && resObsidian' >= geodeRobotObsidianCost
              Just $
                State
                  { timeRemaining = timeRemaining - 1
                  , robots = (rOre', rClay', rObsidian', rGeode + 1)
                  , resources = (resOre' - geodeRobotOreCost + rOre', resClay' + rClay', resObsidian' - geodeRobotObsidianCost + rObsidian', resGeode + rGeode)
                  }

            newStates = Seq.fromList $ catMaybes [buildNothing, newOreRobot, newClayRobot, newObsidianRobot, newGeodeRobot]
           in
            if Set.member st' seen
              then go nextStates (best', seen)
              else go (nextStates <> newStates) (best', Set.insert st' seen)

day19Part1 :: String -> String
day19Part1 s =
  let
    bps = parseInput s
    counts = map (`bfs` initState{timeRemaining = 24}) bps
   in
    show $ sum $ zipWith (*) [1 ..] counts

day19Part2 :: String -> String
day19Part2 s =
  let
    bps = take 3 $ parseInput s
    counts = map (`bfs` initState{timeRemaining = 32}) bps
   in
    show $ product counts
