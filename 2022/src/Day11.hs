module Day11 (
  day11Part1,
  day11Part2,
) where

import Data.Function ((&))
import Data.IntMap.Strict (IntMap, (!))
import qualified Data.IntMap.Strict as Map
import Data.List (foldl', iterate', sortOn)
import Data.Ord (Down (..))

type MonkeyId = Int
type WorryLevel = Integer

type MonkeyActions = IntMap Action
type MonkeyHoldings = IntMap [WorryLevel]

data Action = Action
  { operation :: WorryLevel -> WorryLevel
  , testDiv :: WorryLevel
  , trueAction :: MonkeyId -> WorryLevel -> MonkeyHoldings -> MonkeyHoldings
  , falseAction :: MonkeyId -> WorryLevel -> MonkeyHoldings -> MonkeyHoldings
  }

parseInput :: String -> (MonkeyActions, MonkeyHoldings)
parseInput = mkResult . parse . lines
  where
    parse :: [String] -> [(Action, [WorryLevel])]
    parse [] = []
    parse (_id : start : opr : tst : true : false : "" : rest) =
      let
        throwItem :: MonkeyId -> MonkeyId -> WorryLevel -> MonkeyHoldings -> MonkeyHoldings
        throwItem toMonkeyId fromMonkeyId worry =
          Map.adjust (++ [worry]) toMonkeyId
            . Map.adjust tail fromMonkeyId

        action =
          Action
            { operation =
                case (opr !! 23, drop 25 opr) of
                  ('+', "old") -> (\x -> x + x)
                  ('*', "old") -> (\x -> x * x)
                  ('+', y) -> (\x -> x + read y)
                  ('*', y) -> (\x -> x * read y)
                  x -> error $ "Bad op: " <> show x
            , testDiv = read $ drop 21 tst
            , trueAction =
                let toMonkeyId = read $ drop 29 true
                 in throwItem toMonkeyId
            , falseAction =
                let toMonkeyId = read $ drop 30 false
                 in throwItem toMonkeyId
            }
        worries = read $ "[" ++ drop 18 start ++ "]"
       in
        (action, worries) : parse rest
    parse x = error $ "Bad input: " <> show x

    mkResult :: [(Action, [WorryLevel])] -> (MonkeyActions, MonkeyHoldings)
    mkResult xs =
      let as = map fst xs
          hs = map snd xs
       in (Map.fromList (zip [0 ..] as), Map.fromList (zip [0 ..] hs))

type Inspections = IntMap Integer

simulate ::
  MonkeyActions ->
  (Action -> WorryLevel -> WorryLevel) ->
  (Inspections, MonkeyHoldings) ->
  (Inspections, MonkeyHoldings)
simulate actions updateWorry (inspections, holdings) = Map.foldlWithKey' f (inspections, holdings) actions
  where
    f :: (Inspections, MonkeyHoldings) -> MonkeyId -> Action -> (Inspections, MonkeyHoldings)
    f (ins, hs) monkeyId action = foldl' (g monkeyId action) (ins, hs) (hs ! monkeyId)

    g :: MonkeyId -> Action -> (Inspections, MonkeyHoldings) -> WorryLevel -> (Inspections, MonkeyHoldings)
    g monkeyId action (ins, hs) worry =
      let
        ins' = Map.adjust (+ 1) monkeyId ins
        worry' = updateWorry action worry
        hs' =
          if worry' `mod` testDiv action == 0
            then trueAction action monkeyId worry' hs
            else falseAction action monkeyId worry' hs
       in
        (ins', hs')

simulateNRounds ::
  Int ->
  MonkeyActions ->
  MonkeyHoldings ->
  (Action -> WorryLevel -> WorryLevel) ->
  String
simulateNRounds n actions holdings updateWorry =
  let
    initInspections = Map.fromList [(idx, 0) | idx <- [0 .. (Map.size actions - 1)]]
   in
    iterate' (simulate actions updateWorry) (initInspections, holdings)
      & (!! n)
      & fst
      & Map.elems
      & sortOn Down
      & take 2
      & product
      & show

day11Part1 :: String -> String
day11Part1 s =
  let
    (actions, holdings) = parseInput s

    updateWorry :: Action -> WorryLevel -> WorryLevel
    updateWorry action worry = operation action worry `div` 3
   in
    simulateNRounds 20 actions holdings updateWorry

day11Part2 :: String -> String
day11Part2 s =
  let
    (actions, holdings) = parseInput s

    modVal :: WorryLevel
    modVal = product $ Map.map testDiv actions

    updateWorry :: Action -> WorryLevel -> WorryLevel
    updateWorry action worry = operation action worry `mod` modVal
   in
    simulateNRounds 10000 actions holdings updateWorry
