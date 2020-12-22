module Day22
  ( day22Part1
  , day22Part2
  ) where

import Data.Foldable
import Data.Function
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set

type GameState = (Seq Int, Seq Int)

parse :: String -> GameState
parse = (\(xs, ys) -> (Seq.fromList (map read $ tail xs), Seq.fromList (map read $ drop 2 ys))) . span (/= "") . lines

combat :: GameState -> GameState
combat (x:<|xs, y:<|ys) | x > y = combat (xs:|>x:|>y, ys)
                        | otherwise = combat (xs, ys:|>y:|>x)
combat (xs, ys) = (xs, ys)

day22Part1 :: String -> String
day22Part1 s = parse s
               & combat
               & scoreOf
               & show

scoreOf :: GameState -> Int
scoreOf = \case
  (xs, Empty) -> f xs
  (_, ys) -> f ys
  where
    f = sum . zipWith (*) [1..] . reverse . toList

recursiveCombat :: (GameState, Set GameState) -> (GameState, Set GameState)
recursiveCombat (s@(xs, _), prev) | Set.member s prev = ((xs, Seq.empty), prev)
recursiveCombat (s@(x:<|xs, y:<|ys), prev)
  | x <= Seq.length xs && y <= Seq.length ys =
      case recursiveCombat ((Seq.take x xs, Seq.take y ys), Set.empty) of
        ((Empty, _), _) -> recursiveCombat ((xs, ys:|>y:|>x), Set.insert s prev)
        ((_, Empty), _) -> recursiveCombat ((xs:|>x:|>y, ys), Set.insert s prev)
        _               -> error "recursiveCombat returned a non-empty result"
  | x > y =
      recursiveCombat ((xs:|>x:|>y, ys), Set.insert s prev)
  | otherwise =
      recursiveCombat ((xs, ys:|>y:|>x), Set.insert s prev)
recursiveCombat (s, prev) = (s, prev)

day22Part2 :: String -> String
day22Part2 s = parse s
               & \gs -> recursiveCombat (gs, Set.empty)
               & fst
               & scoreOf
               & show
