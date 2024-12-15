module Day07 (part1, part2) where

import Data.List (permutations)
import IntCode (
  evalComputer,
  newComputer,
 )

type Phase = Int
type Signal = Int

part1 :: String -> String
part1 = show . findLargestSignal 0 4
  where
    findLargestSignal :: Phase -> Phase -> String -> Signal
    findLargestSignal from to s = maximum do
      [p0, p1, p2, p3, p4] <- permutations [from .. to]
      let out0 = evalComputer [p0, 0] (newComputer s)
          out1 = evalComputer (p1 : out0) (newComputer s)
          out2 = evalComputer (p2 : out1) (newComputer s)
          out3 = evalComputer (p3 : out2) (newComputer s)
      pure $ last $ evalComputer (p4 : out3) (newComputer s)

part2 :: String -> String
part2 = show . findLargestSignal 5 9
  where
    findLargestSignal :: Phase -> Phase -> String -> Signal
    findLargestSignal from to s = maximum do
      [p0, p1, p2, p3, p4] <- permutations [from .. to]
      let out0 = evalComputer (p0 : 0 : out4) (newComputer s)
          out1 = evalComputer (p1 : out0) (newComputer s)
          out2 = evalComputer (p2 : out1) (newComputer s)
          out3 = evalComputer (p3 : out2) (newComputer s)
          out4 = evalComputer (p4 : out3) (newComputer s)
      pure $ last out4
