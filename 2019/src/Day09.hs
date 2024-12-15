module Day09 (part1, part2) where

import IntCode (evalComputer, newComputer)

part1 :: String -> String
part1 s = show $ evalComputer [1] (newComputer s)

part2 :: String -> String
part2 s = show $ evalComputer [2] (newComputer s)
