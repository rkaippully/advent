module Day02 (part1, part2) where

import Data.List (find)
import Data.Map.Strict ((!))
import Data.Map.Strict qualified as Map
import IntCode (Computer (..), execComputer, newComputer)

part1 :: String -> String
part1 = show . runProgram 12 2

part2 :: String -> String
part2 s = show $ fmap snd $ find ((== 19690720) . fst) [(runProgram noun verb s, 100 * noun + verb) | noun <- [0 .. 99], verb <- [0 .. 99]]

runProgram :: Word -> Word -> String -> Word
runProgram noun verb = (! 0) . memory . flip execComputer [] . setNounVerb noun verb . newComputer

setNounVerb :: Word -> Word -> Computer -> Computer
setNounVerb noun verb comp@Computer{memory} = comp{memory = Map.fromList [(1, noun), (2, verb)] <> memory}
