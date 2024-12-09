module Day07 (part1, part2) where

import Control.Arrow ((>>>))
import Data.Function (fix)
import Data.List (foldl1', permutations)
import IntCode (
  Input,
  Output,
  evalComputer,
  newComputer,
 )

type Phase = Word
type Signal = Word

part1 :: String -> String
part1 = show . findLargestSignal head . mkAmpPermutations 0 4

part2 :: String -> String
part2 = show . findLargestSignal last . mkAmpPermutations 5 9

-- A single amplifier
type Amp = [Input] -> [Output]

-- A series of amplifiers connected left to right
type AmpSeries = [Input] -> [Output]

mkAmpPermutations :: Phase -> Phase -> String -> [AmpSeries]
mkAmpPermutations from to s = map (mkAmpSeries s) $ permutations [from .. to]

mkAmpSeries :: String -> [Phase] -> AmpSeries
mkAmpSeries s phases inputs = connectAmps amps (0 : inputs)
  where
    amps = map (mkAmp s) phases

mkAmp :: String -> Phase -> Amp
mkAmp s p inputs = evalComputer (newComputer s) (p : inputs)

-- connect the amplifiers in a series
connectAmps :: [Amp] -> AmpSeries
connectAmps = foldl1' (>>>)

findLargestSignal :: ([Signal] -> Signal) -> [AmpSeries] -> Signal
findLargestSignal f amps = maximum $ map (f . fix) amps
