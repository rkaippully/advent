module Main where

import Data.Text.IO (interact)
import Day01
import Day02
import Day03
import Day04
import Relude
import System.Environment (getArgs)


main :: IO ()
main = getArgs >>= interact . \case
  ["01", "1"] -> day01Part1
  ["01", "2"] -> day01Part2
  ["02", "1"] -> day02Part1
  ["02", "2"] -> day02Part2
  ["03", "1"] -> day03Part1
  ["03", "2"] -> day03Part2
  ["04", "1"] -> day04Part1
  ["04", "2"] -> day04Part2
  _ -> error "Unknown day and part"
