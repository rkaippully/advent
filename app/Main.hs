module Main where

import Data.Text.IO (interact)
import Day01
import Day02
import Day03
import Day04
import Day05
--import Day06
--import Day07
--import Day08
--import Day09
--import Day10
--import Day11
--import Day12
--import Day13
--import Day14
--import Day15
--import Day16
--import Day17
--import Day18
--import Day19
--import Day20
--import Day21
--import Day22
--import Day23
--import Day24
--import Day25
import Relude
import System.Environment (getArgs)


main :: IO ()
main = do
  args <- getArgs
  case args of
    [day, part] -> solve day part
    _           -> error "Usage: advent2020 <2-digit day> <1-digit part>"

solve :: String -> String -> IO ()
solve day part = do
  s <- readFileText $ "data/day" <> day <> ".txt"
  let f = case (day, part) of
            ("01", "1") -> day01Part1
            ("01", "2") -> day01Part2
            ("02", "1") -> day02Part1
            ("02", "2") -> day02Part2
            ("03", "1") -> day03Part1
            ("03", "2") -> day03Part2
            ("04", "1") -> day04Part1
            ("04", "2") -> day04Part2
            ("05", "1") -> day05Part1
            ("05", "2") -> day05Part2
            _           -> error "Unknown day and part"
  putTextLn $ f s
