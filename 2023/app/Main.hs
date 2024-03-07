module Main where

import Relude
import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import qualified Day07
import qualified Day08
import qualified Day09
import qualified Day10
import qualified Day11
import qualified Day12
import qualified Day13
import qualified Day14
import qualified Day15
import qualified Day16
import qualified Day17
import qualified Day18
import qualified Day19
import qualified Day20
import qualified Day21
import qualified Day22
import qualified Day23
import qualified Day24
import qualified Day25

main :: IO ()
main = do
  args <- getArgs
  case args of
    [day, part] -> solve day part
    _invalid -> putStrLn "Usage: advent2023 <2-digit day> <1-digit part>"

solve :: String -> String -> IO ()
solve day part = do
  s <- readFileLBS $ "data/day" <> day <> ".txt"
  let f = case (day, part) of
        ("01", "1") -> Day01.part1
        ("01", "2") -> Day01.part2
        ("02", "1") -> Day02.part1
        ("02", "2") -> Day02.part2
        ("03", "1") -> Day03.part1
        ("03", "2") -> Day03.part2
        ("04", "1") -> Day04.part1
        ("04", "2") -> Day04.part2
        ("05", "1") -> Day05.part1
        ("05", "2") -> Day05.part2
        ("06", "1") -> Day06.part1
        ("06", "2") -> Day06.part2
        ("07", "1") -> Day07.part1
        ("07", "2") -> Day07.part2
        ("08", "1") -> Day08.part1
        ("08", "2") -> Day08.part2
        ("09", "1") -> Day09.part1
        ("09", "2") -> Day09.part2
        ("10", "1") -> Day10.part1
        ("10", "2") -> Day10.part2
        ("11", "1") -> Day11.part1
        ("11", "2") -> Day11.part2
        ("12", "1") -> Day12.part1
        ("12", "2") -> Day12.part2
        ("13", "1") -> Day13.part1
        ("13", "2") -> Day13.part2
        ("14", "1") -> Day14.part1
        ("14", "2") -> Day14.part2
        ("15", "1") -> Day15.part1
        ("15", "2") -> Day15.part2
        ("16", "1") -> Day16.part1
        ("16", "2") -> Day16.part2
        ("17", "1") -> Day17.part1
        ("17", "2") -> Day17.part2
        ("18", "1") -> Day18.part1
        ("18", "2") -> Day18.part2
        ("19", "1") -> Day19.part1
        ("19", "2") -> Day19.part2
        ("20", "1") -> Day20.part1
        ("20", "2") -> Day20.part2
        ("21", "1") -> Day21.part1
        ("21", "2") -> Day21.part2
        ("22", "1") -> Day22.part1
        ("22", "2") -> Day22.part2
        ("23", "1") -> Day23.part1
        ("23", "2") -> Day23.part2
        ("24", "1") -> Day24.part1
        ("24", "2") -> Day24.part2
        ("25", "1") -> Day25.part1
        ("25", "2") -> Day25.part2
        _invalid -> error "Unknown day and part"
  putStrLn $ f $ decodeUtf8 s
