module Main where

import Day01 qualified
import Day02 qualified
import Day03 qualified
import Day04 qualified
import Day05 qualified
import Day06 qualified
import Day07 qualified
import Day08 qualified
import Day09 qualified
import Day10 qualified
import Day11 qualified
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [day, part] -> solve day part
    _invalid -> putStrLn "Usage: advent2019 <2-digit day> <1-digit part>"

solve :: String -> String -> IO ()
solve day part = do
  s <- readFile $ "data/day" <> day <> ".txt"
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
        _invalid -> error "Unknown day and part"
  putStrLn $ f s
