module Main where

import Day01 qualified
import Day02 qualified
import Day03 qualified
import Day04 qualified
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
        _invalid -> error "Unknown day and part"
  putStrLn $ f s
