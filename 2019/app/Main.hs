module Main where

import Day01 qualified
import Day02 qualified
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
        _invalid -> error "Unknown day and part"
  putStrLn $ f s
