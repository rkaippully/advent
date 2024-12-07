module Day03 (part1, part2) where

import Data.List.Extra (splitOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Tuple.Extra (fst3)

type Path = Map Point [Step]
type Point = (Int, Int)
type Step = Int

part1 :: String -> String
part1 = show . minimum . map manhattan . Map.keys . intersectionPoints . map toPath . lines

toPath :: String -> Path
toPath = fst3 . foldl' toSegment (Map.empty, (0, 0), 0) . splitOn ","
  where
    toSegment :: (Path, Point, Step) -> String -> (Path, Point, Step)
    toSegment _ [] = error "bad input"
    toSegment (path, (x, y), step) (c : rest) =
      let
        len = read @Step rest
       in
        case c of
          'R' -> (path <> Map.fromList [((x + n, y), [step + n]) | n <- [1 .. len]], (x + len, y), step + len)
          'L' -> (path <> Map.fromList [((x - n, y), [step + n]) | n <- [1 .. len]], (x - len, y), step + len)
          'U' -> (path <> Map.fromList [((x, y + n), [step + n]) | n <- [1 .. len]], (x, y + len), step + len)
          'D' -> (path <> Map.fromList [((x, y - n), [step + n]) | n <- [1 .. len]], (x, y - len), step + len)
          _ -> error "bad input"

intersectionPoints :: [Path] -> Map Point [Step]
intersectionPoints = foldl1 (Map.intersectionWith (<>))

manhattan :: Point -> Int
manhattan (x, y) = abs x + abs y

part2 :: String -> String
part2 = show . minimum . map sum . Map.elems . intersectionPoints . map toPath . lines
