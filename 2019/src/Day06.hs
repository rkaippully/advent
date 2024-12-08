module Day06 (part1, part2) where

import Data.List.Extra (splitOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

type Object = String
type Level = Word
type Orbits = Map Object Object -- child to parent map

parse :: String -> Orbits
parse = foldl' f Map.empty . lines
  where
    f :: Orbits -> String -> Orbits
    f orbits s =
      case splitOn ")" s of
        [parent, child] -> Map.insert child parent orbits
        _ -> error "bad input"

toParentChain :: Orbits -> Object -> [Object]
toParentChain orbits obj =
  case Map.lookup obj orbits of
    Nothing -> []
    Just parent -> toParentChain orbits parent <> [parent]

toLevels :: Orbits -> [Level]
toLevels orbits = Map.elems m
  where
    m :: Map Object Word
    m = Map.fromSet levelOf (Map.keysSet orbits)

    levelOf :: Object -> Word
    levelOf obj = fromIntegral $ length $ toParentChain orbits obj

part1 :: String -> String
part1 = show . sum . toLevels . parse

part2 :: String -> String
part2 s =
  let
    orbits = parse s
    yourPath = toParentChain orbits "YOU"
    santaPath = toParentChain orbits "SAN"

    dropCommon :: [Object] -> [Object] -> ([Object], [Object])
    dropCommon (a : as) (b : bs)
      | a == b = dropCommon as bs
      | otherwise = (a : as, b : bs)
    dropCommon as bs = (as, bs)

    (p1, p2) = dropCommon yourPath santaPath
   in
    show $ length p1 + length p2
