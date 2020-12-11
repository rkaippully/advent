module Day08
  ( day08Part1
  , day08Part2
  ) where

import Control.Arrow
import Data.IntMap.Strict ((!), (!?))
import qualified Data.IntMap.Strict as M
import qualified Data.Set as S

data Op = Nop Int
        | Jmp Int
        | Acc Int
        deriving (Show)

parseOp :: String -> Op
parseOp s = case words s of
              ["nop", arg] -> Nop (readNum arg)
              ["jmp", arg] -> Jmp (readNum arg)
              ["acc", arg] -> Acc (readNum arg)
              _            -> undefined
  where
    readNum ('+':ns) = read ns
    readNum ns       = read ns

makeMap :: [Op] -> M.IntMap Op
makeMap = M.fromList . zip [0..]

execute :: Int -> M.IntMap Op -> (Int, Bool)
execute accum m = go 0 (accum, S.empty)
  where
    go :: Int -> (Int, S.Set Int) -> (Int, Bool)
    go ip (acc, visited) | ip `S.member` visited = (acc, False)
                         | otherwise = case m !? ip of
                                         Nothing -> (acc, True)
                                         Just op -> let (acc', ip') = step acc ip op
                                                    in go ip' (acc', S.insert ip visited)

    step :: Int -> Int -> Op -> (Int, Int)
    step acc ip (Nop _) = (acc, ip+1)
    step acc ip (Jmp x) = (acc, ip+x)
    step acc ip (Acc x) = (acc+x, ip+1)

day08Part1 :: String -> String
day08Part1 = lines
             >>> map parseOp
             >>> makeMap
             >>> execute 0
             >>> fst
             >>> show

makeMaps :: [Op] -> [M.IntMap Op]
makeMaps = f . makeMap
  where
    f :: M.IntMap Op -> [M.IntMap Op]
    f m = map (g m) (M.keys m)

    g :: M.IntMap Op -> Int -> M.IntMap Op
    g m i = case m ! i of
              Nop x -> M.insert i (Jmp x) m
              Jmp x -> M.insert i (Nop x) m
              _     -> m

hasTerminated :: (Int, Bool) -> Bool
hasTerminated = snd

day08Part2 :: String -> String
day08Part2 = lines
             >>> map parseOp
             >>> makeMaps
             >>> map (execute 0)
             >>> filter hasTerminated
             >>> head
             >>> fst
             >>> show
