module Day08
  ( day08Part1
  , day08Part2
  ) where

import           Control.Arrow
import           Data.IntMap.Strict ((!), (!?))
import qualified Data.IntMap.Strict as M
import qualified Data.Set           as S

data Op = Nop Int
        | Jmp Int
        | Acc Int
        deriving (Show)

parseOp :: String -> Op
parseOp s = let [op, arg] = words s
            in case op of
                 "nop" -> Nop (readNum arg)
                 "jmp" -> Jmp (readNum arg)
                 "acc" -> Acc (readNum arg)
  where
    readNum ('+':s) = read s
    readNum s       = read s

makeMap :: [Op] -> M.IntMap Op
makeMap = M.fromList . zip [0..]

execute :: Int -> M.IntMap Op -> (Int, Bool)
execute acc m = go 0 (acc, S.empty)
  where
    go :: Int -> (Int, S.Set Int) -> (Int, Bool)
    go ip (acc, visited) | ip `S.member` visited = (acc, False)
                         | otherwise = case m !? ip of
                                         Nothing -> (acc, True)
                                         Just op -> let (acc', ip') = execute acc ip op
                                                    in go ip' (acc', S.insert ip visited)

    execute :: Int -> Int -> Op -> (Int, Int)
    execute acc ip (Nop _) = (acc, ip+1)
    execute acc ip (Jmp x) = (acc, ip+x)
    execute acc ip (Acc x) = (acc+x, ip+1)

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
