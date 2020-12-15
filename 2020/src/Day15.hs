module Day15
  ( day15Part1
  , day15Part2
  ) where

import Control.Monad
import Control.Monad.ST
import Data.Vector.Unboxed.Mutable (MVector)
import qualified Data.Vector.Unboxed.Mutable as V

type Number = Int
type Turn = Int

input :: Turn -> String -> ST s (Number, Turn, MVector s Turn)
input limit s = do
  let start = zip (read $ "[" <> s <> "]") [1..]

  arr <- V.replicate limit (-1)
  forM_ start $ uncurry (V.write arr)

  let (n, t) = last start
  pure (n, t, arr)

turns :: Turn -> (Number, Turn, MVector s Turn) -> ST s Number
turns limit = go
  where
    go :: (Number, Turn, MVector s Turn) -> ST s Number
    go (num, turn, arr) | turn == limit = pure num
                        | otherwise     = do
                            t <- arr `V.read` num
                            let nextNum = if t == -1 then 0 else turn - t
                            V.write arr num turn
                            go (nextNum, turn+1, arr)

memoryGame :: Turn -> String -> String
memoryGame t s = show $ runST (input t s >>= turns t)

day15Part1 :: String -> String
day15Part1 = memoryGame 2020

day15Part2 :: String -> String
day15Part2 = memoryGame 30000000
