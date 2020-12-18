module Day13
  ( day13Part1
  , day13Part2
  ) where

import Data.Bifunctor
import Data.List
import Data.Maybe
import Data.Ord

parse :: String -> (Integer, [Maybe Integer])
parse s = case lines s of
            [s1, s2] -> (read s1, toBusID <$> splitOn ',' s2)
            _        -> error "bad format"
  where
    toBusID :: String -> Maybe Integer
    toBusID "x" = Nothing
    toBusID n   = Just $ read n

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn x = foldr go []
  where
    go a [] | a == x = []
            | otherwise = [[a]]
    go a (y:ys) | a == x = []:y:ys
                | otherwise = (a:y):ys

day13Part1 :: String -> String
day13Part1 s =
  let
    (start, ids) = parse s
    mults n = (\i -> (n, n * i)) <$> [0..]
  in
    show $ uncurry (*) $ second (\n -> n - start) $ minimumBy (comparing snd) $ head . dropWhile ((< start) . snd) . mults <$> catMaybes ids

day13Part2 :: String -> String
day13Part2 s =
  let
    ids = map (second fromJust) $ filter (isJust . snd) $ zip [0..] $ snd $ parse s

    nums = snd <$> ids
    rems = fst <$> ids
    prod = product nums
    pps = (prod `quot`) <$> nums
    invs = inverse <$> zip pps nums
  in
    show $ -1 * sum (zipWith3 (\x y z -> x*y*z) rems pps invs) `rem` prod

inverse :: (Integer, Integer) -> Integer
inverse (a, b) = case egcd a b of
                   (x, y) | a*x + b*y == 1 -> x
                          | otherwise      -> error "inverse"

egcd :: Integer -> Integer -> (Integer, Integer)
egcd _ 0 = (1, 0)
egcd a b = (t, s - q * t)
  where
    (s, t) = egcd b r
    (q, r) = a `quotRem` b
