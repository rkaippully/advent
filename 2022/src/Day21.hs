module Day21 (
  day21Part1,
  day21Part2,
) where

import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Text.Printf (printf)

type Var = String

data Exp
  = Lit Double
  | Add Var Var
  | Sub Var Var
  | Mul Var Var
  | Div Var Var
  deriving stock (Show)

parseInput :: String -> Map Var Exp
parseInput = Map.fromList . map toEqn . lines
  where
    toEqn :: String -> (Var, Exp)
    toEqn s = case words s of
      [v, x] -> (init v, Lit (read x))
      [v, x, "+", y] -> (init v, Add x y)
      [v, x, "-", y] -> (init v, Sub x y)
      [v, x, "*", y] -> (init v, Mul x y)
      [v, x, "/", y] -> (init v, Div x y)
      _ -> error s

eval :: Map Var Exp -> Double
eval vars = go "root"
  where
    go :: Var -> Double
    go v = case vars ! v of
      Lit x -> x
      Add x y -> go x + go y
      Sub x y -> go x - go y
      Mul x y -> go x * go y
      Div x y -> go x / go y

findBounds :: Map Var Exp -> (Double, Double)
findBounds vars =
  let
    ns = [0, 1000000000000 ..]

    toLoHi m n =
      let
        vm = eval $ withHumnVal m vars
        vn = eval $ withHumnVal n vars
       in
        if
            | vm <= 0 && vn >= 0 -> Just (m, n)
            | vm >= 0 && vn <= 0 -> Just (n, m)
            | otherwise -> Nothing
   in
    head $ catMaybes $ zipWith toLoHi ns (tail ns)

withHumnVal :: Double -> Map Var Exp -> Map Var Exp
withHumnVal v = Map.insert "humn" (Lit v)

search :: Double -> Double -> Map Var Exp -> Double
search lo hi vars =
  let
    mid = (lo + hi) / 2
    midv = eval $ withHumnVal mid vars
   in
    if
        | midv < 0 -> search mid hi vars
        | midv > 0 -> search lo mid vars
        | otherwise -> mid

day21Part1 :: String -> String
day21Part1 = printf "%.0f" . eval . parseInput

day21Part2 :: String -> String
day21Part2 s =
  let
    vars = adjustRoot $ parseInput s

    adjustRoot vs =
      let
        e = case vs ! "root" of
          Add x y -> Sub x y
          Sub x y -> Sub x y
          Mul x y -> Sub x y
          Div x y -> Sub x y
          Lit _ -> error "root can't be a literal value"
       in
        Map.insert "root" e vs

    (lo, hi) = findBounds vars
   in
    printf "%.0f" $ search lo hi vars
