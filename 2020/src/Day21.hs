module Day21
  ( day21Part1
  , day21Part2
  ) where

import Data.Foldable
import Data.Function
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Ord
import Data.Set (Set)
import qualified Data.Set as S
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

type Allergen = String
type Ingredient = String

data Food = Food
  { ingredients :: Set Ingredient
  , allergens   :: Set Allergen
  }
  deriving Show

parseInput :: String -> [Food]
parseInput = fromJust . parseMaybe (many food)

food :: Parser Food
food = do
  ings <- some (some letterChar <* space)
  alls <- string "(contains " *> (some letterChar `sepBy` string ", ") <* char ')' <* newline
  pure $ Food (S.fromList ings) (S.fromList alls)

allergenToIngredients :: [Food] -> Map Allergen (Set Ingredient)
allergenToIngredients = foldl' addAllergens M.empty
  where
    addAllergens :: Map Allergen (Set Ingredient) -> Food -> Map Allergen (Set Ingredient)
    addAllergens m f = foldr (\a -> M.insertWith S.intersection a (ingredients f)) m (allergens f)

allergicIngredients :: [Food] -> Set Ingredient
allergicIngredients = S.unions . M.elems . allergenToIngredients

nonAllergicIngredients :: [Food] -> [Ingredient]
nonAllergicIngredients fs = concatMap (\f -> S.toList $ S.difference (ingredients f) s) fs
  where
    s = allergicIngredients fs

day21Part1 :: String -> String
day21Part1 = show . length . nonAllergicIngredients . parseInput

day21Part2 :: String -> String
day21Part2 s = parseInput s
               & allergenToIngredients
               & refine
               & M.toList
               & sortBy (comparing fst)
               & map (S.elemAt 0 . snd)
               & intercalate ","

refine :: Map Allergen (Set Ingredient) -> Map Allergen (Set Ingredient)
refine m = if m == m' then m else refine m'
  where
    m' = M.map removeSings m

    sings :: Set Ingredient
    sings = S.unions $ M.elems $ M.filter ((== 1) . S.size) m

    removeSings :: Set Ingredient -> Set Ingredient
    removeSings s | S.size s == 1 = s
                  | otherwise     = S.difference s sings
