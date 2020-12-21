module Day20
  ( day20Part1
  , day20Part2
  ) where

import Data.Foldable
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

type TileId = Integer

data Tile = Tile
  { tileId    :: TileId
  , tileLines :: [String]
  }
  deriving Show

day20Part1 :: String -> String
day20Part1 s = show $ product $ corners $ parseTiles s

parseTiles :: String -> Map TileId Tile
parseTiles = M.fromList . map (\t -> (tileId t, t)) . fromJust . parseMaybe (some tileP)

type Parser = Parsec Void String

tileP :: Parser Tile
tileP = do
  n <- string "Tile " *> number
  ls <- some lineP <* newline
  pure Tile
    { tileId = n
    , tileLines = ls
    }

lineP :: Parser String
lineP = some (char '#' <|> char '.') <* newline

number :: Parser TileId
number = read <$> some digitChar <* string ":\n"

edges :: Tile -> [String]
edges t = [head ls, map last ls, reverse (last ls), reverse (map head ls)]
  where
    ls = tileLines t

flippedEdges :: Tile -> [String]
flippedEdges t = [reverse (head ls), reverse (map last ls), last ls, map head ls]
  where
    ls = tileLines t

corners :: Map TileId Tile -> [TileId]
corners ts = M.keys $ M.filter ((== 2) . countUniqueEdges) ts
  where
    edgesBag :: Map String Int
    edgesBag = foldl' addEdges M.empty ts

    addEdges :: Map String Int -> Tile -> Map String Int
    addEdges m t = let es = edges t ++ flippedEdges t
                   in foldl' (\m' e -> M.insertWith (+) e 1 m') m es

    countUniqueEdges :: Tile -> Int
    countUniqueEdges t = length $ filter ((== 1) . (edgesBag!)) $ edges t

day20Part2 :: String -> String
day20Part2 = undefined
