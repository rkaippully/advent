module Day02 (
  part1,
  part2,
) where

import Control.Monad.Combinators (sepEndBy)
import Control.Monad.Combinators.NonEmpty (sepBy1)
import Relude hiding (many)
import Text.Megaparsec (Parsec, errorBundlePretty, runParser)
import Text.Megaparsec.Char.Lexer (decimal)

data Game = Game
  { gameId :: !Int
  , gameSets :: !(NonEmpty Cubes)
  }
  deriving stock (Show)

data Cubes = Cubes
  { redCount :: !Word
  , greenCount :: !Word
  , blueCount :: !Word
  }
  deriving stock (Show)

data Color = Red | Blue | Green
  deriving stock (Show, Ord, Eq)

part1 :: String -> String
part1 = show . sum . map gameId . filter isValidGame . parseInput

isValidGame :: Game -> Bool
isValidGame Game{gameSets} = all canTakeCubes gameSets

canTakeCubes :: Cubes -> Bool
canTakeCubes (Cubes r g b) = r <= 12 && g <= 13 && b <= 14

--------------------------------------------------------------------------------

part2 :: String -> String
part2 = show . sum . map (power . gameSets) . parseInput

power :: NonEmpty Cubes -> Word
power cs =
  let Cubes r g b = getColorMax $ sconcat @ColorMax $ coerce cs
   in r * g * b

--------------------------------------------------------------------------------

parseInput :: String -> [Game]
parseInput s = case runParser (game `sepEndBy` "\n") "" s of
  Left e -> error $ toText $ errorBundlePretty e
  Right gs -> gs

type Parser = Parsec Void String

game :: Parser Game
game = do
  gameId <- "Game " *> decimal <* ": "
  gameSets <- cubes `sepBy1` "; "
  pure Game{..}

cubes :: Parser Cubes
cubes = do
  cs <- cube `sepBy1` ", "
  pure $ getColorSum $ sconcat @ColorSum $ coerce cs

cube :: Parser Cubes
cube = do
  n <- decimal <* " "
  c <-
    ("red" $> Red)
      <|> ("green" $> Green)
      <|> ("blue" $> Blue)
  pure $ case c of
    Red -> Cubes n 0 0
    Green -> Cubes 0 n 0
    Blue -> Cubes 0 0 n

newtype ColorSum = ColorSum {getColorSum :: Cubes}

instance Semigroup ColorSum where
  ColorSum (Cubes r1 g1 b1) <> ColorSum (Cubes r2 g2 b2) = ColorSum $ Cubes (r1 + r2) (g1 + g2) (b1 + b2)

newtype ColorMax = ColorMax {getColorMax :: Cubes}

instance Semigroup ColorMax where
  ColorMax (Cubes r1 g1 b1) <> ColorMax (Cubes r2 g2 b2) = ColorMax $ Cubes (max r1 r2) (max g1 g2) (max b1 b2)
