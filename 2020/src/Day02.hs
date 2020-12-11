module Day02
  ( day02Part1
  , day02Part2
  ) where

import Data.Maybe
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

data Password = Password
                { number1  :: Int
                , number2  :: Int
                , polChar  :: Char
                , password :: String
                }
              deriving (Show)

toPassword :: String -> Password
toPassword s = fromMaybe (error "Could not parse") $ parseMaybe passwordParser s

passwordParser :: Parser Password
passwordParser = do
  s1 <- some digitChar <* char '-'
  s2 <- some digitChar <* char ' '
  c <- letterChar <* string ": "
  pwd <- some letterChar
  pure Password
    { number1  = read s1
    , number2  = read s2
    , polChar  = c
    , password = pwd
    }

countMatch :: Password -> Bool
countMatch p =
  let n = length $ filter (== polChar p) $ password p
  in number1 p <= n && n <= number2 p

positionMatch :: Password -> Bool
positionMatch p =
  let
    pwd = password p
    (c1, c2) = (pwd !! (number1 p - 1), pwd !! (number2 p - 1))
  in
    case (c1 == polChar p, c2 == polChar p) of
      (True, False) -> True
      (False, True) -> True
      _             -> False

day02Part1 :: String -> String
day02Part1 = show . length . filter countMatch . map toPassword . lines

day02Part2 :: String -> String
day02Part2 = show . length . filter positionMatch . map toPassword . lines
