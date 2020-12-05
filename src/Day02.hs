module Day02
  ( day02Part1
  , day02Part2
  ) where

import Relude
import Text.Megaparsec hiding (some)
import Text.Megaparsec.Char

type Parser = Parsec Void Text

data Password = Password
                { number1  :: Int
                , number2  :: Int
                , polChar  :: Char
                , password :: String
                }
              deriving (Show)

toPassword :: Text -> Password
toPassword s = fromMaybe (error "Could not parse") $ parseMaybe passwordParser s

passwordParser :: Parser Password
passwordParser = do
  s1 <- some digitChar
  char '-'
  s2 <- some digitChar
  char ' '
  c <- letterChar
  string ": "
  pwd <- some letterChar
  case (readMaybe s1, readMaybe s2) of
    (Just n1, Just n2) ->
      pure Password
           { number1  = n1
           , number2  = n2
           , polChar  = c
           , password = pwd
           }
    _ -> fail "Parse error"

countMatch :: Password -> Bool
countMatch p =
  let n = length $ filter (== polChar p) $ password p
  in number1 p <= n && n <= number2 p

positionMatch :: Password -> Bool
positionMatch p =
  let
    pwd = password p
    (c1, c2) = (pwd !!? (number1 p - 1), pwd !!? (number2 p - 1))
  in
    case (c1 == Just (polChar p), c2 == Just (polChar p)) of
      (True, False) -> True
      (False, True) -> True
      _             -> False

day02Part1 :: Text -> Text
day02Part1 = show . length . filter countMatch . map toPassword . lines

day02Part2 :: Text -> Text
day02Part2 = show . length . filter positionMatch . map toPassword . lines
