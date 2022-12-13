module Day13 (
  day13Part1,
  day13Part2,
) where

import Control.Applicative (Alternative (some, (<|>)))
import Data.List (elemIndex, sort)
import Data.List.Split (splitWhen)
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import Text.Megaparsec (Parsec, between, errorBundlePretty, runParser, sepBy)
import Text.Megaparsec.Char (digitChar)

data Item
  = ItemI Int
  | ItemL [Item]
  deriving stock (Show, Eq)

instance Ord Item where
  compare :: Item -> Item -> Ordering
  compare (ItemI x) (ItemI y) = compare x y
  compare (ItemL xs) (ItemL ys) = compare xs ys
  compare i@(ItemI _) (ItemL l) = compare [i] l
  compare (ItemL l) i@(ItemI _) = compare l [i]

type Parser a = Parsec Void String a

parseInput :: String -> [(Item, Item)]
parseInput = parsePairs . splitWhen (== "") . lines
  where
    parsePairs :: [[String]] -> [(Item, Item)]
    parsePairs [] = []
    parsePairs ([s1, s2] : xs) = (parseString s1, parseString s2) : parsePairs xs
    parsePairs xs = error $ "Non-pairs in input: " <> show xs

    parseString :: String -> Item
    parseString = either (error . errorBundlePretty) id . runParser item ""

    item :: Parser Item
    item =
      ItemI . read <$> some digitChar
        <|> ItemL <$> between "[" "]" (item `sepBy` ",")

day13Part1 :: String -> String
day13Part1 = show . sum . map fst . filter goodPair . zip [1 ..] . parseInput
  where
    goodPair :: (Int, (Item, Item)) -> Bool
    goodPair = uncurry (<=) . snd

day13Part2 :: String -> String
day13Part2 s =
  let
    items = sort $ ([divider1, divider2] ++) $ foldMap (\(x, y) -> [x, y]) $ parseInput s

    divider1 = ItemL [ItemL [ItemI 2]]
    divider2 = ItemL [ItemL [ItemI 6]]

    idx1 = fromMaybe (error "divider1 not found") $ elemIndex divider1 items
    idx2 = fromMaybe (error "divider2 not found") $ elemIndex divider2 items
   in
    show ((idx1 + 1) * (idx2 + 1))
