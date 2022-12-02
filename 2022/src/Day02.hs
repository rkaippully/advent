module Day02 (
  day02Part1,
  day02Part2,
) where

data Move = Rock | Paper | Scissors
  deriving stock (Eq, Show)

type Play = (Move, Move)

parse :: ([String] -> Play) -> String -> [Play]
parse mkPlay = map (mkPlay . words) . lines

toPlay1 :: [String] -> Play
toPlay1 = \case
  ["A", "X"] -> (Rock, Rock)
  ["A", "Y"] -> (Rock, Paper)
  ["A", "Z"] -> (Rock, Scissors)
  ["B", "X"] -> (Paper, Rock)
  ["B", "Y"] -> (Paper, Paper)
  ["B", "Z"] -> (Paper, Scissors)
  ["C", "X"] -> (Scissors, Rock)
  ["C", "Y"] -> (Scissors, Paper)
  ["C", "Z"] -> (Scissors, Scissors)
  _ -> error "Invalid input"

toPlay2 :: [String] -> Play
toPlay2 = \case
  ["A", "X"] -> (Rock, Scissors)
  ["A", "Y"] -> (Rock, Rock)
  ["A", "Z"] -> (Rock, Paper)
  ["B", "X"] -> (Paper, Rock)
  ["B", "Y"] -> (Paper, Paper)
  ["B", "Z"] -> (Paper, Scissors)
  ["C", "X"] -> (Scissors, Paper)
  ["C", "Y"] -> (Scissors, Scissors)
  ["C", "Z"] -> (Scissors, Rock)
  _ -> error "Invalid input"

scoreOf :: Play -> Integer
scoreOf (counter, move) = moveScore move + outcomeScore move counter
  where
    moveScore Rock = 1
    moveScore Paper = 2
    moveScore Scissors = 3

    outcomeScore Rock Scissors = 6
    outcomeScore Scissors Paper = 6
    outcomeScore Paper Rock = 6
    outcomeScore x y
      | x == y = 3
      | otherwise = 0

day02Part1 :: String -> String
day02Part1 = show . sum . map scoreOf . parse toPlay1

day02Part2 :: String -> String
day02Part2 = show . sum . map scoreOf . parse toPlay2
