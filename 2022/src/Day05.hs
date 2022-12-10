module Day05 (
  day05Part1,
  day05Part2,
) where

import Control.Arrow ((&&&))
import Control.Monad.State.Class (gets, modify)
import Control.Monad.State.Strict (State, execState)
import Data.Function ((&))
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as Map
import Data.List (transpose)
import Data.Maybe (catMaybes)
import Data.Sequence (Seq ((:<|)))
import qualified Data.Sequence as Seq
import Data.Void (Void)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char

type Stacks = IntMap (Seq Char)

type StackIdx = Int

data Move = Move Int StackIdx StackIdx

type Parser a = Parsec Void String a

inputParser :: Parser (Stacks, [Move])
inputParser = do
  entries <- (parseStackLine <* eol) `manyTill` numberLine
  let stacks =
        transpose entries
          & map (Seq.fromList . catMaybes)
          & zip [1 ..]
          & Map.fromAscList
  moves <- (parseMove <* eol) `manyTill` eof
  pure (stacks, moves)
  where
    numberLine = many (digitChar <|> char ' ') >> eol >> eol

parseStackLine :: Parser [Maybe Char]
parseStackLine =
  some $
    choice
      [ (Just <$> between (char '[') (char ']') asciiChar) <* optional (char ' ')
      , count 3 (char ' ') >> optional (char ' ') >> pure Nothing
      ]

parseMove :: Parser Move
parseMove = do
  idx <- "move " *> many digitChar
  from <- " from " *> many digitChar
  to <- " to " *> many digitChar
  pure $ Move (read idx) (read from) (read to)

makeMoves :: (Seq Char -> Seq Char) -> Stacks -> [Move] -> String
makeMoves f stacks moves =
  let
    program :: State Stacks ()
    program = mapM_ (move f) moves
    stacks' = execState program stacks

    seqHead (ch :<| _) = ch
    seqHead _ = error "Bad sequence"
   in
    map seqHead $ Map.elems stacks'

move :: (Seq Char -> Seq Char) -> Move -> State Stacks ()
move f (Move n from to) =
  gets (Map.lookup from &&& Map.lookup to) >>= \case
    (Just fromStack, Just toStack) -> do
      let (xs, fromStack') = Seq.splitAt n fromStack
      let toStack' = f xs <> toStack
      modify (Map.insert to toStack' . Map.insert from fromStack')
    _ -> error ("Invalid index: " <> show from <> ", " <> show to)

day05Part1 :: String -> String
day05Part1 s =
  let (stacks, moves) =
        either (error . errorBundlePretty) id $
          runParser inputParser "" s
   in makeMoves Seq.reverse stacks moves

day05Part2 :: String -> String
day05Part2 s =
  let (stacks, moves) =
        either (error . errorBundlePretty) id $
          runParser inputParser "" s
   in makeMoves id stacks moves
