module Day07 (
  day07Part1,
  day07Part2,
) where

import Data.Functor (void, ($>))
import Data.List (foldl', intercalate, sort, tails)
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser a = Parsec Void String a

type Filename = String
type Path = [Filename] -- reverse path - from child to root
type Size = Integer

data Command
  = CDRoot
  | CDParent
  | CDDir Filename
  | ListDir [ListResult]
  deriving stock (Show)

data ListResult
  = ChildDir Filename
  | ChildFile Filename Size
  deriving stock (Show)

command :: Parser Command
command =
  " cd /" $> CDRoot
    <|> " cd .." $> CDParent
    <|> cdDir
    <|> listDir
  where
    cdDir :: Parser Command
    cdDir = CDDir <$> (" cd " *> fileName)

fileName :: Parser Filename
fileName = some $ char '.' <|> lowerChar

listDir :: Parser Command
listDir = do
  void $ " ls" >> eol
  results <- many (listResult <* eol)
  pure $ ListDir results

listResult :: Parser ListResult
listResult = childDir <|> childFile
  where
    childDir = ChildDir <$> ("dir " *> fileName)
    childFile = do
      sz <- read <$> many digitChar <* char ' '
      name <- fileName
      pure $ ChildFile name sz

parseInput :: String -> [Command]
parseInput "" = []
parseInput s =
  case span (/= '$') s of
    ("", rest) -> parseInput (tail rest)
    (s', "") -> [parseCommand s']
    (s', rest) -> parseCommand s' : parseInput (tail rest)
  where
    parseCommand prg = either (error . errorBundlePretty) id (runParser command "" prg)

computeDirSizes :: [Command] -> Map Filename Size
computeDirSizes = fst . foldl' go (Map.singleton "" 0, [])
  where
    go :: (Map Filename Size, Path) -> Command -> (Map Filename Size, Path)
    go (sizesMap, currDir) = \case
      CDRoot -> (sizesMap, [])
      CDParent -> (sizesMap, tail currDir)
      CDDir name -> (sizesMap, name : currDir)
      ListDir results -> (foldl' (addSizes currDir) sizesMap results, currDir)

    addSizes :: Path -> Map Filename Size -> ListResult -> Map Filename Size
    addSizes currDir sizesMap = \case
      ChildDir _ -> sizesMap
      ChildFile _ sz ->
        let
          dirNames = map (intercalate "/") $ tails currDir

          addOneDir :: Map Filename Size -> Filename -> Map Filename Size
          addOneDir sizes nm = Map.alter updateSize nm sizes

          updateSize :: Maybe Size -> Maybe Size
          updateSize = Just . (+ sz) . fromMaybe 0
         in
          foldl' addOneDir sizesMap dirNames

day07Part1 :: String -> String
day07Part1 = show . sum . filter (<= 100000) . Map.elems . computeDirSizes . parseInput

day07Part2 :: String -> String
day07Part2 s =
  let
    sizes = computeDirSizes $ parseInput s
    unused = 70000000 - (sizes ! "")
    need = 30000000 - unused
   in
    show $ head $ filter (>= need) $ sort $ Map.elems sizes
