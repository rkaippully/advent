module Day04
  ( day04Part1
  , day04Part2
  )
  where

import Control.Monad
import Data.Char
import Data.Functor
import Data.Maybe
import Data.Monoid
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

data LaxPassport = LaxPassport
  { lbyr :: Last String
  , liyr :: Last String
  , leyr :: Last String
  , lhgt :: Last String
  , lhcl :: Last String
  , lecl :: Last String
  , lpid :: Last String
  }
  deriving (Show)

instance Semigroup LaxPassport where
  p1 <> p2 = LaxPassport
    { lbyr = lbyr p1 <> lbyr p2
    , liyr = liyr p1 <> liyr p2
    , leyr = leyr p1 <> leyr p2
    , lhgt = lhgt p1 <> lhgt p2
    , lhcl = lhcl p1 <> lhcl p2
    , lecl = lecl p1 <> lecl p2
    , lpid = lpid p1 <> lpid p2
    }

instance Monoid LaxPassport where
  mempty = LaxPassport
    { lbyr = mempty
    , liyr = mempty
    , leyr = mempty
    , lhgt = mempty
    , lhcl = mempty
    , lecl = mempty
    , lpid = mempty
    }

toLaxPassports :: String -> [LaxPassport]
toLaxPassports = fromMaybe (error "parse fail") . parseMaybe laxPassports

laxPassports :: Parser [LaxPassport]
laxPassports = laxPassport `sepBy` newline

laxPassport :: Parser LaxPassport
laxPassport = do
  flds <- some $ laxField <* fieldSeparator
  pure $ mconcat flds

fieldSeparator :: Parser ()
fieldSeparator = (char ' ' <|> newline) $> ()

laxField :: Parser LaxPassport
laxField = do
  k <- some letterChar
  char ':'
  v <- some $ satisfy (not . isSpace)
  case k of
    "byr" -> pure $ mempty { lbyr = Last (Just v) }
    "iyr" -> pure $ mempty { liyr = Last (Just v) }
    "eyr" -> pure $ mempty { leyr = Last (Just v) }
    "hgt" -> pure $ mempty { lhgt = Last (Just v) }
    "hcl" -> pure $ mempty { lhcl = Last (Just v) }
    "ecl" -> pure $ mempty { lecl = Last (Just v) }
    "pid" -> pure $ mempty { lpid = Last (Just v) }
    "cid" -> pure mempty
    _     -> fail $ "Invalid field" <> k

hasRequiredFields :: LaxPassport -> Bool
hasRequiredFields p = getAll $ mconcat $
  All . isJust . getLast <$> [lbyr p, liyr p, leyr p, lhgt p, lhcl p, lecl p, lpid p]

day04Part1 :: String -> String
day04Part1 = show . length . filter hasRequiredFields . toLaxPassports


data StrictPassport = StrictPassport
  { sbyr :: Last Year
  , siyr :: Last Year
  , seyr :: Last Year
  , shgt :: Last Height
  , shcl :: Last HairColor
  , secl :: Last EyeColor
  , spid :: Last PassportId
  }

instance Semigroup StrictPassport where
  p1 <> p2 = StrictPassport
    { sbyr = sbyr p1 <> sbyr p2
    , siyr = siyr p1 <> siyr p2
    , seyr = seyr p1 <> seyr p2
    , shgt = shgt p1 <> shgt p2
    , shcl = shcl p1 <> shcl p2
    , secl = secl p1 <> secl p2
    , spid = spid p1 <> spid p2
    }

instance Monoid StrictPassport where
  mempty = StrictPassport
    { sbyr = mempty
    , siyr = mempty
    , seyr = mempty
    , shgt = mempty
    , shcl = mempty
    , secl = mempty
    , spid = mempty
    }

data Year = Year

data Height = Height

data HairColor = HairColor

data EyeColor = EyeColor

data PassportId = PassportId

toStrictPassports :: String -> [StrictPassport]
toStrictPassports = fromMaybe (error "parse fail") . parseMaybe strictPassports

strictPassports :: Parser [StrictPassport]
strictPassports = strictPassport `sepBy` newline

strictPassport :: Parser StrictPassport
strictPassport = do
  flds <- some $ strictField' <* fieldSeparator
  pure $ mconcat flds
  where
    strictField' :: Parser StrictPassport
    strictField' = try strictField
                   <|> some (satisfy (not . isSpace)) $> mempty

strictField :: Parser StrictPassport
strictField = do
  k <- some letterChar
  char ':'
  p <- case k of
         "byr" -> (\y -> mempty { sbyr = pure y }) <$> year 1920 2002
         "iyr" -> (\y -> mempty { siyr = pure y }) <$> year 2010 2020
         "eyr" -> (\y -> mempty { seyr = pure y }) <$> year 2020 2030
         "hgt" -> (\h -> mempty { shgt = pure h }) <$> height
         "hcl" -> (\c -> mempty { shcl = pure c }) <$> hairColor
         "ecl" -> (\c -> mempty { secl = pure c }) <$> eyeColor
         "pid" -> (\i -> mempty { spid = pure i }) <$> passportId
         "cid" -> pure mempty
         _     -> fail $ "Invalid field: " <> k
  lookAhead fieldSeparator
  pure p

year :: Int -> Int -> Parser Year
year min max = do
  n <- number
  if n >= min && n <= max
    then pure Year
    else fail "Year out of range"

number :: Parser Int
number = read <$> some digitChar

height :: Parser Height
height = do
  n <- number
  cmHeight n <|> inHeight n
  where
    cmHeight n = do
      string "cm"
      if n >= 150 && n <= 193
        then pure Height
        else fail "Height out of range"

    inHeight n = do
        string "in"
        if n >= 59 && n <= 76
          then pure Height
          else fail "Height out of range"

hairColor :: Parser HairColor
hairColor = do
  char '#'
  replicateM_ 6 hexDigitChar
  pure HairColor

eyeColor :: Parser EyeColor
eyeColor = do
  s <- some $ satisfy (not . isSpace)
  case s of
    "amb" -> pure EyeColor
    "blu" -> pure EyeColor
    "brn" -> pure EyeColor
    "gry" -> pure EyeColor
    "grn" -> pure EyeColor
    "hzl" -> pure EyeColor
    "oth" -> pure EyeColor
    _     -> fail "Invalid eye color"

passportId :: Parser PassportId
passportId = replicateM 9 digitChar $> PassportId

isValidPassport :: StrictPassport -> Bool
isValidPassport p = isJust (getLast $ sbyr p)
                    && isJust (getLast $ siyr p)
                    && isJust (getLast $ seyr p)
                    && isJust (getLast $ shgt p)
                    && isJust (getLast $ shcl p)
                    && isJust (getLast $ secl p)
                    && isJust (getLast $ spid p)

day04Part2 :: String -> String
day04Part2 = show . length . filter isValidPassport . toStrictPassports
