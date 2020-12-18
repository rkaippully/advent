module Day16
  ( day16Part1
  , day16Part2
  ) where

import Data.Function
import Data.List
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

type FieldName = String
type FieldVal = Integer

type Ticket = [FieldVal]

data TicketData = TicketData
  { fields       :: Map FieldName (FieldVal -> Bool)
  , myTicket     :: Ticket
  , otherTickets :: [Ticket]
  }

getData :: String -> TicketData
getData = fromJust . parseMaybe ticketData

type Parser = Parsec Void String

ticketData :: Parser TicketData
ticketData = do
  flds <- many (field <* newline) <* newline
  tkt <- string "your ticket:" *> newline *> ticket <* newline
  othTkts <- string "nearby tickets:" *> newline *> many ticket
  pure $ TicketData (Map.fromList flds) tkt othTkts
  where
    ticket = (number `sepBy` char ',') <* newline

number :: Parser FieldVal
number = read <$> some digitChar

field :: Parser (FieldName, FieldVal -> Bool)
field = do
  s <- many (letterChar <|> char ' ') <* string ": "
  n1 <- number <* char '-'
  n2 <- number <* string " or "
  n3 <- number <* char '-'
  n4 <- number
  pure (s, \n -> (n1 <= n && n <= n2) || (n3 <= n && n <= n4))

day16Part1 :: String -> String
day16Part1 s =
  let
    td = getData s
  in
    show $ sum $ filter (not . isValidField td) $ concat $ otherTickets td

isValidField :: TicketData -> FieldVal -> Bool
isValidField td n = any ($ n) rangeChecks
  where
    rangeChecks = Map.elems $ fields td

day16Part2 :: String -> String
day16Part2 s = show $ product [v | (fld, v) <- zip fieldAssignments tkt
                                 , "departure" `isPrefixOf` fld]
  where
    td = getData s
    flds = fields td
    fldNames = Set.fromList $ Map.keys flds
    tkt = myTicket td
    validTkts = filter (all (isValidField td)) $ otherTickets td

    fieldAssignments :: [FieldName]
    fieldAssignments = scanl
                         restrictSets
                         (replicate (length tkt) fldNames)
                         (cycle validTkts)
                       & find (all ((== 1) . Set.size))
                       & fromJust
                       & map (Set.elemAt 0)

    restrictSets :: [Set FieldName] -> [FieldVal] -> [Set FieldName]
    restrictSets fs vs = zip fs vs
                         & map removeOutOfRange
                         & (removeAssigned <*> assigned)

    removeOutOfRange :: (Set FieldName, FieldVal) -> Set FieldName
    removeOutOfRange (fs, v) = Set.filter (\f -> (flds!f) v) fs

    assigned :: [Set FieldName] -> Set FieldName
    assigned = foldl' (\acc fs -> if Set.size fs == 1 then Set.union acc fs else acc) Set.empty

    removeAssigned :: [Set FieldName] -> Set FieldName -> [Set FieldName]
    removeAssigned fs taken = map (\xs -> if Set.size xs == 1 then xs else Set.difference xs taken) fs
