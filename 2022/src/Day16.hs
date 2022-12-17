module Day16 (
  day16Part1,
  day16Part2,
) where

import Control.Applicative (Alternative (many, some, (<|>)))
import Control.Monad (forM_)
import qualified Data.Array.ST.Safe as Array
import Data.Array.Unboxed (UArray, (!))
import Data.Bits (setBit, testBit, (.&.))
import Data.List (foldl')
import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import Data.Word (Word64)
import Text.Megaparsec (Parsec, errorBundlePretty, oneOf, runParser, sepBy)
import Text.Megaparsec.Char (digitChar, lowerChar, upperChar)

type Valve = Int
type Flow = Int
type Distance = Int
type Time = Int

data State = State
  { startValveId :: Valve
  , flows :: Map Valve Flow
  -- ^ flow rate of a valve, only non-zero flows
  , distances :: UArray (Valve, Valve) Distance
  -- ^ shortest distance between two valves (in number of steps)
  }
  deriving stock (Show)

parseInput :: String -> State
parseInput = mkState . map parseLine . lines
  where
    parseLine :: String -> (String, (Flow, [String]))
    parseLine = either (error . errorBundlePretty) id . runParser valve ""

    valve :: Parsec Void String (String, (Flow, [String]))
    valve = do
      name <- "Valve " *> some upperChar
      rate <- read <$> (" has flow rate=" *> some digitChar)
      dests <- many (lowerChar <|> oneOf @[] "; ") *> some upperChar `sepBy` ", "
      pure (name, (rate, dests))

    mkState :: [(String, (Flow, [String]))] -> State
    mkState xs =
      let
        valveNameToId :: Map String Valve
        valveNameToId = Map.fromList $ zipWith (\idx (name, _) -> (name, idx)) [0 ..] xs

        startValveId = valveNameToId Map.! "AA"
        maxValveId = Map.size valveNameToId - 1

        flows :: Map Valve Flow
        flows =
          Map.fromList $
            filter ((> 0) . snd) $
              zipWith (\idx (_, (flow, _)) -> (idx, flow)) [0 ..] xs

        distances :: UArray (Valve, Valve) Distance
        distances = Array.runSTUArray $ do
          dists <- Array.newArray ((0, 0), (maxValveId, maxValveId)) 1000000

          forM_ xs $ \(u, (_, vs)) ->
            forM_ vs $ \v ->
              Array.writeArray dists (valveNameToId Map.! u, valveNameToId Map.! v) 1

          -- floyd-warshall algorithm to find shortest paths
          forM_ [0 .. maxValveId] $ \i ->
            Array.writeArray dists (i, i) 0

          forM_ [0 .. maxValveId] $ \k ->
            forM_ [0 .. maxValveId] $ \i ->
              forM_ [0 .. maxValveId] $ \j -> do
                ij <- Array.readArray dists (i, j)
                ik <- Array.readArray dists (i, k)
                kj <- Array.readArray dists (k, j)
                Array.writeArray dists (i, j) (min ij (ik + kj))

          pure dists
       in
        State startValveId flows distances

-- A bit set
type OpenValves = Word64

visit :: State -> Valve -> Time -> OpenValves -> Flow -> Map OpenValves Flow -> Map OpenValves Flow
visit state@State{..} valve time opens flow result =
  let
    result' = Map.insert opens (max (fromMaybe 0 $ result !? opens) flow) result
    nextValves =
      [ (nextValve, remainingTime)
      | nextValve <- Map.keys flows
      , not (opens `testBit` nextValve)
      , let remainingTime = time - distances ! (valve, nextValve) - 1
      , remainingTime > 0
      ]

    f res (nextValve, remainingTime) =
      visit state nextValve remainingTime (opens `setBit` nextValve) (flow + remainingTime * (flows Map.! nextValve)) res
   in
    foldl' f result' nextValves

day16Part1 :: String -> String
day16Part1 s =
  let state@State{startValveId} = parseInput s
   in show $ maximum $ visit state startValveId 30 0 0 Map.empty

day16Part2 :: String -> String
day16Part2 s =
  let
    state@State{startValveId} = parseInput s
    result = visit state startValveId 26 0 0 Map.empty
   in
    show $
      maximum
        [ flow1 + flow2
        | (opens1, flow1) <- Map.toList result
        , (opens2, flow2) <- Map.toList result
        , opens1 .&. opens2 == 0
        ]
