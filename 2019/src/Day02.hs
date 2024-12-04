module Day02 (
  part1,
  part2,
) where

import Data.Coerce (coerce)
import Data.List.Extra (splitOn)
import Data.Map (Map)
import Data.Map.Strict ((!))
import Data.Map.Strict qualified as Map

data Insn
  = Add Addr Addr Addr
  | Mul Addr Addr Addr
  | Halt
  deriving stock (Show, Read)

newtype Addr = Addr Word
  deriving newtype (Show, Read, Eq, Ord, Enum, Num)

part1 :: String -> String
part1 = show . runProgram 12 2

runProgram :: Word -> Word -> String -> Maybe Word
runProgram noun verb input = Map.lookup 0 $ loop initMem initAddr
  where
    initMem :: Map Addr Word
    initMem =
      Map.fromList [(1, noun), (2, verb)]
        `Map.union` Map.fromList (zip [0 ..] $ map read $ splitOn "," input)

    initAddr :: Addr
    initAddr = 0

    loop :: Map Addr Word -> Addr -> Map Addr Word
    loop mem pc = case decode mem pc of
      Halt -> mem
      insn -> loop (exec mem insn) (pc + 4)

    decode :: Map Addr Word -> Addr -> Insn
    decode mem pc =
      let opcode = Map.findWithDefault 0 pc mem
          x = Map.findWithDefault 0 (pc + 1) mem
          y = Map.findWithDefault 0 (pc + 2) mem
          z = Map.findWithDefault 0 (pc + 3) mem
       in case opcode of
            1 -> Add (coerce x) (coerce y) (coerce z)
            2 -> Mul (coerce x) (coerce y) (coerce z)
            99 -> Halt
            _ -> error "Invalid instruction"

    exec :: Map Addr Word -> Insn -> Map Addr Word
    exec mem = \case
      Add x y z -> Map.insert z ((mem ! x) + (mem ! y)) mem
      Mul x y z -> Map.insert z ((mem ! x) * (mem ! y)) mem
      Halt -> mem

part2 :: String -> String
part2 input =
  show $
    map snd $
      filter ((== Just 19690720) . fst) [(runProgram noun verb input, 100 * noun + verb) | noun <- [0 .. 99], verb <- [0 .. 99]]
