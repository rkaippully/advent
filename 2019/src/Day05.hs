{-# LANGUAGE TemplateHaskell #-}

module Day05 (part1, part2) where

import Control.Lens (
  Lens',
  at,
  lens,
  makeFieldsNoPrefix,
  (&),
  (+~),
  (.~),
  (<>~),
  (?~),
  (^.),
 )
import Data.List.Extra (intercalate, splitOn)
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq, ViewL (..))
import Data.Sequence qualified as Seq
import GHC.IsList (toList)

data Insn
  = ADD Operand Operand Addr
  | MUL Operand Operand Addr
  | IN Addr
  | OUT Operand
  | JNZ Operand Operand
  | JZ Operand Operand
  | SLT Operand Operand Addr
  | SEQ Operand Operand Addr
  | HLT
  deriving stock (Show, Read, Eq)

data Operand
  = PosOperand Addr
  | ImmOperand Word
  deriving stock (Show, Read, Eq)

newtype Addr = Addr Word
  deriving newtype (Show, Read, Eq, Ord, Enum, Num)

type Mem = Map Addr Word

data Computer = Computer
  { _memory :: Mem
  , _pc :: Addr
  , _input :: Seq Word
  , _output :: Seq Word
  }

makeFieldsNoPrefix ''Computer

atAddr :: Addr -> Lens' Computer Word
atAddr addr = lens getter setter
  where
    getter :: Computer -> Word
    getter comp = fromMaybe 0 $ comp ^. memory . at addr

    setter :: Computer -> Word -> Computer
    setter comp val = comp & memory . at addr ?~ val

atPCOffset :: Addr -> Lens' Computer Word
atPCOffset offset = lens getter setter
  where
    addr :: Computer -> Addr
    addr comp = comp ^. pc + offset

    getter :: Computer -> Word
    getter comp = fromMaybe 0 $ comp ^. memory . at (addr comp)

    setter :: Computer -> Word -> Computer
    setter comp val = comp & memory . at (addr comp) ?~ val

--------------------------------------------------------------------------------

runProgram :: Word -> String -> Computer
runProgram sysId prog =
  loop
    Computer
      { _memory = initMem
      , _pc = 0
      , _input = Seq.singleton sysId
      , _output = Seq.empty
      }
  where
    initMem :: Mem
    initMem = Map.fromList (zip [0 ..] $ map read $ splitOn "," prog)

    loop :: Computer -> Computer
    loop comp =
      let
        insn = decode comp
        comp' = exec comp insn
       in
        if insn == HLT
          then comp'
          else loop comp'

    decode :: Computer -> Insn
    decode comp =
      let
        (modes, opcode) = (comp ^. atPCOffset 0) `divMod` 100
        p0 = comp ^. atPCOffset 1
        p1 = comp ^. atPCOffset 2
        p2 = comp ^. atPCOffset 3

        toOperand :: Word -> Word -> Operand
        toOperand idx v =
          case modeAt idx of
            0 -> PosOperand (Addr v)
            1 -> ImmOperand v
            _ -> error "Invalid parameter mode"

        modeAt :: Word -> Word
        modeAt idx = (modes `div` (10 ^ idx)) `mod` 10
       in
        case opcode of
          1 -> ADD (toOperand 0 p0) (toOperand 1 p1) (Addr p2)
          2 -> MUL (toOperand 0 p0) (toOperand 1 p1) (Addr p2)
          3 -> IN (Addr p0)
          4 -> OUT (toOperand 0 p0)
          5 -> JNZ (toOperand 0 p0) (toOperand 1 p1)
          6 -> JZ (toOperand 0 p0) (toOperand 1 p1)
          7 -> SLT (toOperand 0 p0) (toOperand 1 p1) (Addr p2)
          8 -> SEQ (toOperand 0 p0) (toOperand 1 p1) (Addr p2)
          99 -> HLT
          _ -> error "Invalid instruction"

    exec :: Computer -> Insn -> Computer
    exec comp = \case
      ADD x y z ->
        comp
          & (atAddr z .~ operandVal comp x + operandVal comp y)
          & (pc +~ 4)
      MUL x y z ->
        comp
          & (atAddr z .~ operandVal comp x * operandVal comp y)
          & (pc +~ 4)
      IN x ->
        case Seq.viewl (comp ^. input) of
          EmptyL -> error "empty input"
          (val :< vals) ->
            comp
              & (atAddr x .~ val)
              & (input .~ vals)
              & (pc +~ 2)
      OUT x ->
        comp
          & (output <>~ Seq.singleton (operandVal comp x))
          & (pc +~ 2)
      JNZ x y ->
        if operandVal comp x /= 0
          then comp & pc .~ Addr (operandVal comp y)
          else comp & pc +~ 3
      JZ x y ->
        if operandVal comp x == 0
          then comp & pc .~ Addr (operandVal comp y)
          else comp & pc +~ 3
      SLT x y z ->
        comp
          & (atAddr z .~ if operandVal comp x < operandVal comp y then 1 else 0)
          & (pc +~ 4)
      SEQ x y z ->
        comp
          & (atAddr z .~ if operandVal comp x == operandVal comp y then 1 else 0)
          & (pc +~ 4)
      HLT -> comp & (pc +~ 1)

    operandVal :: Computer -> Operand -> Word
    operandVal comp = \case
      PosOperand addr -> comp ^. atAddr addr
      ImmOperand v -> v

part1 :: String -> String
part1 = showOutput . runProgram 1

showOutput :: Computer -> String
showOutput = intercalate "\n" . map show . toList . (^. output)

part2 :: String -> String
part2 = showOutput . runProgram 5
