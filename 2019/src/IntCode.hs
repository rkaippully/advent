-- | The IntCode computer
module IntCode (
  Computer,
  Input,
  Output,
  newComputer,
  runComputer,
  evalComputer,
  execComputer,
) where

import Data.List.Extra (splitOn)
import Data.Map.Strict (Map, (!))
import Data.Map.Strict qualified as Map

data Computer = Computer
  { memory :: Memory
  , pc :: Addr
  }

type Memory = Map Addr Word

newtype Addr = Addr Word
  deriving newtype (Show, Eq, Ord, Enum, Num)

type Input = Word
type Output = Word

newComputer :: String -> Computer
newComputer s = Computer{memory = initMemory, pc = 0}
  where
    initMemory = Map.fromList $ zip [0 ..] $ map read $ splitOn "," s

runComputer :: Computer -> [Input] -> ([Output], Computer)
runComputer = run . step

evalComputer :: Computer -> [Input] -> [Output]
evalComputer c = fst . runComputer c

execComputer :: Computer -> [Input] -> Computer
execComputer c = snd . runComputer c

run :: Step -> [Input] -> ([Output], Computer)
run st inputs =
  case st of
    NeedInput f ->
      case inputs of
        [] -> error "Not enough input"
        (x : xs) -> run (f x) xs
    HasOutput x nxt -> let (xs, c) = run nxt inputs in (x : xs, c)
    Halted c -> ([], c)

data Step
  = NeedInput (Word -> Step)
  | HasOutput Word Step
  | Halted Computer

step :: Computer -> Step
step comp = exec comp (decode comp)

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
  deriving stock (Show, Eq)

data Operand
  = PosOperand Addr
  | ImmOperand Word
  deriving stock (Show, Eq)

decode :: Computer -> Insn
decode Computer{memory, pc} =
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
  where
    (modes, opcode) = (memory ! pc) `divMod` 100
    p0 = memory ! (pc + 1)
    p1 = memory ! (pc + 2)
    p2 = memory ! (pc + 3)

    toOperand :: Word -> Word -> Operand
    toOperand idx v =
      case modeAt idx of
        0 -> PosOperand (Addr v)
        1 -> ImmOperand v
        _ -> error "Invalid parameter mode"

    modeAt :: Word -> Word
    modeAt idx = (modes `div` (10 ^ idx)) `mod` 10

exec :: Computer -> Insn -> Step
exec comp@Computer{memory, pc} = \case
  ADD x y z ->
    step
      Computer
        { memory = Map.insert z (operandVal comp x + operandVal comp y) memory
        , pc = pc + 4
        }
  MUL x y z ->
    step
      Computer
        { memory = Map.insert z (operandVal comp x * operandVal comp y) memory
        , pc = pc + 4
        }
  IN x -> NeedInput \val ->
    step
      Computer
        { memory = Map.insert x val memory
        , pc = pc + 2
        }
  OUT x -> HasOutput (operandVal comp x) (step comp{pc = pc + 2})
  JNZ x y ->
    step
      if operandVal comp x /= 0
        then comp{pc = Addr (operandVal comp y)}
        else comp{pc = pc + 3}
  JZ x y ->
    step
      if operandVal comp x == 0
        then comp{pc = Addr (operandVal comp y)}
        else comp{pc = pc + 3}
  SLT x y z ->
    step
      Computer
        { memory = Map.insert z (if operandVal comp x < operandVal comp y then 1 else 0) memory
        , pc = pc + 4
        }
  SEQ x y z ->
    step
      Computer
        { memory = Map.insert z (if operandVal comp x == operandVal comp y then 1 else 0) memory
        , pc = pc + 4
        }
  HLT -> Halted comp{pc = pc + 1}

operandVal :: Computer -> Operand -> Word
operandVal Computer{memory} = \case
  PosOperand addr -> memory ! addr
  ImmOperand v -> v
