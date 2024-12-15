-- | The IntCode computer
module IntCode (
  Computer (..),
  Input,
  Output,
  newComputer,
  runComputer,
  evalComputer,
  execComputer,
) where

import Data.List.Extra (splitOn, uncons)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Tuple.Extra (snd3, thd3)

data Computer = Computer
  { memory :: Memory
  , pc :: Addr
  , relBase :: Addr
  }

type Memory = Map Addr Int

newtype Addr = Addr Word
  deriving newtype (Show, Eq, Ord, Enum, Real, Integral, Num)

type Input = Int
type Output = Int

newComputer :: String -> Computer
newComputer s = Computer{memory = initMemory, pc = 0, relBase = 0}
  where
    initMemory = Map.fromList $ zip [0 ..] $ map read $ splitOn "," s

evalComputer :: [Input] -> Computer -> [Output]
evalComputer ins = snd3 . runComputer uncons (flip const) ins

execComputer :: [Input] -> Computer -> Computer
execComputer ins = thd3 . runComputer uncons (flip const) ins

runComputer ::
  -- | Provide next input
  (s -> Maybe (Input, s)) ->
  -- | Consume next output
  (Output -> s -> s) ->
  s ->
  Computer ->
  (s, [Output], Computer)
runComputer getInput putOutput s = run getInput putOutput s . step

run ::
  forall s.
  -- | Provide next input
  (s -> Maybe (Input, s)) ->
  -- | Consume next output
  (Output -> s -> s) ->
  s ->
  Step ->
  (s, [Output], Computer)
run getInput putOutput = go
  where
    go :: s -> Step -> (s, [Output], Computer)
    go s = \case
      NeedInput f ->
        case getInput s of
          Nothing -> error "missing input"
          Just (x, s') -> go s' (f x)
      HasOutput x nxt ->
        let (s', xs, c) = go (putOutput x s) nxt
         in (s', x : xs, c)
      Halted c ->
        (s, [], c)

data Step
  = NeedInput (Input -> Step)
  | HasOutput Output Step
  | Halted Computer

step :: Computer -> Step
step comp = exec (decode comp) comp

data Instruction
  = ADD Operand Operand Operand
  | MUL Operand Operand Operand
  | IN Operand
  | OUT Operand
  | JNZ Operand Operand
  | JZ Operand Operand
  | SLT Operand Operand Operand
  | SEQ Operand Operand Operand
  | ARB Operand
  | HLT
  deriving stock (Show, Eq)

data Operand
  = PosOperand Addr
  | ImmOperand Int
  | RelOperand Int
  deriving stock (Show, Eq)

decode :: Computer -> Instruction
decode Computer{memory, pc} =
  case opcode of
    1 -> ADD (toOperand 0 p0) (toOperand 1 p1) (toOperand 2 p2)
    2 -> MUL (toOperand 0 p0) (toOperand 1 p1) (toOperand 2 p2)
    3 -> IN (toOperand 0 p0)
    4 -> OUT (toOperand 0 p0)
    5 -> JNZ (toOperand 0 p0) (toOperand 1 p1)
    6 -> JZ (toOperand 0 p0) (toOperand 1 p1)
    7 -> SLT (toOperand 0 p0) (toOperand 1 p1) (toOperand 2 p2)
    8 -> SEQ (toOperand 0 p0) (toOperand 1 p1) (toOperand 2 p2)
    9 -> ARB (toOperand 0 p0)
    99 -> HLT
    x -> error $ "Invalid opcode: " ++ show x
  where
    modes, opcode :: Word
    (modes, opcode) = (fromIntegral $ memLookup memory pc) `divMod` 100

    p0, p1, p2 :: Int
    p0 = memLookup memory (pc + 1)
    p1 = memLookup memory (pc + 2)
    p2 = memLookup memory (pc + 3)

    toOperand :: Word -> Int -> Operand
    toOperand idx v =
      case modeAt idx of
        0 -> PosOperand (toAddr v)
        1 -> ImmOperand v
        2 -> RelOperand v
        _ -> error "Invalid parameter mode"

    modeAt :: Word -> Word
    modeAt idx = (modes `div` (10 ^ idx)) `mod` 10

memLookup :: Memory -> Addr -> Int
memLookup = flip (Map.findWithDefault 0)

toAddr :: Int -> Addr
toAddr v
  | v < 0 = error "negative address!"
  | otherwise = Addr (fromIntegral v)

exec :: Instruction -> Computer -> Step
exec instruction = execInstruction . incrementPC instruction
  where
    execInstruction :: Computer -> Step
    execInstruction comp@Computer{relBase} =
      case instruction of
        ADD x y z ->
          step $ memSet z (operandVal comp x + operandVal comp y) comp
        MUL x y z ->
          step $ memSet z (operandVal comp x * operandVal comp y) comp
        IN x ->
          NeedInput \val -> step $ memSet x val comp
        OUT x ->
          HasOutput (operandVal comp x) (step comp)
        JNZ x y ->
          step
            if operandVal comp x /= 0
              then comp{pc = addrVal comp y}
              else comp
        JZ x y ->
          step
            if operandVal comp x == 0
              then comp{pc = addrVal comp y}
              else comp
        SLT x y z ->
          step $ memSet z (if operandVal comp x < operandVal comp y then 1 else 0) comp
        SEQ x y z ->
          step $ memSet z (if operandVal comp x == operandVal comp y then 1 else 0) comp
        ARB x ->
          step comp{relBase = toAddr (fromIntegral relBase + operandVal comp x)}
        HLT ->
          Halted comp

operandVal :: Computer -> Operand -> Int
operandVal Computer{memory, relBase} = \case
  PosOperand addr -> memLookup memory addr
  ImmOperand v -> v
  RelOperand o -> memLookup memory $ toAddr $ fromIntegral relBase + o

addrVal :: Computer -> Operand -> Addr
addrVal comp = toAddr . operandVal comp

memSet :: Operand -> Int -> Computer -> Computer
memSet op val comp@Computer{memory, relBase} = comp{memory = Map.insert addr val memory}
  where
    addr = case op of
      PosOperand a -> a
      ImmOperand v -> toAddr v
      RelOperand o -> toAddr $ fromIntegral relBase + o

incrementPC :: Instruction -> Computer -> Computer
incrementPC instruction comp@Computer{pc} = comp{pc = pc + insnLength}
  where
    insnLength =
      case instruction of
        ADD _ _ _ -> 4
        MUL _ _ _ -> 4
        IN _ -> 2
        OUT _ -> 2
        JNZ _ _ -> 3
        JZ _ _ -> 3
        SLT _ _ _ -> 4
        SEQ _ _ _ -> 4
        ARB _ -> 2
        HLT -> 1
