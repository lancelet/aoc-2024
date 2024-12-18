{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module P17 (main, part1, part2, mainEx1, stepInput) where

import Control.Monad (forM_)
import Data.Bits (bitSizeMaybe, shiftL, shiftR, xor, (.&.))
import Data.Function ((&))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (catMaybes)
import Data.Text (Text, intercalate, pack, unpack)
import Data.Text.IO (readFile)
import qualified Data.Vector.Unboxed as VU
import Data.Void (Void)
import Data.Word (Word32, Word8)
import System.Exit (exitFailure)
import Text.Megaparsec (Parsec, eof, errorBundlePretty, optional, parse, sepBy1)
import Text.Megaparsec.Char (char, newline, string)
import Text.Megaparsec.Char.Lexer (decimal)
import Prelude hiding (readFile)

main :: IO ()
main = do
  putStrLn "Day 17"
  -- stepInput
  part1
  part2

part1 :: IO ()
part1 = do
  putStrLn "Part 1"
  let inputFile = "../inputs/input-day-17.txt"
  machine <- loadFile inputFile
  let machine' = runMachineTerminate machine
  putStrLn $ "Machine output: " <> (prettyOutputBuffer . obuf $ machine')

part2 :: IO ()
part2 = do
  putStrLn "Part 2"
  let inputFile = "../inputs/input-day-17.txt"
  machine <- loadFile inputFile
  let pprog = prettyProgram machine
  putStrLn "Decoded program:"
  putStrLn pprog
  let regA = minimum $ findProgProgRegA machine
  putStrLn $ "Register A: " <> show regA
  let machine' = runMachineTerminate (machine {regA = regA})
  putStrLn $ "Machine output: " <> (prettyOutputBuffer . obuf $ machine')

mainEx1 :: IO ()
mainEx1 = do
  putStrLn "Example 1"
  let inputFile = "../inputs/input-day-17-example-1.txt"
  machine <- loadFile inputFile
  let machine' = runMachineTerminate machine
  putStrLn $ "Machine output: " <> (prettyOutputBuffer . obuf $ machine')

stepInput :: IO ()
stepInput = do
  putStrLn "Stepping input"
  let inputFile = "../inputs/input-day-17.txt"
  start_machine <- loadFile inputFile

  putStrLn "Starting machine:"
  putStrLn $ prettyMachine start_machine

  let n_steps = 7 :: Int
  ref_machine <- newIORef start_machine
  forM_ [1 .. n_steps] $ \step -> do
    putStrLn $ "Step: " <> show step
    machine <- readIORef ref_machine
    let maybe_instr = decode machine
    case maybe_instr of
      Nothing -> putStrLn "No instruction."
      Just instr -> do
        putStrLn $ "Instruction: " <> prettyInstr instr
        let machine' = execute instr machine
        putStrLn $ prettyMachine machine'
        writeIORef ref_machine machine'

---- Program Analysis ---------------------------------------------------------

{-
Hand-evaluation of input-day-17.txt

----
Register A: 47719761
Register B: 0
Register C: 0

Program: 2,4,1,5,7,5,0,3,4,1,1,6,5,5,3,0
         ^
Out: _
----
Instruction 2,4 --> bst 4 --> B := A `mod` 8 = 1

Register A: 47719761
Register B: 1
Register C: 0

Program: 2,4,1,5,7,5,0,3,4,1,1,6,5,5,3,0
             ^
Out: _
----
Instruction 1,5 --> bxl 5 --> B := B `xor` 5 = 4

Register A: 47719761
Register B: 4
Register C: 0

Program: 2,4,1,5,7,5,0,3,4,1,1,6,5,5,3,0
                 ^
Out: _
----
Instruction 7,5 --> cdv 5 --> C := A `shiftR` B =

Register A: 47719761
Register B: 4
Register C: 2982485

Program: 2,4,1,5,7,5,0,3,4,1,1,6,5,5,3,0
                     ^
Out: _
----
Instruction 0,3 --> adv 3 --> A := A `shiftR` 3

Register A: 5964970
Register B: 4
Register C: 2982485

Program: 2,4,1,5,7,5,0,3,4,1,1,6,5,5,3,0
                         ^
Out: _
----
Instruction 4,1 --> bxc 1 --> B := B `xor` C = 2982481

Register A: 5964970
Register B: 2982481
Register C: 2982485

Program: 2,4,1,5,7,5,0,3,4,1,1,6,5,5,3,0
                             ^
Out: _
----
Instruction 1,6 --> bxl 6 --> B := B `xor` 6

Register A: 5964970
Register B: 2982487
Register C: 2982485

Program: 2,4,1,5,7,5,0,3,4,1,1,6,5,5,3,0
                                 ^
Out: _
----
Instruction 5,5 --> out (B `mod` 8) = out

Register A: 5964970
Register B: 2982487
Register C: 2982485

Program: 2,4,1,5,7,5,0,3,4,1,1,6,5,5,3,0
                                 ^
Out: 7
-}

{-
My program is (after slight rearrangement):

i0 : bst 4  --   A `mod` 8       -> B    -- B := A and 0b111
i1 : bxl 5  --   B `xor` 5       -> B    -- B := B xor 5
i2 : cdv 5  --   A / (2 ^ B)     -> C    -- C := (A rshift B) & 0b111
i3 : bxc 1  --   B `xor` C       -> B    -- B := B xor C
i4 : bxl 6  --   B `xor` 6       -> B    -- B := B xor 0b110
i5 : out 5  --   output B                -- output (B and 0b111)
i6 : adv 3  --   A / 8           -> A    -- A := A rshift 3
i7 : jnz 0  --   A != 0 ? jmp 0

We work on chunks of 3 bits of A at a time, but there is a dependency for
i2 on up-coming bits within A.

In SSA form:
  b0 = a0 & 0b111
  b1 = b0 xor 5        -- can be 0,1,2,3,4,5,6,7
  c  = (a0 >> b1)      -- only depends on next 8 bits of a0
  b2 = b1 xor c
  b3 = b2 xor 0b110
  output b3
  a1 = a0 >> 3

Considering 4 x 3-bit chunks of A, there are 8*8*8*8 = 4096 combinations to
examine. From those 4096 combinations, a sub-set will be valid ways to produce
the digit. ==> Combine with later digits and find smallest valid set.
-}

-- | Find a list of valid register A values.
findProgProgRegA :: Machine -> [Reg]
findProgProgRegA machine =
  let p = VU.toList $ prog machine
   in chunkNumToInteger <$> matchOutput machine p

-- | Chunknum is an integer stored in chunks of 3 bits at a time.
--
--   The most significant 3-bit chunk is stored first.
newtype ChunkNum = ChunkNum [Word8] deriving (Eq, Show)

instance Semigroup ChunkNum where
  (<>) :: ChunkNum -> ChunkNum -> ChunkNum
  (<>) (ChunkNum xs) (ChunkNum ys) = ChunkNum (xs <> ys)

-- | Convert a ChunkNum to an integer.
chunkNumToInteger :: ChunkNum -> Reg
chunkNumToInteger (ChunkNum chunks) = go 0 chunks
  where
    go accum [] = accum
    go accum (x : xs) = go (accum `shiftL` 3 + fromIntegral x) xs

-- | Match the output of a machine.
--
--   Here, we successively produce all four 3-bit numbers which produce a given
--   output Word8. Then we move on to the next output Word8 and do the same
--   thing. At each stage, there's a reconciliation (ultimately in `spc`),
--   which keeps only the tail ends which are consistent with the front-end of
--   the next output digit.
--
--   (This explanation is not very thorough, but understanding what's going on
--   requires looking at the program analysis above.)
matchOutput ::
  -- | The machine in question.
  Machine ->
  -- | Output to match (the program of the machine).
  [Word8] ->
  -- | List of numbers which will produce the output.
  [ChunkNum]
matchOutput _ [] = []
matchOutput machine (o : os) = go os (findValidABits machine o)
  where
    go :: [Word8] -> [ChunkNum] -> [ChunkNum]
    go [] c = c
    go (y : ys) c =
      let newchunks =
            catMaybes
              [ spc prefix suffix
                | prefix <- findValidABits machine y,
                  suffix <- c
              ]
       in go ys newchunks

    spc :: ChunkNum -> ChunkNum -> Maybe ChunkNum
    spc (ChunkNum [a, b, c, d]) (ChunkNum (e : f : g : zs)) =
      if (b == e) && (c == f) && (d == g)
        then Just (ChunkNum (a : b : c : d : zs))
        else Nothing
    spc _ _ = error "Invalid ChunkNums"

-- | Find combinations of 3-bit chunks of A that result in a given
--   3-bit output.
findValidABits :: Machine -> Word8 -> [ChunkNum]
findValidABits machine out =
  filter
    (\cn -> out == runMyProg machine (chunkNumToInteger cn))
    all4DigitChunkNums

-- | Return the list of all 4x3-bit ChunkNums, ordered from smallest to
--   largest.
all4DigitChunkNums :: [ChunkNum]
all4DigitChunkNums =
  [ ChunkNum [b3, b2, b1, b0]
    | b3 <- [0 .. 7],
      b2 <- [0 .. 7],
      b1 <- [0 .. 7],
      b0 <- [0 .. 7]
  ]

-- | Run a program until it outputs a single value.
runMyProg :: Machine -> Reg -> Word8
runMyProg machine regA = go machine {regA = regA}
  where
    go m =
      case advance m of
        Nothing -> error "Machine terminated"
        Just m' ->
          case obuf m' of
            [] -> go m'
            [x] -> x
            _ -> error "Machine advanced too far"

---- Machine Simulator --------------------------------------------------------

-- | Run the machine to termination and return its comma-separated output
--   buffer.
runMachineTerminate :: Machine -> Machine
runMachineTerminate machine =
  maybe machine runMachineTerminate (advance machine)

-- | Advance executing the machine.
--
-- A return of `Just` indicates that the machine is still simulating, while a
-- return of `Nothing` indicates that the machine is done simulating.
advance :: Machine -> Maybe Machine
advance machine =
  case decode machine of
    Nothing -> Nothing
    Just instr -> Just $ execute instr machine

-- | Decode the next instruction in the machine at the `pcnt` instruction
--   pointer.
--
--   This function returns `Nothing` if the instruction pointer points outside
--   the program memory.
decode :: Machine -> Maybe Instr
decode Machine {prog, pcnt} =
  let ipcnt = fromIntegral pcnt
      g i = prog VU.! i
   in if ipcnt >= VU.length prog - 1
        then Nothing
        else Just $ decode' (g ipcnt) (g (ipcnt + 1))

-- | Decode an opcode.
decodeOpcode :: Bub -> Opcode
decodeOpcode b =
  case b of
    0 -> Adv
    1 -> Bxl
    2 -> Bst
    3 -> Jnz
    4 -> Bxc
    5 -> Out
    6 -> Bdv
    7 -> Cdv
    _ -> error "Invalid program 3-bit value."

-- | Decode two bytes into an instruction
decode' :: Bub -> Bub -> Instr
decode' i = Instr (decodeOpcode i)

-- | Decode a whole program.
decodeProgram :: Machine -> [Instr]
decodeProgram Machine {prog} = go (VU.toList prog)
  where
    go :: [Bub] -> [Instr]
    go [] = []
    go (i : o : xs) = decode' i o : go xs
    go _ = error "Invalid number of program bytes"

-- | Fetch the value of a combo operand.
comboOperandValue :: Machine -> Bub -> Reg
comboOperandValue Machine {regA, regB, regC} b =
  case b of
    0 -> 0
    1 -> 1
    2 -> 2
    3 -> 3
    4 -> regA
    5 -> regB
    6 -> regC
    _ -> error "Invalid combo operator value: 7"

-- | Execute an instruction in the machine.
--
--   The instruction to be executed should be the one decoded from the
--   instruction pointer (`pcnt`), but that is not checked.
execute :: Instr -> Machine -> Machine
execute instr m@Machine {regA, regB, regC} =
  let cv = comboOperandValue m
      av = advPcnt
   in case instr of
        Instr Adv o -> av $ m {regA = regA `trdiv` cv o}
        Instr Bxl o -> av $ m {regB = xor regB (fromIntegral o)}
        Instr Bst o -> av $ m {regB = cv o .&. 7}
        Instr Bxc _ -> av $ m {regB = xor regB regC}
        Instr Out o -> av $ output (cv o) m
        Instr Bdv o -> av $ m {regB = regA `trdiv` cv o}
        Instr Cdv o -> av $ m {regC = regA `trdiv` cv o}
        Instr Jnz _ | regA == 0 -> av m
        Instr Jnz o | otherwise -> m {pcnt = fromIntegral o}

-- | Right shift division.
trdiv :: Reg -> Reg -> Reg
trdiv numerator denominator =
  case bitSizeMaybe numerator of
    Just width ->
      if denominator > width
        then 0
        else numerator `shiftR` denominator
    Nothing ->
      numerator `shiftR` denominator

-- | Advance the program counter of the machine by 2.
advPcnt :: Machine -> Machine
advPcnt machine@Machine {pcnt} = machine {pcnt = pcnt + 2}

-- | Output a byte by prepending to the output buffer.
output :: Reg -> Machine -> Machine
output b machine@Machine {obuf} =
  machine {obuf = fromIntegral (b .&. 7) : obuf}

---- Types --------------------------------------------------------------------

-- | Type of a register in the machine.
type Reg = Int

-- | Type of an instruction "a Bub" in the machine.
type Bub = Word8

-- | Vector of instructions for the program's memory.
type BubVec = VU.Vector Bub

-- | Machine definition.
data Machine = Machine
  { regA :: !Reg,
    regB :: !Reg,
    regC :: !Reg,
    prog :: !BubVec,
    pcnt :: !Word32,
    -- | Output buffer, stored in reverse order.
    obuf :: ![Word8]
  }
  deriving (Eq, Show)

-- | Opcodes
data Opcode = Adv | Bxl | Bst | Jnz | Bxc | Out | Bdv | Cdv deriving (Eq, Show)

-- | Instruction: opcode plus operand.
data Instr = Instr !Opcode !Bub

-- | Pretty-print the machine.
prettyMachine :: Machine -> String
prettyMachine Machine {regA, regB, regC, prog, pcnt, obuf} =
  unlines
    [ "================================",
      "Register A:      " <> show regA,
      "Register B:      " <> show regB,
      "Register C:      " <> show regC,
      "Program Counter: " <> show pcnt,
      "",
      "Program: " <> (show . VU.toList) prog,
      "Output:  " <> prettyOutputBuffer obuf,
      "================================"
    ]

-- | Pretty-print the output buffer.
prettyOutputBuffer :: [Word8] -> String
prettyOutputBuffer = unpack . intercalate "," . fmap (pack . show) . reverse

-- | Pretty-print the program from a machine.
prettyProgram :: Machine -> String
prettyProgram machine = decodeProgram machine & fmap prettyInstr & unlines

-- | Pretty-print an instruction.
prettyInstr :: Instr -> String
prettyInstr (Instr opcode b) = prettyOpcode opcode <> " " <> show b

-- | Pretty-print an opcode.
prettyOpcode :: Opcode -> String
prettyOpcode opcode =
  case opcode of
    Adv -> "adv"
    Bxl -> "bxl"
    Bst -> "bst"
    Jnz -> "jnz"
    Bxc -> "bxc"
    Out -> "out"
    Bdv -> "bdv"
    Cdv -> "cdv"

---- Parsing ------------------------------------------------------------------

type Parser = Parsec Void Text

loadFile :: FilePath -> IO Machine
loadFile inputFile = do
  txt <- readFile inputFile
  let parseResult = parse parseDocument inputFile txt
  case parseResult of
    Left errorBundle -> do
      putStrLn "ERROR: Could not parse input file."
      putStrLn $ errorBundlePretty errorBundle
      exitFailure
    Right input -> pure input

parseDocument :: Parser Machine
parseDocument = do
  regA <- parseRegister 'A' <* newline
  regB <- parseRegister 'B' <* newline
  regC <- parseRegister 'C' <* newline
  _ <- newline
  prog <- parseProgram
  _ <- optional newline
  _ <- eof
  let pcnt = 0
  let obuf = []
  pure $ Machine {..}

parseRegister :: Char -> Parser Reg
parseRegister reg_id = string "Register " *> char reg_id *> ": " *> decimal

parseProgram :: Parser BubVec
parseProgram =
  string "Program: " *> (VU.fromList <$> (decimal `sepBy1` char ','))