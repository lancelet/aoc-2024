{-# LANGUAGE OverloadedStrings #-}

module P03 (main) where

import Control.Applicative ((<|>))
import Data.Functor (($>))
import Data.Text (Text)
import Data.Text.IO (readFile)
import Data.Void (Void)
import System.Exit (exitFailure)
import Text.Megaparsec (Parsec, anySingle, eof, many, parseMaybe, some, takeWhileP, try)
import Text.Megaparsec.Char (char, newline, string)
import Text.Megaparsec.Char.Lexer (decimal)
import Prelude hiding (readFile)

---- Main ---------------------------------------------------------------------

main :: IO ()
main = do
  exprs <- readInputFirstPart
  progs <- readInputSecondPart
  let rsum = sumExprs exprs
  let rsumed = sumAllProgramInterpret progs
  putStrLn $ "Sum of expressions:   " <> show rsum
  putStrLn $ "Interpreted programs: " <> show rsumed

---- Multiplication Evaluation ------------------------------------------------

data RunState = Enabled | Disabled

newtype Program = Program [Instruction] deriving (Eq, Show)

data Instruction
  = IMul !Int !Int
  | IEnable
  | IDisable
  deriving (Eq, Show)

newtype Expr = Expr [Mul] deriving (Eq, Show)

data Mul = Mul !Int !Int deriving (Eq, Show)

sumAllProgramInterpret :: [Program] -> Int
sumAllProgramInterpret ps = sum $ interpret <$> ps

interpret :: Program -> Int
interpret = go Enabled 0
  where
    go :: RunState -> Int -> Program -> Int
    go Disabled r (Program (IEnable : xs)) = go Enabled r (Program xs)
    go Disabled r (Program (_ : xs)) = go Disabled r (Program xs)
    go Enabled r (Program (IDisable : xs)) = go Disabled r (Program xs)
    go Enabled r (Program (IEnable : xs)) = go Enabled r (Program xs)
    go Enabled r (Program ((IMul x y) : xs)) = go Enabled (r + x * y) (Program xs)
    go _ r (Program []) = r

evalMul :: Mul -> Int
evalMul (Mul x y) = x * y

evalExpr :: Expr -> Int
evalExpr (Expr muls) = sum $ evalMul <$> muls

sumExprs :: [Expr] -> Int
sumExprs exprs = sum $ evalExpr <$> exprs

---- Parsing Input ------------------------------------------------------------

type Parser = Parsec Void Text

inputFile :: FilePath
inputFile = "../inputs/input-day-03.txt"

---- Second Part --------------------------------------------------------------

readInputSecondPart :: IO [Program]
readInputSecondPart = do
  txt <- readFile inputFile
  let res = parseMaybe parseAllPrograms txt
  case res of
    Nothing -> do
      putStrLn "ERROR: Could not parse input file."
      exitFailure
    Just progs -> pure progs

parseAllPrograms :: Parser [Program]
parseAllPrograms = some (parseProgram <* newline) <* eof

parseProgram :: Parser Program
parseProgram = Program <$> parseFoundInstructions

parseFoundInstructions :: Parser [Instruction]
parseFoundInstructions =
  many parseNextInstruction <* takeWhileP Nothing ('\n' /=)

parseNextInstruction :: Parser Instruction
parseNextInstruction =
  try parseInstruction <|> try (anySingle *> parseNextInstruction)

parseInstruction :: Parser Instruction
parseInstruction =
  (mulToInstruction <$> parseMul)
    <|> parseEnable
    <|> parseDisable

mulToInstruction :: Mul -> Instruction
mulToInstruction (Mul x y) = IMul x y

parseEnable :: Parser Instruction
parseEnable = string "do()" $> IEnable

parseDisable :: Parser Instruction
parseDisable = string "don't()" $> IDisable

---- First Part ---------------------------------------------------------------

readInputFirstPart :: IO [Expr]
readInputFirstPart = do
  txt <- readFile inputFile
  let res = parseMaybe parseAllExpr txt
  case res of
    Nothing -> do
      putStrLn "ERROR: Could not parse input file."
      exitFailure
    Just exprs -> pure exprs

parseAllExpr :: Parser [Expr]
parseAllExpr = some (parseExpr <* newline) <* eof

parseExpr :: Parser Expr
parseExpr = Expr <$> parseFoundMul

parseFoundMul :: Parser [Mul]
parseFoundMul = many parseNextMul <* takeWhileP Nothing ('\n' /=)

parseNextMul :: Parser Mul
parseNextMul = try parseMul <|> try (anySingle *> parseNextMul)

parseMul :: Parser Mul
parseMul =
  Mul <$> (string "mul(" *> decimal) <*> (char ',' *> decimal <* char ')')