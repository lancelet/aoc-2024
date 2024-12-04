{-# LANGUAGE OverloadedStrings #-}

module P03 where

import Control.Applicative ((<|>))
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
  exprs <- readInput
  let rsum = sumExprs exprs
  putStrLn $ "Sum of expressions: " <> show rsum

---- Multiplication Evaluation ------------------------------------------------

newtype Expr = Expr [Mul] deriving (Eq, Show)

data Mul = Mul !Int !Int deriving (Eq, Show)

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

readInput :: IO [Expr]
readInput = do
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