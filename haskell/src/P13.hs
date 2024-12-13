{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module P13 (main) where

import Control.Exception (assert)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text.IO (readFile)
import Data.Void (Void)
import System.Exit (exitFailure)
import Text.Megaparsec (Parsec, eof, errorBundlePretty, optional, parse, some)
import Text.Megaparsec.Char (char, newline, string)
import Text.Megaparsec.Char.Lexer (decimal)
import Prelude hiding (readFile)

main :: IO ()
main = do
  putStrLn "Day 13"
  machines <- readInput
  let part1Result = part1 machines
  putStrLn $ "Part 1 result: " <> show part1Result
  let part2Result = part2 machines
  putStrLn $ "Part 2 result: " <> show part2Result

---- Part 2 -------------------------------------------------------------------

part2 :: [Machine] -> Int
part2 =
  sum
    . fmap solutionCost
    . mapMaybe (solveMachineNonCollinear . adjustMachineForPart2)

adjustMachineForPart2 :: Machine -> Machine
adjustMachineForPart2 (Machine a b (P2 px py)) =
  Machine a b (P2 (10000000000000 + px) (10000000000000 + py))

---- Part 1 -------------------------------------------------------------------

part1 :: [Machine] -> Int
part1 = sum . fmap solutionCost . mapMaybe solveMachineNonCollinear

-- | Location for a machine.
data P2 = P2 !Int !Int deriving (Eq, Show)

p2Tuple :: P2 -> (Int, Int)
p2Tuple (P2 x y) = (x, y)

-- | Machine data.
--
--   Pressing button A costs 3 tokens
--   Pressing button B costs 1 token
data Machine = Machine
  { machineBtnA :: !P2, -- 3 tokens
    machineBtnB :: !P2, -- 1 token
    machinePrize :: !P2
  }
  deriving (Eq, Show)

data Solution = Solution
  { solutionAPresses :: !Int,
    solutionBPresses :: !Int
  }
  deriving (Eq, Show)

-- | Find the cost of a solution.
solutionCost :: Solution -> Int
solutionCost (Solution aa bb) = 3 * aa + bb

-- | Solve a machine of the two vectors are not collinear.
solveMachineNonCollinear :: Machine -> Maybe Solution
solveMachineNonCollinear machine =
  let p = machinePrize machine
      a = machineBtnA machine
      b = machineBtnB machine
      (px, py) = p2Tuple p
      (ax, ay) = p2Tuple a
      (bx, by) = p2Tuple b
      det = ax * by - ay * bx
      ca_numer = by * px - bx * py
      cb_numer = -ay * px + ax * py
      ca = ca_numer `div` det
      cb = cb_numer `div` det
   in assert (not $ vectorsCollinear a b) $
        if (ca_numer `mod` det == 0) && (cb_numer `mod` det == 0)
          then Just $ Solution ca cb
          else Nothing

-- | Check if two vectors are collinear.
vectorsCollinear :: P2 -> P2 -> Bool
vectorsCollinear !v1 !v2 =
  let (x1, y1) = p2Tuple v1
      (x2, y2) = p2Tuple v2
   in ((x1 `mod` x2 == 0) && (y1 `mod` y2 == 0))
        || ((x2 `mod` x1 == 0) && (y2 `mod` y1 == 0))

---- Parsing ------------------------------------------------------------------

type Parser = Parsec Void Text

inputFile :: FilePath
inputFile = "../inputs/input-day-13.txt"

readInput :: IO [Machine]
readInput = do
  txt <- readFile inputFile
  let parseResult = parse parseDocument inputFile txt
  case parseResult of
    Left errorBundle -> do
      putStrLn "ERROR: Could not parse input file."
      putStrLn $ errorBundlePretty errorBundle
      exitFailure
    Right input -> pure input

parseDocument :: Parser [Machine]
parseDocument = some parseMachine <* eof

parseMachine :: Parser Machine
parseMachine =
  Machine
    <$> (parseButton 'A' <* newline)
    <*> (parseButton 'B' <* newline)
    <*> (parsePrize <* newline)
    <* optional (some newline)

parseButton :: Char -> Parser P2
parseButton charLabel = do
  _ <- string "Button "
  _ <- char charLabel
  _ <- string ": X+"
  x <- decimal
  _ <- string ", Y+"
  P2 x <$> decimal

parsePrize :: Parser P2
parsePrize = do
  _ <- string "Prize: X="
  x <- decimal
  _ <- string ", Y="
  P2 x <$> decimal