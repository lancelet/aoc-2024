module P07 (main) where

import Data.Text (Text)
import Data.Text.IO (readFile)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import Data.Void (Void)
import System.Exit (exitFailure)
import Text.Megaparsec (Parsec, eof, errorBundlePretty, optional, parse, sepBy, some)
import Text.Megaparsec.Char (char, newline)
import Text.Megaparsec.Char.Lexer (decimal)
import Prelude hiding (readFile)

main :: IO ()
main = do
  putStrLn "Day 7"
  equations <- readInput
  let possibles = V.filter isPossibleEquation equations
  let part1Result = sum $ eqResult <$> V.toList possibles
  putStrLn $ "Part 1 Result: " <> show part1Result

-- | Equation with a final result and a vector of values.
data Equation = Equation
  { eqResult :: !Int,
    eqValues :: !(UV.Vector Int)
  }
  deriving (Eq, Show)

isPossibleEquation :: Equation -> Bool
isPossibleEquation (Equation result values) =
  let hd = UV.head values
      tl = UV.toList $ UV.tail values
   in go hd tl
  where
    go :: Int -> [Int] -> Bool
    go accum [] = result == accum
    go accum (x : xs)
      | accum > result = False
      | otherwise =
          go (accum * x) xs
            || go (accum + x) xs
            || go (catOp accum x) xs

catOp :: Int -> Int -> Int
catOp l r =
  let findPow :: Int -> Int
      findPow n = go 10
        where
          go :: Int -> Int
          go p
            | n < p = p
            | otherwise = go (p * 10)
   in l * findPow r + r

---- Parsing ------------------------------------------------------------------

type Parser = Parsec Void Text

inputFile :: FilePath
inputFile = "../inputs/input-day-07.txt"

readInput :: IO (V.Vector Equation)
readInput = do
  txt <- readFile inputFile
  let parseResult = parse parseEquations inputFile txt
  case parseResult of
    Left errorBundle -> do
      putStrLn "ERROR: Could not parse input file."
      putStrLn $ errorBundlePretty errorBundle
      exitFailure
    Right r -> pure r

parseEquations :: Parser (V.Vector Equation)
parseEquations = V.fromList <$> some parseEquation <* optional newline <* eof

parseEquation :: Parser Equation
parseEquation =
  Equation
    <$> ( decimal
            <* char ':'
            <* char ' '
        )
    <*> ( UV.fromList <$> (decimal `sepBy` char ' ')
        )
    <* newline