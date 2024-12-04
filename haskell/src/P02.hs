{-# LANGUAGE ScopedTypeVariables #-}

module P02 (main) where

import Control.Applicative (many, optional)
import Data.Text (Text)
import Data.Text.IO (readFile)
import Data.Void (Void)
import System.Exit (exitFailure)
import Text.Megaparsec (Parsec, eof, errorBundlePretty, parse, sepBy)
import Text.Megaparsec.Char (char, newline)
import Text.Megaparsec.Char.Lexer (decimal)
import Prelude hiding (readFile)

---- Main ---------------------------------------------------------------------

main :: IO ()
main = do
  reports <- readInput
  let nSafe = countSafeReports reports
  putStrLn $ "Safe report count: " <> show nSafe

---- Handling Reports ---------------------------------------------------------

newtype Report = Report [Int] deriving (Eq, Show)

countSafeReports :: [Report] -> Int
countSafeReports = count isReportSafe

count :: forall a. (a -> Bool) -> [a] -> Int
count f = go 0
  where
    go :: Int -> [a] -> Int
    go n [] = n
    go n (x : xs)
      | f x = go (n + 1) xs
      | otherwise = go n xs

isReportSafe :: Report -> Bool
isReportSafe report =
  isReportSafeIncreasing report || isReportSafeDecreasing report

isReportSafeIncreasing :: Report -> Bool
isReportSafeIncreasing (Report xs) = all inRange $ mapAdjacent (flip (-)) xs
  where
    inRange :: Int -> Bool
    inRange x = (x == 1) || (x == 2) || (x == 3)

isReportSafeDecreasing :: Report -> Bool
isReportSafeDecreasing (Report xs) = all inRange $ mapAdjacent (-) xs
  where
    inRange :: Int -> Bool
    inRange x = (x == 1) || (x == 2) || (x == 3)

mapAdjacent :: forall a b. (a -> a -> b) -> [a] -> [b]
mapAdjacent f = go []
  where
    go :: [b] -> [a] -> [b]
    go bs (a1 : a2 : as) = go (f a1 a2 : bs) (a2 : as)
    go bs _ = reverse bs

---- Parsing Input ------------------------------------------------------------

type Parser = Parsec Void Text

inputFile :: FilePath
inputFile = "../inputs/input-day-02.txt"

readInput :: IO [Report]
readInput = do
  txt <- readFile inputFile
  let res = parse parseReports inputFile txt
  case res of
    Left errorBundle -> do
      putStrLn "ERROR: Could not parse input file."
      putStrLn $ errorBundlePretty errorBundle
      exitFailure
    Right reports -> pure reports

parseReports :: Parser [Report]
parseReports = many parseReportLine <* optional newline <* eof

parseReportLine :: Parser Report
parseReportLine = Report <$> sepBy decimal (char ' ') <* newline