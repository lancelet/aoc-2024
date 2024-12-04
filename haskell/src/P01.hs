{-# LANGUAGE ScopedTypeVariables #-}

module P01 (main) where

import Control.Applicative (many, some, (<|>))
import Data.Functor (($>))
import Data.List (sort)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text.IO (readFile)
import Data.Void (Void)
import System.Exit (exitFailure)
import Text.Megaparsec (Parsec, parseMaybe)
import Text.Megaparsec.Char (newline, space1)
import Text.Megaparsec.Char.Lexer (decimal)
import Prelude hiding (readFile)

---- Main ---------------------------------------------------------------------

main :: IO ()
main = do
  (xs, ys) <- readInput
  let d = distance xs ys
  let s = similarity xs ys
  putStrLn $ "Computed distance:   " <> show d
  putStrLn $ "Computed similarity: " <> show s

---- Similarity score ---------------------------------------------------------

-- This is a bit slow; but meh.
similarity :: [Int] -> [Int] -> Int
similarity xs ys = go 0 xs
  where
    go :: Int -> [Int] -> Int
    go score [] = score
    go score (e : es) = go (score + countElem e ys * e) es

countElem :: forall a. (Eq a) => a -> [a] -> Int
countElem item = go 0
  where
    go :: Int -> [a] -> Int
    go count [] = count
    go count (e : es)
      | e == item = go (count + 1) es
      | otherwise = go count es

---- Compute distance ---------------------------------------------------------

distance :: [Int] -> [Int] -> Int
distance xs ys =
  let pairs :: [(Int, Int)]
      pairs = zip (sort xs) (sort ys)

      pairwise :: [Int]
      pairwise = map (\(x, y) -> abs (x - y)) pairs
   in sum pairwise

---- Parsing Input ------------------------------------------------------------

type Parser = Parsec Void Text

data InputLine = InputLine !Int !Int deriving (Eq, Show)

inputFile :: FilePath
inputFile = "../inputs/input-day-01.txt"

readInput :: IO ([Int], [Int])
readInput = do
  txt <- readFile inputFile
  let res = parseMaybe parseDoc txt
  case res of
    Nothing -> do
      putStrLn "ERROR: Could not parse input file."
      exitFailure
    Just ils -> pure (inputLinesToLists ils)

inputLinesToLists :: [InputLine] -> ([Int], [Int])
inputLinesToLists ils = unzip $ (\(InputLine x y) -> (x, y)) <$> ils

parseDoc :: Parser [InputLine]
parseDoc = fmap catMaybes (many parseInputLineMaybe)

parseInputLineMaybe :: Parser (Maybe InputLine)
parseInputLineMaybe = (Just <$> parseInputLine) <|> (parseEmptyLine $> Nothing)

parseEmptyLine :: Parser ()
parseEmptyLine = some space1 *> newline $> ()

parseInputLine :: Parser InputLine
parseInputLine = InputLine <$> decimal <* some space1 <*> decimal <* newline