module P11 (main) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Text.IO (readFile)
import Data.Void (Void)
import System.Exit (exitFailure)
import Text.Megaparsec (Parsec, eof, errorBundlePretty, optional, parse, sepBy)
import Text.Megaparsec.Char (char, newline)
import Text.Megaparsec.Char.Lexer (decimal)
import Prelude hiding (readFile)

main :: IO ()
main = do
  putStrLn "Day 11"
  stones <- readInput
  let part1Result = part1 stones
  putStrLn $ "Part 1 result: " <> show part1Result
  let part2Result = part2 stones
  putStrLn $ "Part 2 result: " <> show part2Result

---- Part 2 -------------------------------------------------------------------

part2 :: Stones -> Int
part2 = countCachedStones 75

newtype StoneNumber = StoneNumber Int deriving (Eq, Ord, Show)

newtype StepsRemaining = StepsRemaining Int deriving (Eq, Ord, Show)

data StoneAtLevel
  = StoneAtLevel !StepsRemaining !StoneNumber
  deriving (Eq, Show, Ord)

type Cache = Map StoneAtLevel Int

data SplitResult
  = SRSingle !StoneAtLevel
  | SRPair !StoneAtLevel StoneAtLevel

countCachedStones :: Int -> Stones -> Int
countCachedStones nsplits = go Map.empty
  where
    go :: Cache -> Stones -> Int
    go _ (Stones []) = 0
    go cache (Stones (x : xs)) =
      let sal = StoneAtLevel (StepsRemaining nsplits) (StoneNumber x)
          (count, cache') = countCached cache sal
       in count + go cache' (Stones xs)

countCached :: Cache -> StoneAtLevel -> (Int, Cache)
countCached cache (StoneAtLevel (StepsRemaining 0) _) = (1, cache)
countCached cache sal =
  case Map.lookup sal cache of
    Just count ->
      (count, cache)
    Nothing ->
      case split sal of
        SRSingle a ->
          let (count, cache') = countCached cache a
              cache'' = Map.insert sal count cache'
           in (count, Map.union cache cache'')
        SRPair a b ->
          let (count_a, cache_a) = countCached cache a
              cache_a'' = Map.insert a count_a cache_a
              (count_b, cache_b) = countCached (Map.union cache cache_a'') b
              cache_b'' = Map.insert b count_b cache_b
           in (count_a + count_b, cache_b'')

split :: StoneAtLevel -> SplitResult
split sal =
  let StoneAtLevel (StepsRemaining nrem) (StoneNumber sn) = sal
      nrem' = StepsRemaining (nrem - 1)
   in case sn of
        0 -> SRSingle $ StoneAtLevel nrem' (StoneNumber 1)
        y
          | hasEvenDigits y ->
              let (a, b) = splitDigits y
                  sa = StoneAtLevel nrem' (StoneNumber a)
                  sb = StoneAtLevel nrem' (StoneNumber b)
               in SRPair sa sb
        y -> SRSingle $ StoneAtLevel nrem' (StoneNumber (2024 * y))

---- Part 1 -------------------------------------------------------------------

newtype Stones = Stones [Int] deriving (Eq, Show)

part1 :: Stones -> Int
part1 = countCachedStones 25

hasEvenDigits :: Int -> Bool
hasEvenDigits = even . countDigits

countDigits :: Int -> Int
countDigits x
  | x >= 0 && x < 10 = 1
  | otherwise = 1 + countDigits (x `div` 10)

splitDigits :: Int -> (Int, Int)
splitDigits x =
  let half = countDigits x `div` 2
      sx = show x
   in (read (take half sx), read (drop half sx))

---- Parsing ------------------------------------------------------------------

type Parser = Parsec Void Text

inputFile :: FilePath
inputFile = "../inputs/input-day-11.txt"

readInput :: IO Stones
readInput = do
  txt <- readFile inputFile
  let parseResult = parse parseDocument inputFile txt
  case parseResult of
    Left errorBundle -> do
      putStrLn "ERROR: Could not parse input file."
      putStrLn $ errorBundlePretty errorBundle
      exitFailure
    Right input -> pure input

parseDocument :: Parser Stones
parseDocument =
  Stones <$> (decimal `sepBy` char ' ') <* optional newline <* eof
