module P10 (main) where

import Data.Char (digitToInt)
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.IO (readFile)
import qualified Data.Vector.Unboxed as VU
import Data.Void (Void)
import Data.Word (Word32)
import Grid (Grid)
import qualified Grid
import System.Exit (exitFailure)
import Text.Megaparsec (Parsec, eof, errorBundlePretty, optional, parse)
import Text.Megaparsec.Char (digitChar, newline)
import Prelude hiding (readFile)

main :: IO ()
main = do
  putStrLn "Day 10"
  altMap <- readInput
  let resultPart1 = part1 altMap
  let resultPart2 = part2 altMap
  putStrLn $ "Part 1 result: " <> show resultPart1
  putStrLn $ "Part 2 result: " <> show resultPart2

---- Part 1 -------------------------------------------------------------------

part1 :: AltMap -> Int
part1 altMap =
  let heads = candidateTrailHeads altMap
      scores = traceAllTrailsFrom ScorePeaks altMap <$> heads
   in sum scores

part2 :: AltMap -> Int
part2 altMap =
  let heads = candidateTrailHeads altMap
      scores = traceAllTrailsFrom ScoreUniq altMap <$> heads
   in sum scores

data Score = ScorePeaks | ScoreUniq

-- | Find all candidate trail heads in a map. These are altitudes of zero.
candidateTrailHeads :: AltMap -> [(Word32, Word32)]
candidateTrailHeads altMap =
  [ (r, c)
    | r <- [0 .. Grid.getRows altMap - 1],
      c <- [0 .. Grid.getCols altMap - 1],
      Grid.getElem altMap (r, c) == Just 0
  ]

traceAllTrailsFrom :: Score -> AltMap -> (Word32, Word32) -> Int
traceAllTrailsFrom score altMap cs =
  length $ foldr (\_ -> trailsStep score altMap) [cs] [1 :: Int .. 9]

-- | Take a single step along all trails.
trailsStep :: Score -> AltMap -> [(Word32, Word32)] -> [(Word32, Word32)]
trailsStep score altMap cs = f $ concatMap (availableIncSteps altMap) cs
  where
    f = case score of
      ScorePeaks -> nub
      ScoreUniq -> id

-- | Given an altitude map, and current coordinates, return a list of
--   coordinates where the map goes up by one.
availableIncSteps :: AltMap -> (Word32, Word32) -> [(Word32, Word32)]
availableIncSteps altMap coord@(r, c) =
  let curHeight :: Int
      curHeight =
        fromMaybe
          (error "Coordinaate must be valid")
          (Grid.getElem altMap coord)

      validHeight :: Int
      validHeight = curHeight + 1

      coordIsValidIncStep :: (Word32, Word32) -> Bool
      coordIsValidIncStep c' =
        case Grid.getElem altMap c' of
          Nothing -> False
          Just x | x == validHeight -> True
          Just _ | otherwise -> False

      testCoords :: [(Word32, Word32)]
      testCoords = [(r, c + 1), (r, c - 1), (r + 1, c), (r - 1, c)]
   in filter coordIsValidIncStep testCoords

---- Altitude Map -------------------------------------------------------------

-- | Altitude map.
type AltMap = Grid VU.Vector Int

---- Parsing ------------------------------------------------------------------

type Parser = Parsec Void Text

inputFile :: FilePath
inputFile = "../inputs/input-day-10.txt"

readInput :: IO AltMap
readInput = do
  txt <- readFile inputFile
  let parseResult = parse parseDocument inputFile txt
  case parseResult of
    Left errorBundle -> do
      putStrLn "ERROR: Could not parse input file."
      putStrLn $ errorBundlePretty errorBundle
      exitFailure
    Right inputMap -> pure inputMap

parseDocument :: Parser AltMap
parseDocument =
  Grid.parseGridDenseNL (digitToInt <$> digitChar) <* optional newline <* eof