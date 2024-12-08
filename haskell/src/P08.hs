module P08 (main) where

import Data.Bifunctor (bimap)
import Data.Functor (($>))
import Data.List (tails)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text.IO (readFile)
import qualified Data.Vector as V
import Data.Void (Void)
import Data.Word (Word32)
import qualified Grid
import System.Exit (exitFailure)
import Text.Megaparsec (Parsec, errorBundlePretty, parse, (<|>))
import Text.Megaparsec.Char (char, digitChar, lowerChar, upperChar)
import Prelude hiding (readFile)

main :: IO ()
main = do
  putStrLn "Day 8"
  inGrid <- readInput
  let part1Result = part1 inGrid
  putStrLn $ "Part 1 result: " <> show part1Result

data Cell
  = CellEmpty
  | CellAntenna !Char
  deriving (Eq, Ord, Show)

data AntennaAndLocs = AntennaAndLocs !Char [(Int, Int)]
  deriving (Eq, Show)

type InGrid = Grid.Grid V.Vector Cell

type AntiNodeGrid = Grid.Grid V.Vector Bool

part1 :: InGrid -> Int
part1 inGrid = Grid.countEqual True $ createAntiNodeGrid inGrid

createAntiNodeGrid :: InGrid -> AntiNodeGrid
createAntiNodeGrid inGrid =
  let locs :: [AntennaAndLocs]
      locs = findAntennaLocs inGrid

      ans :: [(Int, Int)]
      ans = antinodesForAntennas locs

      answ32 :: [(Word32, Word32)]
      answ32 = bimap fromIntegral fromIntegral <$> ans

      emptyGrid :: AntiNodeGrid
      emptyGrid =
        Grid.filledWith
          (Grid.getRows inGrid)
          (Grid.getCols inGrid)
          False
   in Grid.setMulti True answ32 emptyGrid

antinodesForAntennas :: [AntennaAndLocs] -> [(Int, Int)]
antinodesForAntennas = concatMap antinodesForAntenna

antinodesForAntenna :: AntennaAndLocs -> [(Int, Int)]
antinodesForAntenna (AntennaAndLocs _ xs) =
  concatMap antiNodePair (uniquePairs xs)

findAntennaLocs :: InGrid -> [AntennaAndLocs]
findAntennaLocs inGrid =
  mapMaybe toAntennaAndLocs (Map.toList $ Grid.locations inGrid)
  where
    toAntennaAndLocs (CellAntenna c, locs) =
      Just $ AntennaAndLocs c (wptoi <$> locs)
    toAntennaAndLocs (CellEmpty, _) =
      Nothing

    wptoi (x, y) = (fromIntegral x, fromIntegral y)

uniquePairs :: [a] -> [(a, a)]
uniquePairs xs = [(x, y) | (x : ys) <- tails xs, y <- ys]

antiNodePair :: ((Int, Int), (Int, Int)) -> [(Int, Int)]
antiNodePair ((x1, y1), (x2, y2)) =
  let dx = x2 - x1
      dy = y2 - y1
   in [ (x1 - dx, y1 - dy),
        (x2 + dx, y2 + dy)
      ]

---- Parsing ------------------------------------------------------------------

type Parser = Parsec Void Text

inputFile :: FilePath
inputFile = "../inputs/input-day-08.txt"

readInput :: IO InGrid
readInput = do
  txt <- readFile inputFile
  let parseResult = parse (Grid.parseGridDenseNL parseCell) inputFile txt
  case parseResult of
    Left errorBundle -> do
      putStrLn "ERROR: Could not parse input file."
      putStrLn $ errorBundlePretty errorBundle
      exitFailure
    Right grid -> pure grid

parseCell :: Parser Cell
parseCell =
  (char '.' $> CellEmpty)
    <|> (CellAntenna <$> (upperChar <|> lowerChar <|> digitChar))
