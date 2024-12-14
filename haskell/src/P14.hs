{-# LANGUAGE OverloadedStrings #-}

module P14 where

import Control.Exception (assert)
import Data.Text (Text)
import Data.Text.IO (readFile)
import Data.Void (Void)
import System.Exit (exitFailure)
import Text.Megaparsec (Parsec, eof, errorBundlePretty, optional, parse, some)
import Text.Megaparsec.Char (char, newline, string)
import Text.Megaparsec.Char.Lexer (decimal, signed)
import Prelude hiding (readFile)

main :: IO ()
main = do
  putStrLn "Day 14"
  robots <- readInput
  let part1Result = part1 robots
  putStrLn $ "Part 1 Result: " <> show part1Result

---- Part1 --------------------------------------------------------------------

part1 :: [Robot] -> Int
part1 robots =
  let regionSize = RegionSize 101 103
      -- regionSize = RegionSize 11 7
      nSteps = 100
   in countInQuadrants regionSize $ traceRobot regionSize nSteps <$> robots

countInQuadrants :: RegionSize -> [Robot] -> Int
countInQuadrants (RegionSize rx ry) robots =
  let mid_x = (rx - 1) `div` 2
      mid_y = (ry - 1) `div` 2
      q1 = inSquare (0, 0) (mid_x, mid_y) robots
      q2 = inSquare (mid_x + 1, 0) (mid_x, mid_y) robots
      q3 = inSquare (0, mid_y + 1) (mid_x, mid_y) robots
      q4 = inSquare (mid_x + 1, mid_y + 1) (mid_x, mid_y) robots
   in q1 * q2 * q3 * q4

inSquare :: (Int, Int) -> (Int, Int) -> [Robot] -> Int
inSquare (sx, sy) (wx, wy) robots =
  let isInSquare (Robot (V2 x y) _) =
        x >= sx && x < (sx + wx) && y >= sy && y < (sy + wy)
   in length $ filter isInSquare robots

traceRobot :: RegionSize -> Int -> Robot -> Robot
traceRobot (RegionSize rx ry) nSteps (Robot (V2 x y) v@(V2 vx vy)) =
  let xf = (x + nSteps * vx) `mod` rx
      yf = (y + nSteps * vy) `mod` ry
      x' = if xf >= 0 then xf else rx + xf
      y' = if yf >= 0 then yf else ry + yf
   in assert (x' >= 0 && y' >= 0 && x' < rx && y' < ry) $
        Robot (V2 x' y') v

---- Types --------------------------------------------------------------------

-- | State vector of a robot.
data Robot = Robot
  { robotPos :: {-# UNPACK #-} !V2,
    robotVel :: {-# UNPACK #-} !V2
  }
  deriving (Eq, Show)

-- 2D point or 2D vector.
data V2 = V2 {-# UNPACK #-} !Int {-# UNPACK #-} !Int deriving (Eq, Show)

data RegionSize = RegionSize
  { regionSizeX :: {-# UNPACK #-} !Int,
    regionSizeY :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Show)

---- Parsing ------------------------------------------------------------------

type Parser = Parsec Void Text

inputFile :: FilePath
inputFile = "../inputs/input-day-14.txt"

readInput :: IO [Robot]
readInput = do
  txt <- readFile inputFile
  let parseResult = parse parseDocument inputFile txt
  case parseResult of
    Left errorBundle -> do
      putStrLn "ERROR: Could not parse input file."
      putStrLn $ errorBundlePretty errorBundle
      exitFailure
    Right input -> pure input

parseDocument :: Parser [Robot]
parseDocument = some (parseRobot <* newline) <* optional newline <* eof

parseRobot :: Parser Robot
parseRobot = do
  p <- parsePair 'p'
  _ <- char ' '
  v <- parsePair 'v'
  pure $ Robot p v

parsePair :: Char -> Parser V2
parsePair label = do
  _ <- char label
  _ <- string "="
  x <- signedDecimal
  _ <- char ','
  V2 x <$> signedDecimal

signedDecimal :: Parser Int
signedDecimal = signed (return ()) decimal