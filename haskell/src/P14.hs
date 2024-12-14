{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module P14 (main) where

import Codec.Picture (Image, Pixel8, writePng)
import Control.Exception (assert)
import Control.Monad (unless, when)
import Control.Monad.ST (runST)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Text.IO (readFile)
import qualified Data.Vector.Unboxed as VU
import Data.Void (Void)
import Grid (Grid)
import qualified Grid
import System.Exit (exitFailure)
import Text.Megaparsec (Parsec, eof, errorBundlePretty, optional, parse, some)
import Text.Megaparsec.Char (char, newline, string)
import Text.Megaparsec.Char.Lexer (decimal, signed)
import Text.Printf (printf)
import Prelude hiding (readFile)

main :: IO ()
main = do
  putStrLn "Day 14"
  robots <- readInput
  let part1Result = part1 robots
  putStrLn $ "Part 1 Result: " <> show part1Result

  putStrLn "Part 2"
  let regionSize = RegionSize 101 103
  -- Inspection shows that all the periods are the same
  let period = head $ robotPeriod regionSize <$> robots
  putStrLn $ "Peroid: " <> show period
  let stepSize = 1
  ref_frame <- newIORef (0 :: Int)
  ref_cur_robots <- newIORef robots
  ref_done <- newIORef False
  loop ref_done $ do
    frame <- readIORef ref_frame
    cur_robots <- readIORef ref_cur_robots
    let fileName :: String = printf "f%07d.png" frame
    putStrLn $ "Frame: " <> show frame <> " : " <> fileName
    writePng fileName $ plotRobotsImage regionSize cur_robots
    let next_robots = traceRobot regionSize stepSize <$> cur_robots
    writeIORef ref_frame (frame + stepSize)
    writeIORef ref_cur_robots next_robots
    when (frame == period) $ writeIORef ref_done True

loop :: IORef Bool -> IO () -> IO ()
loop ref action = do
  b <- readIORef ref
  unless b (action >> loop ref action)

---- Part2 --------------------------------------------------------------------

plotRobotsImage :: RegionSize -> [Robot] -> Image Pixel8
plotRobotsImage rs = Grid.toImage f . gridFromMap rs . collectRobots
  where
    f :: Int -> Pixel8
    f 0 = 0
    f 1 = 127
    f 2 = 200
    f 3 = 240
    f _ = 255

collectRobots :: [Robot] -> Map V2 Int
collectRobots = go Map.empty
  where
    go :: Map V2 Int -> [Robot] -> Map V2 Int
    go m [] = m
    go m (x : xs) = go (Map.insertWith (+) (robotPos x) 1 m) xs

gridFromMap :: RegionSize -> Map V2 Int -> Grid VU.Vector Int
gridFromMap (RegionSize rx ry) posMap = runST $ do
  grid <- Grid.thaw $ Grid.filledWith (fromIntegral ry) (fromIntegral rx) 0
  mapM_
    (\(V2 x y, n) -> Grid.write grid (fromIntegral y, fromIntegral x) n)
    (Map.toList posMap)
  Grid.freeze grid

robotPeriod :: RegionSize -> Robot -> Int
robotPeriod rs robot' = go 0 robot'
  where
    go 0 robot =
      let next = traceRobot rs 1 robot
       in if next == robot' then 0 else go 1 next
    go n robot =
      if robot == robot'
        then n
        else go (n + 1) (traceRobot rs 1 robot)

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
data V2 = V2 {-# UNPACK #-} !Int {-# UNPACK #-} !Int deriving (Eq, Ord, Show)

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