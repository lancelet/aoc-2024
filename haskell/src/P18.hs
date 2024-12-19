{-# LANGUAGE ScopedTypeVariables #-}

module P18 (main, mainEx1) where

import AStar
  ( Mode (ModeSingle),
    Path (Path),
    Result (NoPathFound, PathsFound),
    astar,
  )
import Control.Monad (forM_)
import Control.Monad.ST (runST)
import Data.Bifunctor (second)
import Data.Function ((&))
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text.IO (readFile)
import Data.Void (Void)
import Data.Word (Word32)
import Grid (BoolGrid)
import qualified Grid
import System.Exit (exitFailure)
import Text.Megaparsec (Parsec, eof, errorBundlePretty, many, optional, parse)
import Text.Megaparsec.Char (char, newline)
import Text.Megaparsec.Char.Lexer (decimal)
import Prelude hiding (readFile)

main :: IO ()
main = do
  putStrLn "Day 18"
  part1
  part2

part1 :: IO ()
part1 = do
  putStrLn "Part 1"
  let inputFile = "../inputs/input-day-18.txt"
  fallingBytes <- loadFile inputFile
  let memSpace = populateMemSpace 71 71 1024 fallingBytes
  let nSteps =
        fromMaybe (error "No path found") (navigateMemSpaceNSteps memSpace)
  putStrLn $ "Length of path: " <> show nSteps

mainEx1 :: IO ()
mainEx1 = do
  putStrLn "Example 1"
  let inputFile = "../inputs/input-day-18-example.txt"
  fallingBytes <- loadFile inputFile
  let mxy = findBlockingFallingByte 7 7 12 fallingBytes
  putStrLn $ "Blocking byte coordinates: " <> show mxy

part2 :: IO ()
part2 = do
  putStrLn "Part 2"
  let inputFile = "../inputs/input-day-18.txt"
  fallingBytes <- loadFile inputFile
  let mxy = findBlockingFallingByte 71 71 1024 fallingBytes
  putStrLn $ "Block byte coordinates: " <> show mxy

---- Processing ---------------------------------------------------------------

findBlockingFallingByte ::
  -- | Number of rows of the memory space.
  Word32 ->
  -- | Number of columns of the memory space.
  Word32 ->
  -- | Starting number of bytes.
  Int ->
  -- | The falling bytes.
  FallingBytes ->
  -- | Optional falling byte that blocks the path.
  Maybe XY
findBlockingFallingByte rows cols start_bytes fb =
  let lohi_init = (start_bytes, length . unFallingBytes $ fb)
      p i =
        let memSpace = populateMemSpace rows cols i fb
         in case navigateMemSpaceNSteps memSpace of
              Just _ -> False
              Nothing -> True
      maybe_idx = binaryThresholdSearch lohi_init p
   in case maybe_idx of
        Nothing -> Nothing
        Just i -> Just $ unFallingBytes fb !! i

binaryThresholdSearch :: (Int, Int) -> (Int -> Bool) -> Maybe Int
binaryThresholdSearch lohi p =
  case lohi of
    (lo, hi) | lo + 1 >= hi, p lo -> Just (lo - 1)
    (lo, hi) | lo + 1 >= hi, p hi -> Just (hi - 1)
    (lo, hi) ->
      let h = (hi + lo) `div` 2
       in if p h
            then binaryThresholdSearch (lo, h) p
            else binaryThresholdSearch (h + 1, hi) p

-- | Populate the memory space from a list of falling bytes.
populateMemSpace ::
  -- | Number of rows for the memory space.
  Word32 ->
  -- | Number of columns for the memory space.
  Word32 ->
  -- | Number of bytes to allow to fall.
  Int ->
  -- | The falling bytes.
  FallingBytes ->
  -- | Populated memory space.
  MemSpace
populateMemSpace rows cols n_bytes fb = runST $ do
  mask <- Grid.thaw $ Grid.filledWith rows cols False
  forM_ (take n_bytes . unFallingBytes $ fb) $ \(xy :: XY) ->
    Grid.write mask (toRC xy) True
  MemSpace <$> Grid.freeze mask

-- | Find the number of steps to get from the beginning to end of the
--   memory space.
navigateMemSpaceNSteps :: MemSpace -> Maybe Int
navigateMemSpaceNSteps m =
  case navigateMemSpace m of
    NoPathFound -> Nothing
    PathsFound ps -> Just . (\(Path _ xs) -> length xs) . head $ ps

-- | Use A* to find the paths from the beginning of the memory space to
--   the end location.
navigateMemSpace :: MemSpace -> Result Dir Node
navigateMemSpace memSpace =
  astar ModeSingle start end_fn successor cost heuristic
  where
    start = Node (0, 0)
    target = memSpaceTarget memSpace
    end_fn = (target ==) . unNode
    successor = fmap (second Node) . validDirections memSpace . unNode
    cost _ = 1
    heuristic = rcManhattan target . unNode

-- | Direction we can travel in.
data Dir = DUp | DDown | DLeft | DRight deriving (Eq, Show)

-- | A node in our A* travels.
newtype Node = Node RC deriving (Eq, Ord, Show)

-- | Unapply the `Node` constructor.
unNode :: Node -> RC
unNode (Node rc) = rc

-- | Take a step in the given direction.
stepDir :: Dir -> RC -> RC
stepDir dir (r, c) =
  case dir of
    DUp -> (r - 1, c)
    DDown -> (r + 1, c)
    DLeft -> (r, c + 1)
    DRight -> (r, c - 1)

-- | Find the valid directions to move given a starting coordinate.
validDirections :: MemSpace -> RC -> [(Dir, RC)]
validDirections ms rc =
  [DUp, DDown, DLeft, DRight]
    & mapMaybe
      ( \dir ->
          let rc' = stepDir dir rc
           in if memSpaceIsObstacle' ms rc'
                then Nothing
                else Just (dir, rc')
      )

---- Types --------------------------------------------------------------------

-- | X,Y coordinates.
data XY = XY !Word32 !Word32 deriving (Eq, Show)

-- | Row, Column.
type RC = (Word32, Word32)

-- | The falling bytes.
newtype FallingBytes = FallingBytes [XY] deriving (Eq, Show)

-- | Memory space.
--
--   This is a boolean grid, where `True` represents a corrupted byte, and
--   `False` represents a valid location.
newtype MemSpace = MemSpace BoolGrid deriving (Eq, Show)

-- | Convert X,Y coordinates to R,C.
toRC :: XY -> RC
toRC (XY x y) = (y, x)

-- | Manhatten distance between two RC coordinates.
rcManhattan :: RC -> RC -> Int
rcManhattan (x1, y1) (x2, y2) =
  abs (fromIntegral x2 - fromIntegral x1)
    + abs (fromIntegral y2 - fromIntegral y1)

-- | Undo the `FallingBytes` constructor.
unFallingBytes :: FallingBytes -> [XY]
unFallingBytes (FallingBytes fb) = fb

-- | Undo the `MemSpace` constructor.
unMemSpace :: MemSpace -> BoolGrid
unMemSpace (MemSpace mask) = mask

-- | Check if a memory space location is an obstacle.
memSpaceIsObstacle :: MemSpace -> RC -> Maybe Bool
memSpaceIsObstacle ms = Grid.getElem (unMemSpace ms)

-- | Check if a memory space location is an obstacle, assuming out-of-bounds
--   locations are obstacles.
memSpaceIsObstacle' :: MemSpace -> RC -> Bool
memSpaceIsObstacle' ms = fromMaybe True . memSpaceIsObstacle ms

-- | Return the number of rows in the memory space.
memSpaceRows :: MemSpace -> Word32
memSpaceRows = Grid.getRows . unMemSpace

-- | Return the number of columns in the memory space.
memSpaceCols :: MemSpace -> Word32
memSpaceCols = Grid.getCols . unMemSpace

-- | Return the target location in the memory space (bottom right).
memSpaceTarget :: MemSpace -> RC
memSpaceTarget m = (memSpaceRows m - 1, memSpaceCols m - 1)

{-
-- | Pretty-print the `MemSpace` grid.
prettyMemSpace :: MemSpace -> Text
prettyMemSpace memSpace =
  unMemSpace memSpace & Grid.toLists & fmap prettyLine & T.unlines
  where
    prettyLine = T.pack . fmap prettyCell
    prettyCell True = '#'
    prettyCell False = '.'
-}

---- Parsing ------------------------------------------------------------------

loadFile :: FilePath -> IO FallingBytes
loadFile inputFile = do
  txt <- readFile inputFile
  let parseResult = parse parseDocument inputFile txt
  case parseResult of
    Left errorBundle -> do
      putStrLn "ERROR: Could not parse input file."
      putStrLn $ errorBundlePretty errorBundle
      exitFailure
    Right input -> pure input

type Parser = Parsec Void Text

parseDocument :: Parser FallingBytes
parseDocument =
  FallingBytes
    <$> many (XY <$> (decimal <* char ',') <*> (decimal <* newline))
    <* optional newline
    <* eof