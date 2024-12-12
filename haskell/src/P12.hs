module P12 (main) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.IO (readFile)
import qualified Data.Vector as V
import Data.Void (Void)
import Data.Word (Word32)
import Grid (Grid)
import qualified Grid
import System.Exit (exitFailure)
import Text.Megaparsec (Parsec, eof, errorBundlePretty, optional, parse)
import Text.Megaparsec.Char (newline, upperChar)
import Prelude hiding (readFile)

main :: IO ()
main = do
  putStrLn "Day 12"
  plot <- readInput
  let part1Result = part1 plot
  putStrLn $ "Part 1 result: " <> show part1Result
  let part2Result = part2 plot
  putStrLn $ "Part 2 result: " <> show part2Result

---- Part 2 -------------------------------------------------------------------

part2 :: Plot -> Int
part2 = sum . fmap modRegionPrice . regions

modRegionPrice :: Region -> Int
modRegionPrice region = regionArea region * allFences (regionToFences region)

data Fence = FCorner | FHoriz | FVert | FNone deriving (Eq, Show)

newtype Fences = Fences (Grid V.Vector Fence) deriving (Eq, Show)

fencesRows :: Fences -> Word32
fencesRows (Fences fences) = Grid.getRows fences

fencesCols :: Fences -> Word32
fencesCols (Fences fences) = Grid.getCols fences

getFence :: Fences -> (Word32, Word32) -> Fence
getFence (Fences fences) rc = fromMaybe FNone $ Grid.getElem fences rc

{-
prettyFences :: Fences -> String
prettyFences (Fences fences) = unlines $ prettyRows <$> Grid.toLists fences
  where
    prettyRows = fmap prettyFence
    prettyFence FCorner = '+'
    prettyFence FHoriz = '-'
    prettyFence FVert = '|'
    prettyFence FNone = ' '
-}

allFences :: Fences -> Int
allFences fences = allHoriz fences + allVert fences

allHoriz :: Fences -> Int
allHoriz fences = sum $ scanHoriz fences <$> [0 .. fencesRows fences - 1]

allVert :: Fences -> Int
allVert fences = sum $ scanVert fences <$> [0 .. fencesCols fences - 1]

scanHoriz :: Fences -> Word32 -> Int
scanHoriz fences row = if odd row then 0 else go False 0 0
  where
    p :: (Word32, Word32) -> Fence
    p = getFence fences

    go :: Bool -> Word32 -> Int -> Int
    go inside col accum
      | col == fencesCols fences = accum
      | inside && p (row, col) == FHoriz = go True (col + 1) accum
      | not inside && p (row, col) == FHoriz = go True (col + 1) (accum + 1)
      | inside
          && p (row, col) == FCorner
          && (p (row + 1, col) == FVert || p (row - 1, col) == FVert) =
          go False (col + 1) accum
      | otherwise = go inside (col + 1) accum

scanVert :: Fences -> Word32 -> Int
scanVert fences col = if odd col then 0 else go False 0 0
  where
    p :: (Word32, Word32) -> Fence
    p = getFence fences

    go :: Bool -> Word32 -> Int -> Int
    go inside row accum
      | row == fencesRows fences = accum
      | inside && p (row, col) == FVert = go True (row + 1) accum
      | not inside && p (row, col) == FVert = go True (row + 1) (accum + 1)
      | inside
          && p (row, col) == FCorner
          && (p (row, col + 1) == FHoriz || p (row, col - 1) == FHoriz) =
          go False (row + 1) accum
      | otherwise = go inside (row + 1) accum

regionToFences :: Region -> Fences
regionToFences (Region _ mask) =
  let amask = asplode mask
   in Fences $
        Grid.generate (Grid.getRows amask) (Grid.getCols amask) $
          classifyFence amask

classifyFence :: Grid.BoolGrid -> (Word32, Word32) -> Fence
classifyFence mask (r, c) =
  let p rc = fromMaybe False (Grid.getElem mask rc)
      fvert = xor (p (r, c - 1)) (p (r, c + 1))
      fhorz = xor (p (r - 1, c)) (p (r + 1, c))
      fcorn_count =
        length
          $ filter
            id
          $ p
            <$> [ (r - 1, c - 1),
                  (r - 1, c + 1),
                  (r + 1, c - 1),
                  (r + 1, c + 1)
                ]
      fcorn = fcorn_count >= 1 && fcorn_count < 4
   in if fhorz
        then FHoriz
        else
          if fvert
            then FVert
            else
              if fcorn
                then FCorner
                else FNone

xor :: Bool -> Bool -> Bool
xor a b = (a || b) && not (a && b)

-- | Expand a mask grid, placing spaces between original elements.
asplode :: Grid.BoolGrid -> Grid.BoolGrid
asplode mask =
  let new_rows = 1 + Grid.getRows mask * 2
      new_cols = 1 + Grid.getCols mask * 2
   in Grid.generate new_rows new_cols $
        \(r, c) ->
          not (even r || even c)
            && fromMaybe False (Grid.getElem mask ((r - 1) `div` 2, (c - 1) `div` 2))

---- Part 1 -------------------------------------------------------------------

part1 :: Plot -> Int
part1 = sum . fmap regionPrice . regions

-- | A region.
--
-- A region contains:
--    - The plant id.
--    - A mask grid which indicates the plot belonging to the region.
data Region = Region !Plant !Grid.BoolGrid deriving (Eq, Show)

{-
regionRows :: Region -> Word32
regionRows (Region _ mask) = Grid.getRows mask

regionCols :: Region -> Word32
regionCols (Region _ mask) = Grid.getCols mask

-- | Check if an element of a region is set.
--
-- Points outside the region are valid input and return `False`.
inRegion :: Region -> (Word32, Word32) -> Bool
inRegion (Region _ mask) = fromMaybe False . Grid.getElem mask

-- | Coordinates of a region in row-major order.
regionRowMajor :: Region -> [(Word32, Word32)]
regionRowMajor (Region _ mask) = Grid.rowMajorCoords mask

-- | Render a `Region` as a pretty string.
prettyRegion :: Bool -> Region -> String
prettyRegion printStat region@(Region plant mask) =
  ("Region: Plant = " <> [unPlant plant] <> "\n")
    <> Grid.prettyBoolGrid mask
    <> if not printStat
      then ""
      else
        "\n"
          <> ("Area     = " <> show (regionArea region) <> "\n")
          <> ("Boundary = " <> show (regionBoundary region) <> "\n")
-}

-- | Find all the distinct regions in a plot.
regions :: Plot -> [Region]
regions (Plot plot) =
  reverse $
    go (Grid.emptyBoolGridMatchingSize plot) [] (Grid.rowMajorCoords plot)
  where
    go :: Grid.BoolGrid -> [Region] -> [(Word32, Word32)] -> [Region]
    go _ accum [] = accum
    go occupied accum (rc : rcs) =
      if Grid.getElemUnsafe occupied rc
        then go occupied accum rcs
        else
          let plant = Grid.getElemUnsafe plot rc
              mask = Grid.floodFill rc plot
              occupied' = Grid.boolGridOrUnsafe occupied mask
              region = Region plant mask
              accum' = region : accum
           in go occupied' accum' rcs

-- | Area of a region.
regionArea :: Region -> Int
regionArea (Region _ mask) = Grid.countBoolGrid mask

-- | Boundary of a region.
regionBoundary :: Region -> Int
regionBoundary (Region _ mask) =
  let countBorderFor :: (Word32, Word32) -> Int
      countBorderFor = length . filter (isBorderByAdj mask) . borderStencil
   in sum $
        fmap countBorderFor $
          filter (Grid.getElemUnsafe mask) $
            Grid.rowMajorCoords mask

borderStencil :: (Word32, Word32) -> [(Word32, Word32)]
borderStencil (r, c) =
  [ (r - 1, c),
    (r + 1, c),
    (r, c - 1),
    (r, c + 1)
  ]

isBorderByAdj :: Grid.BoolGrid -> (Word32, Word32) -> Bool
isBorderByAdj mask rc =
  case Grid.getElem mask rc of
    Nothing -> True
    Just True -> False
    Just False -> True

-- | Price of a region.
regionPrice :: Region -> Int
regionPrice region = regionArea region * regionBoundary region

---- Types --------------------------------------------------------------------

-- | A single plant in the plot.
newtype Plant = Plant Char deriving (Eq, Show)

-- | The whole plot.
newtype Plot = Plot (Grid V.Vector Plant) deriving (Eq, Show)

{-
-- | Unwrap a `Plant`.
unPlant :: Plant -> Char
unPlant (Plant p) = p

-- | Unwrap a `Plot`.
unPlot :: Plot -> Grid V.Vector Plant
unPlot (Plot p) = p

-- | Render a `Plot` as a pretty string.
prettyPlot :: Plot -> String
prettyPlot (Plot plants) =
  unlines $ prettyRow <$> Grid.toLists plants
  where
    prettyRow :: [Plant] -> String
    prettyRow = fmap prettyPlant

    prettyPlant :: Plant -> Char
    prettyPlant (Plant c) = c
-}

---- Parsing ------------------------------------------------------------------

type Parser = Parsec Void Text

inputFile :: FilePath
inputFile = "../inputs/input-day-12.txt"

readInput :: IO Plot
readInput = do
  txt <- readFile inputFile
  let parseResult = parse parseDocument inputFile txt
  case parseResult of
    Left errorBundle -> do
      putStrLn "ERROR: Could not parse input file."
      putStrLn $ errorBundlePretty errorBundle
      exitFailure
    Right input -> pure input

parseDocument :: Parser Plot
parseDocument =
  Plot <$> Grid.parseGridDenseNL parsePlant <* optional newline <* eof

parsePlant :: Parser Plant
parsePlant = Plant <$> upperChar
