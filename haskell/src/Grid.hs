{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Grid
  ( Grid,
    filledWith,
    fromList,
    fromLists,
    getRows,
    getCols,
    getElem,
    locations,
    setMulti,
    countEqual,
    parseGridDenseNL,
    thaw,
    freeze,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Vector.Generic as VG
import Data.Vector.Mutable (PrimMonad, PrimState)
import Data.Word (Word32)
import Text.Megaparsec (Parsec, Stream, Token, some)
import Text.Megaparsec.Char (newline)

-- | A grid of values.
data Grid v a = Grid
  { rows :: !Word32,
    cols :: !Word32,
    cells :: !(v a)
  }
  deriving (Eq, Show)

-- | Create a grid filled with a value.
filledWith ::
  forall v a.
  (VG.Vector v a) =>
  -- | Number of rows.
  Word32 ->
  -- | Number of columns.
  Word32 ->
  -- | Value to fill the grid with.
  a ->
  -- | Grid.
  Grid v a
filledWith nRows nCols value =
  let nElems = nRows * nCols
      gCells = VG.replicate (w32ToInt nElems) value
   in Grid {rows = nRows, cols = nCols, cells = gCells}

-- | Convert a list to a grid of values.
fromList ::
  forall v a.
  (VG.Vector v a) =>
  -- | Number of rows.
  Word32 ->
  -- | Number of columns.
  Word32 ->
  -- | List of elements.
  [a] ->
  -- | Grid (provided the number of elements matches.)
  Maybe (Grid v a)
fromList nRows nCols values =
  let nElem = nRows * nCols
      gCells = VG.fromList values
      nCells = intToW32 $ VG.length gCells
   in if nElem == nCells
        then
          Just
            ( Grid
                { rows = nRows,
                  cols = nCols,
                  cells = gCells
                }
            )
        else Nothing

-- | Construct a grid from a list of lists; provided the lengths of all the
--   sub-lists are the same.
--
-- The elements of the list are rows, and the elements of those elements are
-- the column values.
fromLists :: forall v a. (VG.Vector v a) => [[a]] -> Maybe (Grid v a)
fromLists xxs =
  let nRows = length xxs
   in if nRows == 0
        then Nothing
        else
          let nCols = length . head $ xxs
              allColsMatch = all ((nCols ==) . length) xxs
           in if allColsMatch
                then fromList (intToW32 nRows) (intToW32 nCols) (concat xxs)
                else Nothing

-- | Return the number of rows in a grid.
getRows :: Grid v a -> Word32
getRows = rows

-- | Return the number of columns in a grid.
getCols :: Grid v a -> Word32
getCols = cols

-- | Get an element from the grid, if it is in range.
getElem :: (VG.Vector v a) => Grid v a -> (Word32, Word32) -> Maybe a
getElem grid rc
  | coordInRange grid rc = Just $ getElemUnsafe grid rc
  | otherwise = Nothing

-- \ Get an element from a grid without a bounds check.
getElemUnsafe :: (VG.Vector v a) => Grid v a -> (Word32, Word32) -> a
getElemUnsafe grid rc = cells grid VG.! lindex grid rc

-- | Linear index into a grid.
lindex :: Grid v a -> (Word32, Word32) -> Int
lindex grid (row, col) =
  let iRow = w32ToInt row
      iCol = w32ToInt col
      iCols = w32ToInt $ getCols grid
   in iRow * iCols + iCol

-- | Check if a grid coordinate is in range.
coordInRange :: Grid v a -> (Word32, Word32) -> Bool
coordInRange grid (row, col) = rowInRange grid row && colInRange grid col

-- | Check if a row coordinate is in range.
rowInRange :: Grid v a -> Word32 -> Bool
rowInRange grid row = row >= 0 && row < getRows grid

-- | Check if a column coordinate is in range.
colInRange :: Grid v a -> Word32 -> Bool
colInRange grid col = col >= 0 && col < getCols grid

-- | Locations of unique items in the grid.
locations ::
  forall v a.
  (VG.Vector v a, Ord a) =>
  Grid v a ->
  Map a [(Word32, Word32)]
locations grid =
  let items :: [(a, [(Word32, Word32)])]
      items =
        [ (getElemUnsafe grid (row, col), [(row, col)])
          | row <- [0 .. getRows grid - 1],
            col <- [0 .. getCols grid - 1]
        ]
   in Map.fromListWith (<>) items

-- | Thaw a grid into a mutable grid.
thaw ::
  (PrimMonad m, VG.Vector v a) =>
  Grid v a ->
  m (Grid (VG.Mutable v (PrimState m)) a)
thaw grid = do
  gCells <- VG.thaw (cells grid)
  pure $
    Grid
      { rows = getRows grid,
        cols = getCols grid,
        cells = gCells
      }

-- | Freeze a mutable grid into an immutable one.
freeze ::
  (PrimMonad m, VG.Vector v a) =>
  Grid (VG.Mutable v (PrimState m)) a ->
  m (Grid v a)
freeze grid = do
  gCells <- VG.freeze (cells grid)
  pure $
    Grid
      { rows = getRows grid,
        cols = getCols grid,
        cells = gCells
      }

-- | Set multiple elements of a grid to a given value.
setMulti ::
  forall v a.
  (VG.Vector v a) =>
  -- | Value to set in all elements.
  a ->
  -- | Locations to set.
  [(Word32, Word32)] ->
  -- | Input grid.
  Grid v a ->
  -- | Result grid.
  Grid v a
setMulti value locs grid =
  let lxs :: [Int]
      lxs = lindex grid <$> filter (coordInRange grid) locs

      vs :: [(Int, a)]
      vs = (\i -> (i, value)) <$> lxs
   in Grid
        { rows = getRows grid,
          cols = getCols grid,
          cells = cells grid VG.// vs
        }

-- | Count elements of a grid that are equal to a value.
countEqual ::
  forall v a.
  (VG.Vector v a, Eq a) =>
  a ->
  Grid v a ->
  Int
countEqual value grid =
  length $ filter (value ==) $ VG.toList $ cells grid

-- | Convert a Word32 to an Int.
w32ToInt :: Word32 -> Int
w32ToInt = fromIntegral

-- | Convert an Int to a Word32.
intToW32 :: Int -> Word32
intToW32 = fromIntegral

-- | Parse a grid in dense format, separated by newlines.
parseGridDenseNL ::
  forall e s v a.
  (VG.Vector v a, Ord e, Stream s, Token s ~ Char) =>
  -- | Parse an item from the grid, of type `a`.
  Parsec e s a ->
  -- | Parse the grid.
  Parsec e s (Grid v a)
parseGridDenseNL parseItem = do
  let parseRow = some parseItem <* newline
  parsedRows <- some parseRow
  case fromLists parsedRows of
    Nothing -> fail $ "Could not convert rows to Grid."
    Just grid -> pure grid