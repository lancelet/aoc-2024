{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Grid
  ( Grid,
    BoolGrid,
    filledWith,
    fromList,
    fromLists,
    generate,
    toList,
    toLists,
    toImage,
    getRows,
    getCols,
    rowMajorCoords,
    getElem,
    getElemUnsafe,
    locations,
    setMulti,
    countEqual,
    parseGridDenseNL,
    thaw,
    freeze,
    read,
    readMaybe,
    write,
    prettyBoolGrid,
    emptyBoolGrid,
    emptyBoolGridMatchingSize,
    boolGridOr,
    boolGridOrUnsafe,
    countBoolGrid,
    floodFill,
    expandGridBy,
  )
where

import Codec.Picture (Image, Pixel, generateImage)
import Control.Exception (assert)
import Control.Monad (filterM)
import Control.Monad.ST (runST)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import Data.Vector.Mutable (PrimMonad, PrimState)
import qualified Data.Vector.Unboxed as VU
import Data.Word (Word32)
import Text.Megaparsec (Parsec, Stream, Token, some)
import Text.Megaparsec.Char (newline)
import Prelude hiding (read)

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
                then
                  let xs = concat xxs
                   in assert
                        (length xs == nRows * nCols)
                        (fromList (intToW32 nRows) (intToW32 nCols) xs)
                else Nothing

-- | Generate a grid from a generator function.
generate ::
  forall v a.
  (VG.Vector v a) =>
  -- Rows.
  Word32 ->
  -- Colums.
  Word32 ->
  -- Generator function.
  ((Word32, Word32) -> a) ->
  -- Generated grid.
  Grid v a
generate nRows nCols f =
  fromMaybe (error "Dimensions should be correct") $
    Grid.fromList nRows nCols $
      [ f (r, c)
        | r <- [0 .. nRows - 1],
          c <- [0 .. nCols - 1]
      ]

-- | Convert a grid to a list of lists.
toLists :: (VG.Vector v a) => Grid v a -> [[a]]
toLists grid =
  [ [getElemUnsafe grid (r, c) | c <- [0 .. getCols grid - 1]]
    | r <- [0 .. getRows grid - 1]
  ]

-- | Convert a grid to a row-major list of elements.
toList :: (VG.Vector v a) => Grid v a -> [a]
toList grid = VG.toList (cells grid)

-- | Convert a grid to an image.
toImage ::
  forall v a px.
  (VG.Vector v a, Pixel px) =>
  (a -> px) ->
  Grid v a ->
  Image px
toImage f grid =
  generateImage genf (w32ToInt $ getCols grid) (w32ToInt $ getRows grid)
  where
    genf :: Int -> Int -> px
    genf x y =
      let r = intToW32 y
          c = intToW32 x
       in f $ Grid.getElemUnsafe grid (r, c)

-- | Return the number of rows in a grid.
getRows :: Grid v a -> Word32
getRows = rows

-- | Return the number of columns in a grid.
getCols :: Grid v a -> Word32
getCols = cols

-- | List of all coordinates in the grid, in row major form.
rowMajorCoords :: Grid v a -> [(Word32, Word32)]
rowMajorCoords grid =
  [(r, c) | r <- [0 .. getRows grid - 1], c <- [0 .. getCols grid - 1]]

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

{-
-- | Convert a linear index into a plain index.
--
-- This throws an error if the index is out of range.
unlindexUnsafe :: Grid v a -> Int -> (Word32, Word32)
unlindexUnsafe grid lx =
  case unlindex grid lx of
    Nothing -> error "Grid.unlindexUnsafe: index out of bounds"
    Just rc -> rc

-- | Convert a linear index into a plain index.
unlindex :: Grid v a -> Int -> Maybe (Word32, Word32)
unlindex grid lx =
  let iRows = w32ToInt $ getRows grid
      iCols = w32ToInt $ getCols grid
      iMax = iRows * iCols
      iRow = lx `div` iCols
      iCol = lx `mod` iCols
      row = intToW32 iRow
      col = intToW32 iCol
   in if lx >= iMax
        then Nothing
        else Just (row, col)
-}

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

-- | Read an element from a mutable grid; throwing an exception if
--   out of bounds
read ::
  (PrimMonad m, VG.Vector v a) =>
  Grid (VG.Mutable v (PrimState m)) a ->
  (Word32, Word32) ->
  m a
read grid rc = do
  mval <- readMaybe grid rc
  case mval of
    Nothing -> error "Grid.read: coordinates are out of bounds"
    Just x -> pure x

-- | Read an element from a mutable grid.
readMaybe ::
  (PrimMonad m, VG.Vector v a) =>
  Grid (VG.Mutable v (PrimState m)) a ->
  (Word32, Word32) ->
  m (Maybe a)
readMaybe grid rc
  | not (coordInRange grid rc) = pure Nothing
  | otherwise = Just <$> VGM.read (cells grid) (lindex grid rc)

-- | Write an element into a mutable grid.
write ::
  (PrimMonad m, VG.Vector v a) =>
  Grid (VG.Mutable v (PrimState m)) a ->
  (Word32, Word32) ->
  a ->
  m ()
write grid rc value
  | not (coordInRange grid rc) = error $ "Grid.write: Index out of bounds!"
  | otherwise = VGM.write (cells grid) (lindex grid rc) value

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

-- | Grid containing boolean values.
type BoolGrid = Grid VU.Vector Bool

-- | Mutable boolean grid.
type MutBoolGrid m = Grid (VG.Mutable VU.Vector (PrimState m)) Bool

-- | Pretty-print a boolean mask grid.
prettyBoolGrid :: BoolGrid -> String
prettyBoolGrid grid = unlines $ prettyRow <$> toLists grid
  where
    prettyRow = fmap prettyCol
    prettyCol x = if x then 'T' else '.'

-- | Empty boolean grid.
emptyBoolGrid :: Word32 -> Word32 -> BoolGrid
emptyBoolGrid n_rows n_cols = filledWith n_rows n_cols False

-- | Return the count of `True` inside a `BoolGrid`.
countBoolGrid :: BoolGrid -> Int
countBoolGrid = length . filter id . VG.toList . cells

-- | Create an empty bool grid of a size matching an existing grid.
emptyBoolGridMatchingSize :: Grid v a -> BoolGrid
emptyBoolGridMatchingSize grid =
  let n_rows = getRows grid
      n_cols = getCols grid
   in emptyBoolGrid n_rows n_cols

-- | Or two boolean grids together, throwing an exception if their sizes
--   differ.
boolGridOrUnsafe :: BoolGrid -> BoolGrid -> BoolGrid
boolGridOrUnsafe gridA gridB =
  case boolGridOr gridA gridB of
    Just bg -> bg
    Nothing -> error "Grid.boolGridOrUnsafe: grid sizes differed"

-- | Or two boolean grids together if they are the same size.
boolGridOr :: BoolGrid -> BoolGrid -> Maybe BoolGrid
boolGridOr = boolGridOp (||)

-- | Combine two boolean grids using an operation.
boolGridOp :: (Bool -> Bool -> Bool) -> BoolGrid -> BoolGrid -> Maybe BoolGrid
boolGridOp op gridA gridB =
  let n_rows = getRows gridA
      n_cols = getCols gridA
      sz_ok = (n_rows == getRows gridB) && (n_cols == getCols gridB)
   in if sz_ok
        then
          Just $
            Grid n_rows n_cols $
              VG.zipWith op (cells gridA) (cells gridB)
        else Nothing

floodFill :: (VG.Vector v a, Eq a) => (Word32, Word32) -> Grid v a -> BoolGrid
floodFill (row, col) grid =
  let mask = emptyBoolGridMatchingSize grid
   in case getElem grid (row, col) of
        Nothing -> mask
        Just value -> runST $ do
          mask' <- thaw mask
          floodFillMut value [(row, col)] grid mask'
          freeze mask'

floodFillMut ::
  (PrimMonad m, VG.Vector v a, Eq a) =>
  a ->
  [(Word32, Word32)] ->
  Grid v a ->
  MutBoolGrid m ->
  m ()
floodFillMut _ [] _ _ = pure ()
floodFillMut value ((row, col) : xs) grid mask = do
  -- set the current pixel in the mask image
  write mask (row, col) True
  -- find the next pixels to set; they must be in-range and not already set
  -- in the mask
  dp_next <-
    filterM (fmap not . read mask) $
      filter (\rc -> getElemUnsafe grid rc == value) $
        filter
          (coordInRange grid)
          [(row - 1, col), (row + 1, col), (row, col - 1), (row, col + 1)]
  -- loop
  floodFillMut value (dp_next <> xs) grid mask

-- | Expand a grid by a given number of cells equally on all sides.
expandGridBy ::
  forall v a.
  (VG.Vector v a) =>
  -- | Number of cells to expand by.
  Word32 ->
  -- | Value to put in the new cells.
  a ->
  -- | Old grid.
  Grid v a ->
  -- | New grid.
  Grid v a
expandGridBy n value grid =
  let n_rows, n_cols :: Word32
      n_rows = n + n + getRows grid
      n_cols = n + n + getCols grid

      n_rows_i, n_cols_i, n_cells :: Int
      n_rows_i = w32ToInt n_rows
      n_cols_i = w32ToInt n_cols
      n_cells = n_rows_i * n_cols_i

      cells' :: v a
      cells' = VG.generate n_cells f

      f :: Int -> a
      f i =
        let row_out = i `div` n_cols_i
            col_out = i `mod` n_cols_i

            row_in = row_out - w32ToInt n
            col_in = col_out - w32ToInt n

            row_in_w = intToW32 row_in
            col_in_w = intToW32 col_in
            rc_in = (row_in_w, col_in_w)
         in fromMaybe value $ getElem grid rc_in
   in Grid n_rows n_cols cells'