{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module P20 where

import AStar (Mode (ModeSingle), Result (NoPathFound, PathsFound), astar, pathNodes)
import Control.Applicative ((<|>))
import Control.Monad (forM_)
import Control.Monad.ST (runST)
import Data.Function ((&))
import Data.Functor (($>), (<&>))
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text.IO (readFile)
import qualified Data.Text.IO as T
import Data.Void (Void)
import Data.Word (Word32)
import Grid (BoolGrid, CharGrid)
import qualified Grid
import System.Exit (exitFailure)
import Text.Megaparsec (Parsec, errorBundlePretty, parse, some)
import Text.Megaparsec.Char (char, eol)
import Prelude hiding (readFile)

main :: IO ()
main = do
  putStrLn "Day 20"
  -- mainEx1
  mainPart1

mainPart1 :: IO ()
mainPart1 = do
  putStrLn "Part 1"
  let filePath = "../inputs/input-day-20.txt"
  track <- loadFile filePath
  let nCheats100 = part1 track
  putStrLn $ "Number of cheats saving >=100 ps: " <> show nCheats100

mainEx1 :: IO ()
mainEx1 = do
  putStrLn "Example 1"
  let filePath = "../inputs/input-day-20-example.txt"
  track <- loadFile filePath
  let path = fromMaybe (error "No path!") $ findPath track Nothing
  let cgp = addPathToCharGrid path . trackToCharGrid $ track
  let pathLen = length path
  T.putStrLn . Grid.prettyCharGrid $ cgp
  putStrLn $ "Path length: " <> show pathLen

---- Processing ---------------------------------------------------------------

part1 :: Track -> Int
part1 track =
  let normal_len = length $ findPath' track Nothing
      cheat_gain rc = normal_len - length (findPath' track (Just rc))
   in length $ filter (>= 100) (cheat_gain <$> allCheatCandidates track)

-- | Find all the coordinates that are cheat candidates.
allCheatCandidates :: Track -> [RC]
allCheatCandidates track =
  filter (isCheatCandidate track) $ Grid.rowMajorCoords track.grid

-- | Check if a square is a cheat candidate.
isCheatCandidate :: Track -> RC -> Bool
isCheatCandidate track rc@(r, c) =
  isWall track rc
    && ( (isOpen track (r, c - 1) && isOpen track (r, c + 1))
           || (isOpen track (r - 1, c) && isOpen track (r + 1, c))
       )

-- | Find a path through the track from start to end.
--
--   Errors if there is no path.
findPath' :: Track -> Maybe RC -> [RC]
findPath' track cheat_candidate =
  fromMaybe (error "No path found!") $ findPath track cheat_candidate

-- | Find a path through the track from start to end.
findPath :: Track -> Maybe RC -> Maybe [RC]
findPath track cheat_candidate =
  astar
    ModeSingle
    track.start
    (track.end ==)
    (successor track cheat_candidate)
    (const 1)
    (manhattanRC track.end)
    & \case
      NoPathFound -> Nothing
      PathsFound ps -> Just (pathNodes (head ps))

-- | Action for A*. These are all the moves possible for a given point.
data Action = AUp | ADown | ALeft | ARight deriving (Eq, Show)

-- | The Manhattan distance between a pair of RC coordinates.
manhattanRC :: RC -> RC -> Int
manhattanRC (r1, c1) (r2, c2) =
  let f = fromIntegral
   in abs (f r2 - f r1) + abs (f c2 - f c1)

-- | Successor function for A*.
successor :: Track -> Maybe RC -> RC -> [(Action, RC)]
successor track cheat_candidate (r, c) = mapMaybe f [AUp, ADown, ALeft, ARight]
  where
    f :: Action -> Maybe (Action, RC)
    f a =
      let rc' = g a
       in if isWall track rc' && cheat_candidate /= Just rc'
            then Nothing
            else Just (a, rc')

    g :: Action -> RC
    g = \case
      AUp -> (r - 1, c)
      ADown -> (r + 1, c)
      ALeft -> (r, c - 1)
      ARight -> (r, c + 1)

---- Data Types ---------------------------------------------------------------

-- | (Row, Col) pair.
type RC = (Word32, Word32)

-- | Representation of the race track.
data Track = Track
  { -- | Grid of cells. `True` is a wall, `False` is open space.
    grid :: !BoolGrid,
    -- | Start coordinate.
    start :: !RC,
    -- | End coordinate.
    end :: !RC
  }
  deriving (Eq, Show)

-- | Convert a track to a character grid.
trackToCharGrid :: Track -> CharGrid
trackToCharGrid track = runST $ do
  let bf x = if x then '#' else '.'
  cgrid <- Grid.thaw $ Grid.toCharGrid bf track.grid
  Grid.write cgrid track.start 'S'
  Grid.write cgrid track.end 'E'
  Grid.freeze cgrid

-- | Add a path to a character grid.
addPathToCharGrid :: [RC] -> CharGrid -> CharGrid
addPathToCharGrid rcs cg = runST $ do
  cgrid <- Grid.thaw cg
  forM_ rcs $ \rc -> Grid.write cgrid rc 'O'
  Grid.freeze cgrid

-- | Check if a coordinate in the grid is a wall.
isWall :: Track -> RC -> Bool
isWall track rc = fromMaybe True $ Grid.getElem track.grid rc

-- | Check if a coordinate on the grid is open (ie. not a wall).
isOpen :: Track -> RC -> Bool
isOpen track = not . isWall track

---- Parsing ------------------------------------------------------------------

loadFile :: FilePath -> IO Track
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

parseDocument :: Parser Track
parseDocument = do
  rl <- parseRowList
  let start = fromMaybe (error "No start position defined.") rl.opt_start
  let end = fromMaybe (error "No end position defined.") rl.opt_end
  let grid = fromMaybe (error "Could not parse grid.") $ Grid.fromLists rl.rows
  pure $ Track {grid = grid, start = start, end = end}

parseRowList :: Parser RowList
parseRowList =
  some (parseRow <* eol)
    & fmap (foldr f emptyRowList . zip [0 ..])
  where
    f :: (Word32, Row) -> RowList -> RowList
    f (row_idx, row) rl =
      let rl' = prependRowListRow row rl
          opt_start_r = row.opt_start <&> (row_idx,)
          opt_end_r = row.opt_end <&> (row_idx,)
          opt_start' = disallowDup rl.opt_start opt_start_r
          opt_end' = disallowDup rl.opt_end opt_end_r
       in rl' {opt_start = opt_start', opt_end = opt_end'}

    disallowDup :: Maybe a -> Maybe a -> Maybe a
    disallowDup old new =
      case (old, new) of
        (Nothing, Nothing) -> Nothing
        (Nothing, Just x) -> Just x
        (Just x, Nothing) -> Just x
        (Just _, Just _) -> error "Already defined!"

parseRow :: Parser Row
parseRow = fmap (foldr f emptyRow . zip [0 ..]) parseCellRow
  where
    f :: (Word32, Cell) -> Row -> Row
    f (col, c) row =
      case c of
        CellWall -> prependBlock True row
        CellTrack -> prependBlock False row
        CellStart -> setRowStart col row
        CellEnd -> setRowEnd col row

parseCellRow :: Parser [Cell]
parseCellRow = some parseCell

parseCell :: Parser Cell
parseCell =
  (char '#' $> CellWall)
    <|> (char '.' $> CellTrack)
    <|> (char 'S' $> CellStart)
    <|> (char 'E' $> CellEnd)

-- | List of rows.
data RowList = RowList
  { rows :: [[Bool]],
    opt_start :: Maybe RC,
    opt_end :: Maybe RC
  }

-- | An empty row list.
emptyRowList :: RowList
emptyRowList = RowList {rows = [], opt_start = Nothing, opt_end = Nothing}

-- | Reverse the list of rows in a `RowList`.
reverseRowListRows :: RowList -> RowList
reverseRowListRows rl = rl {rows = reverse rl.rows}

-- | Prepend a row to a row list.
prependRowListRow :: Row -> RowList -> RowList
prependRowListRow r rl = rl {rows = r.walls : rl.rows}

-- | A parsed-in row.
data Row = Row
  { walls :: [Bool],
    opt_start :: Maybe Word32,
    opt_end :: Maybe Word32
  }
  deriving (Eq, Show)

-- \ Empty row.
emptyRow :: Row
emptyRow = Row {walls = [], opt_start = Nothing, opt_end = Nothing}

-- | Reverse the walls in a row.
reverseRowWalls :: Row -> Row
reverseRowWalls row = row {walls = reverse row.walls}

-- | Prepend a wall or not-wall to a row.
prependBlock :: Bool -> Row -> Row
prependBlock is_wall row = row {walls = is_wall : row.walls}

-- | Set a start coordinate in a row.
setRowStart :: Word32 -> Row -> Row
setRowStart i row =
  case row.opt_start of
    Just _ -> error "Start is already defined."
    Nothing -> prependBlock False $ row {opt_start = Just i}

-- | Set an end coordinate in a row.
setRowEnd :: Word32 -> Row -> Row
setRowEnd i row =
  case row.opt_end of
    Just _ -> error "End is already defined."
    Nothing -> prependBlock False $ row {opt_end = Just i}

-- | Cell in the track.
data Cell = CellWall | CellTrack | CellStart | CellEnd deriving (Eq, Show)
