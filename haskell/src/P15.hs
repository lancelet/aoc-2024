{-# LANGUAGE ScopedTypeVariables #-}

module P15 where

import Control.Monad (when)
import Control.Monad.ST (ST, runST)
import Data.Functor (($>))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (mapMaybe)
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Data.Text (Text)
import Data.Text.IO (readFile)
import qualified Data.Vector as V
import Data.Void (Void)
import Data.Word (Word32)
import Grid (Grid)
import qualified Grid
import System.Exit (exitFailure)
import Text.Megaparsec (Parsec, eof, errorBundlePretty, optional, parse, some, (<|>))
import Text.Megaparsec.Char (char, newline)
import Prelude hiding (readFile)

main :: IO ()
main = do
  putStrLn "Day 15"
  -- mainEx1
  -- mainEx2
  part1

part1 :: IO ()
part1 = do
  putStrLn "Part 1"
  let inputFile = "../inputs/input-day-15.txt"

  document <- loadFile inputFile
  let s0 = initState (docMap document)
  let m0 = docMoves document
  let sf = executeAllMoves m0 s0
  let gs = sumGPSCoords (stateMap sf)
  putStrLn $ "Sum of GPS coordinates: " <> show gs

mainEx1 :: IO ()
mainEx1 = do
  putStrLn "Example 1"
  let inputFile = "../inputs/input-day-15-example-1.txt"

  document <- loadFile inputFile
  let s0 = initState (docMap document)
  let m0 = docMoves document
  let sf = executeAllMoves m0 s0

  putStrLn "Final State: "
  putStrLn . prettyState $ sf

  let gs = sumGPSCoords (stateMap sf)
  putStrLn $ "Sum of GPS coordinates: " <> show gs

mainEx2 :: IO ()
mainEx2 = do
  putStrLn "Example 2"
  let inputFile = "../inputs/input-day-15-example-2.txt"

  document <- loadFile inputFile
  let s0 = initState (docMap document)
  let m0 = docMoves document

  ref_state <- newIORef s0
  ref_moves <- newIORef m0

  loopBoolIO $ do
    state <- readIORef ref_state
    moves <- readIORef ref_moves

    putStrLn $ prettyState state

    case popMove moves of
      Nothing -> do
        let gs = sumGPSCoords (stateMap state)
        putStrLn $ "Sum of GPS coordinates: " <> show gs
        pure False
      Just (move, moves') -> do
        putStrLn $ "Move: " <> [prettyMove move]
        let state' = executeMove move state
        writeIORef ref_state state'
        writeIORef ref_moves moves'
        pure True

loopBoolIO :: IO Bool -> IO ()
loopBoolIO action = do
  result <- action
  when result $ loopBoolIO action

---- Types --------------------------------------------------------------------

-- | 2D vector; for robot position.
--
-- This is (x, y) not (row, col).
data V2 = V2 !Int !Int deriving (Eq, Show)

-- | Add two V2 vectors.
v2Add :: V2 -> V2 -> V2
v2Add (V2 x1 y1) (V2 x2 y2) = V2 (x1 + x2) (y1 + y2)

-- | Convert a V2 coordinate to a grid (row, col) coordinate.
v2ToGridRC :: V2 -> (Word32, Word32)
v2ToGridRC (V2 x y) = (fromIntegral y, fromIntegral x)

-- | Convert a grid (row, col) to a V2.
gridRCToV2 :: (Word32, Word32) -> V2
gridRCToV2 (r, c) = V2 (fromIntegral c) (fromIntegral r)

-- | Document containing an initial map and robot moves.
data Document = Document
  { docMap :: !InitMap,
    docMoves :: !Moves
  }
  deriving (Eq, Show)

-- | Room map. This is the same as the "initial map", but excludes the robot
--   position.
newtype RoomMap = RoomMap (Grid V.Vector Cell) deriving (Eq, Show)

-- | Initial map read in from a file. This is the same as the "room map", but
--   includes the robot position.
newtype InitMap = InitMap (Grid V.Vector InitCell) deriving (Eq, Show)

-- | List of moves for the robot.
newtype Moves = Moves [Move] deriving (Eq, Show)

-- | Take the top move off the list.
popMove :: Moves -> Maybe (Move, Moves)
popMove (Moves []) = Nothing
popMove (Moves (x : xs)) = Just (x, Moves xs)

-- | A single attempted move of the robot.
data Move
  = MoveUp
  | MoveDown
  | MoveLeft
  | MoveRight
  deriving (Eq, Show)

-- | Convert a move to an increment.
moveToInc :: Move -> V2
moveToInc move =
  case move of
    MoveUp -> V2 0 (-1)
    MoveDown -> V2 0 1
    MoveLeft -> V2 (-1) 0
    MoveRight -> V2 1 0

-- | Cell in the map being processed (excludes the robot).
data Cell
  = CWall
  | CBox
  | CEmpty
  deriving (Eq, Show)

-- | Initial cell types for the map that is first read in.
data InitCell
  = ICWall
  | ICBox
  | ICRobot
  | ICEmpty
  deriving (Eq, Show)

-- | State, including the map and the robot's position.
data State = State
  { stateMap :: !RoomMap,
    stateRobot :: !V2
  }
  deriving (Eq, Show)

-- | Pretty-print the state.
prettyState :: State -> String
prettyState (State (RoomMap grid) xy) =
  let cellsCoords :: [((Word32, Word32), Cell)]
      cellsCoords = zip (Grid.rowMajorCoords grid) (Grid.toList grid)

      icells :: [[InitCell]]
      icells =
        chunksOf
          (fromIntegral (Grid.getCols grid))
          ( ( \((r, c), cell) ->
                if V2 (fromIntegral c) (fromIntegral r) == xy
                  then ICRobot
                  else case cell of
                    CBox -> ICBox
                    CEmpty -> ICEmpty
                    CWall -> ICWall
            )
              <$> cellsCoords
          )

      prettyRow :: [InitCell] -> String
      prettyRow = fmap prettyCell

      prettyCell :: InitCell -> Char
      prettyCell cell =
        case cell of
          ICBox -> 'O'
          ICEmpty -> '.'
          ICWall -> '#'
          ICRobot -> '@'
   in unlines $ prettyRow <$> icells

prettyMoves :: Moves -> String
prettyMoves (Moves moveList) = prettyMove <$> moveList

prettyMove :: Move -> Char
prettyMove move =
  case move of
    MoveDown -> 'v'
    MoveUp -> '^'
    MoveLeft -> '<'
    MoveRight -> '>'

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = go n []
  where
    go :: Int -> [a] -> [a] -> [[a]]
    go _ accum [] = [reverse accum]
    go 0 accum xs = reverse accum : go n [] xs
    go i accum (x : xs) = go (i - 1) (x : accum) xs

---- Processing ---------------------------------------------------------------

-- | Convert the initial map to the initial state.
--
-- This involves extracting the robot's position.
initState :: InitMap -> State
initState (InitMap igrid) =
  let go :: Maybe V2 -> [Cell] -> [(Word32, Word32)] -> ([Cell], V2)
      go Nothing _ [] = error "No robot position found!"
      go (Just rp) accum [] = (reverse accum, rp)
      go Nothing accum (rc : rcs)
        | Grid.getElemUnsafe igrid rc == ICRobot =
            let (r, c) = rc
                x = fromIntegral c
                y = fromIntegral r
                v = V2 x y
             in go (Just v) (CEmpty : accum) rcs
      go (Just _) _ (rc : _)
        | Grid.getElemUnsafe igrid rc == ICRobot =
            error "Duplicate robots encountered!"
      go rp accum (rc : rcs) =
        let e = Grid.getElemUnsafe igrid rc
            e' = case e of
              ICEmpty -> CEmpty
              ICBox -> CBox
              ICWall -> CWall
              ICRobot -> error "Robot should be handled elsewhere!"
         in go rp (e' : accum) rcs

      (cells', rp') = go Nothing [] (Grid.rowMajorCoords igrid)
      grid =
        case Grid.fromList (Grid.getRows igrid) (Grid.getCols igrid) cells' of
          Nothing -> error "Could not build grid from list of cells!"
          Just g -> g
   in State (RoomMap grid) rp'

-- | Validate a move and count the number of boxes that will be moved.
validateMoveCountBoxen :: State -> Move -> Maybe Int
validateMoveCountBoxen (State (RoomMap grid) rc) move =
  go 0 (nextCoord $ v2ToGridRC rc)
  where
    nextCoord :: (Word32, Word32) -> (Word32, Word32)
    nextCoord coord = v2ToGridRC $ v2Add (gridRCToV2 coord) (moveToInc move)

    go :: Int -> (Word32, Word32) -> Maybe Int
    go n_boxes coord =
      let cell = Grid.getElemUnsafe grid coord
       in case cell of
            CEmpty -> Just n_boxes
            CWall -> Nothing
            CBox -> go (n_boxes + 1) (nextCoord coord)

-- | Execute a move if it is allowed.
executeMove :: Move -> State -> State
executeMove move state@(State (RoomMap grid) rp) =
  case validateMoveCountBoxen state move of
    Nothing -> state
    Just n_boxes -> runST $ do
      mgrid <- Grid.thaw grid
      Grid.write mgrid (v2ToGridRC rp) CEmpty
      let delta :: V2 = moveToInc move
      let rp' = v2Add rp delta
      pos_ref <- newSTRef $ v2Add rp' delta
      loopN n_boxes $ do
        pos <- readSTRef pos_ref
        Grid.write mgrid (v2ToGridRC pos) CBox
        writeSTRef pos_ref (v2Add pos delta)
      grid' <- Grid.freeze mgrid
      pure $ State (RoomMap grid') rp'

loopN :: Int -> ST s () -> ST s ()
loopN 0 _ = pure ()
loopN n action = action >> loopN (n - 1) action

-- | Run all moves and return the final state.
executeAllMoves :: Moves -> State -> State
executeAllMoves moves state =
  case popMove moves of
    Nothing -> state
    Just (move, moves') ->
      let state' = executeMove move state
       in executeAllMoves moves' state'

-- | Sum up the "GPS coordinates" of a room map.
sumGPSCoords :: RoomMap -> Int
sumGPSCoords (RoomMap grid) =
  sum
    $ mapMaybe
      ( \rc@(r, c) ->
          case Grid.getElemUnsafe grid rc of
            CBox -> Just $ 100 * fromIntegral r + fromIntegral c
            _ -> Nothing
      )
    $ Grid.rowMajorCoords grid

---- Parsing ------------------------------------------------------------------

loadFile :: FilePath -> IO Document
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

parseDocument :: Parser Document
parseDocument = do
  imap <- parseInitMap
  _ <- optional newline
  moves <- parseMoves
  _ <- optional newline
  _ <- eof
  pure $ Document imap moves

parseInitMap :: Parser InitMap
parseInitMap = do
  mgrid <- Grid.fromLists <$> parseInitMapLines
  case mgrid of
    Nothing -> fail "Could not parse grid!"
    Just grid -> pure $ InitMap grid

parseInitMapLines :: Parser [[InitCell]]
parseInitMapLines = some (parseInitMapLine <* newline)

parseInitMapLine :: Parser [InitCell]
parseInitMapLine = some parseInitMapCell

parseInitMapCell :: Parser InitCell
parseInitMapCell =
  (char '#' $> ICWall)
    <|> (char '.' $> ICEmpty)
    <|> (char 'O' $> ICBox)
    <|> (char '@' $> ICRobot)

parseMoves :: Parser Moves
parseMoves = Moves . concat <$> some (parseMoveLine <* newline)

parseMoveLine :: Parser [Move]
parseMoveLine = some parseMove

parseMove :: Parser Move
parseMove =
  (char '^' $> MoveUp)
    <|> (char 'v' $> MoveDown)
    <|> (char '<' $> MoveLeft)
    <|> (char '>' $> MoveRight)