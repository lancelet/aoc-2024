{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module P16 where

import AStar (astar)
import qualified AStar
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import Data.Text (Text)
import Data.Text.IO (readFile)
import Data.Void (Void)
import Data.Word (Word32)
import Grid (BoolGrid)
import qualified Grid
import System.Exit (exitFailure)
import Text.Megaparsec (Parsec, eof, errorBundlePretty, optional, parse, some, (<|>))
import Text.Megaparsec.Char (char, newline)
import Prelude hiding (readFile)

main :: IO ()
main = do
  putStrLn "Day 16"
  part1

part1 :: IO ()
part1 = do
  putStrLn "Part 1"
  let filePath = "../inputs/input-day-16.txt"
  maze <- loadFile filePath
  let pathScore = scorePath . solvePath $ maze
  putStrLn $ "pathScore = " <> show pathScore

mainEx1 :: IO ()
mainEx1 = do
  putStrLn "Example 1"
  let filePath = "../inputs/input-day-16-example-1.txt"
  maze <- loadFile filePath
  putStrLn $ prettyMaze maze
  let soln = solvePath maze
  let s = scorePath soln
  print soln
  putStrLn $ "score = " <> show s

mainEx2 :: IO ()
mainEx2 = do
  putStrLn "Example 2"
  let filePath = "../inputs/input-day-16-example-2.txt"
  maze <- loadFile filePath
  putStrLn $ prettyMaze maze
  let soln = solvePath maze
  let s = scorePath soln
  print soln
  putStrLn $ "score = " <> show s

---- Branching Traversal ------------------------------------------------------

solvePath :: Maze -> [(Action, Node)]
solvePath maze =
  case astar sn ef successor cost heuristic of
    AStar.NoPathFound -> error "A* could not find a valid path"
    AStar.PathFound _start ns -> ns
  where
    sn :: Node
    sn = Node PointE (mazeStart maze)

    ef :: Node -> Bool
    ef = (mazeEnd maze ==) . nodePosition

    successor :: Node -> [(Action, Node)]
    successor node =
      maybeToList (tryStep maze node) <> [turnLeftNode node, turnRightNode node]

    heuristic :: Node -> Int
    heuristic node =
      let rc = nodePosition node
          r, c, re, ce :: Int
          r = fromIntegral . fst $ rc
          c = fromIntegral . snd $ rc
          re = fromIntegral . fst . mazeEnd $ maze
          ce = fromIntegral . snd . mazeEnd $ maze
       in abs (re - r) + abs (ce - c)

data Node = Node
  { nodePointing :: !Pointing,
    nodePosition :: !RC
  }
  deriving (Eq, Ord, Show)

data Pointing = PointN | PointE | PointS | PointW deriving (Eq, Ord, Show)

data Action
  = ActionStep
  | ActionTurnLeft
  | ActionTurnRight
  deriving (Eq, Show)

scorePath :: [(Action, Node)] -> Int
scorePath = sum . fmap cost

cost :: (Action, Node) -> Int
cost (action, _) =
  case action of
    ActionStep -> 1
    ActionTurnLeft -> 1000
    ActionTurnRight -> 1000

tryStep :: Maze -> Node -> Maybe (Action, Node)
tryStep maze node =
  let rc_step = stepInc (nodePointing node) (nodePosition node)
   in if fromMaybe True (wallAt maze rc_step)
        then Nothing
        else Just (ActionStep, node {nodePosition = rc_step})

turnLeftNode :: Node -> (Action, Node)
turnLeftNode node =
  (ActionTurnLeft, node {nodePointing = turnLeft . nodePointing $ node})

turnRightNode :: Node -> (Action, Node)
turnRightNode node =
  (ActionTurnRight, node {nodePointing = turnRight . nodePointing $ node})

stepInc :: Pointing -> RC -> RC
stepInc p (r, c) =
  case p of
    PointN -> (r - 1, c)
    PointE -> (r, c + 1)
    PointS -> (r + 1, c)
    PointW -> (r, c - 1)

turnLeft :: Pointing -> Pointing
turnLeft p =
  case p of
    PointN -> PointW
    PointE -> PointN
    PointS -> PointE
    PointW -> PointS

turnRight :: Pointing -> Pointing
turnRight p =
  case p of
    PointN -> PointE
    PointE -> PointS
    PointS -> PointW
    PointW -> PointN

---- Data Types ---------------------------------------------------------------

type RC = (Word32, Word32)

data Maze = Maze
  { mazeWalls :: !BoolGrid,
    mazeStart :: !RC,
    mazeEnd :: !RC
  }
  deriving (Eq, Show)

wallAt :: Maze -> RC -> Maybe Bool
wallAt maze = Grid.getElem (mazeWalls maze)

wallAt' :: Maze -> RC -> Bool
wallAt' maze rc =
  fromMaybe (error "wallAt': Coordinates out of bounds") $ wallAt maze rc

mazeCols :: Maze -> Word32
mazeCols = Grid.getCols . mazeWalls

mazeRows :: Maze -> Word32
mazeRows = Grid.getRows . mazeWalls

prettyMaze :: Maze -> String
prettyMaze maze =
  unlines $
    chunksOf (fromIntegral . mazeCols $ maze) $
      rcChar <$> (Grid.rowMajorCoords . mazeWalls $ maze)
  where
    rcChar :: RC -> Char
    rcChar rc
      | wallAt' maze rc = '#'
      | mazeStart maze == rc = 'S'
      | mazeEnd maze == rc = 'E'
      | otherwise = '.'

chunksOf :: forall a. Int -> [a] -> [[a]]
chunksOf n = go n []
  where
    go :: Int -> [a] -> [a] -> [[a]]
    go _ accum [] = [reverse accum]
    go 0 accum xs = reverse accum : chunksOf n xs
    go i accum (x : xs) = go (i - 1) (x : accum) xs

---- Parsing ------------------------------------------------------------------

loadFile :: FilePath -> IO Maze
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

parseDocument :: Parser Maze
parseDocument = do
  (bools, maze_start, maze_end) <- parseInMap
  _ <- optional newline
  _ <- eof
  let grid =
        fromMaybe
          (error "parseDocument: could not parse grid")
          (Grid.fromLists bools)
  pure $ Maze grid maze_start maze_end

parseInMap :: Parser ([[Bool]], RC, RC)
parseInMap = do
  rows <- some (parseInRow <* newline)
  let bools :: [[Bool]] = fst <$> rows
  let specials :: [(MazeSpecial, RC)] =
        concatMap
          ( \(xs, r) ->
              (\(ms, c) -> (ms, (r, c))) <$> xs
          )
          $ zip
            (snd <$> rows)
            [0 ..]
  let (s, e) =
        case specials of
          [(SpecialStart, ss), (SpecialEnd, ee)] -> (ss, ee)
          [(SpecialEnd, ee), (SpecialStart, ss)] -> (ss, ee)
          _ -> error "Only one start and end point are allowed"
  pure (bools, s, e)

type InCell = Char

data MazeSpecial = SpecialStart | SpecialEnd

classifyInCell :: InCell -> Either Bool MazeSpecial
classifyInCell c =
  case c of
    '#' -> Left True
    '.' -> Left False
    'S' -> Right SpecialStart
    'E' -> Right SpecialEnd
    _ -> error "classifyInCell: Unknown cell type"

classifiedToBool :: [Either Bool MazeSpecial] -> [Bool]
classifiedToBool = fmap toBool
  where
    toBool (Left b) = b
    toBool (Right _) = False

findSpecials :: [Either Bool MazeSpecial] -> [(MazeSpecial, Word32)]
findSpecials xs =
  mapMaybe
    ( \(x, i) ->
        case x of
          Left _ -> Nothing
          Right ms -> Just (ms, i)
    )
    (zip xs [0 ..])

parseInRow :: Parser ([Bool], [(MazeSpecial, Word32)])
parseInRow = do
  in_cells <- fmap classifyInCell <$> parseInRowCells
  pure (classifiedToBool in_cells, findSpecials in_cells)

parseInRowCells :: Parser [InCell]
parseInRowCells = some parseInCell <* optional (some $ char ' ')

parseInCell :: Parser InCell
parseInCell = char '#' <|> char '.' <|> char 'S' <|> char 'E'