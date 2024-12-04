module P04 (main) where

import Data.Text (Text)
import Data.Text.IO (readFile)
import Data.Vector.Unboxed (Vector, (!))
import qualified Data.Vector.Unboxed as V
import Data.Void (Void)
import System.Exit (exitFailure)
import Text.Megaparsec (Parsec, anySingleBut, eof, errorBundlePretty, parse, some)
import Text.Megaparsec.Char (newline)
import Prelude hiding (readFile)

---- Main ---------------------------------------------------------------------

main :: IO ()
main = do
  wg <- readInput
  let nWords = countWord "XMAS" wg
  let nMas = countMas wg
  putStrLn $ "N instances of XMAS:         " <> show nWords
  putStrLn $ "N instances of crossed XMAS: " <> show nMas

---- Processing ---------------------------------------------------------------

countMas :: WordGrid -> Int
countMas wg =
  sum $
    (\b -> if b then 1 else 0)
      <$> [ isMas (r, c) wg
            | r <- [0 .. wgRows wg - 1],
              c <- [0 .. wgCols wg - 1]
          ]

-- Configurations of MAS:
--  1     2     3     4
-- M.S   M.M   S.S   S.M
-- .A.   .A.   .A.   .A.
-- M.S   S.S   M.M   S.M
isMas :: (Int, Int) -> WordGrid -> Bool
isMas (r, c) wg =
  (getElem wg (r, c) == Just 'A')
    && ( let w1 = getWord (\(r', c') -> (r' + 1, c' + 1)) 3 (r - 1, c - 1) wg
             w2 = getWord (\(r', c') -> (r' - 1, c' + 1)) 3 (r + 1, c - 1) wg
          in (w1 == Just "MAS" || w1 == Just "SAM")
               && (w2 == Just "MAS" || w2 == Just "SAM")
       )

countWord :: String -> WordGrid -> Int
countWord target wg =
  sum $
    [ countWordsAt target (r, c) wg
      | r <- [0 .. wgRows wg - 1],
        c <- [0 .. wgCols wg - 1]
    ]

countWordsAt :: String -> (Int, Int) -> WordGrid -> Int
countWordsAt target rc wg =
  chkDir (Dir Hori Pos)
    + chkDir (Dir Hori Neg)
    + chkDir (Dir Vert Pos)
    + chkDir (Dir Vert Neg)
    + chkDir (Dir DiaU Pos)
    + chkDir (Dir DiaU Neg)
    + chkDir (Dir DiaD Pos)
    + chkDir (Dir DiaD Neg)
  where
    n :: Int
    n = length target

    chkDir :: Dir -> Int
    chkDir dir = if getWordD dir n rc wg == Just target then 1 else 0

---- Extracting Words ---------------------------------------------------------

data Ori = Hori | Vert | DiaU | DiaD

data Sen = Pos | Neg

data Dir = Dir !Ori !Sen

-- | Get a word in a given direction.
getWordD :: Dir -> Int -> (Int, Int) -> WordGrid -> Maybe String
getWordD dir = getWord (getAdv dir)

-- | Find the "advance function" for different word directions.
getAdv :: Dir -> ((Int, Int) -> (Int, Int))
getAdv (Dir Hori Pos) = \(r, c) -> (r, c + 1)
getAdv (Dir Hori Neg) = \(r, c) -> (r, c - 1)
getAdv (Dir Vert Pos) = \(r, c) -> (r + 1, c)
getAdv (Dir Vert Neg) = \(r, c) -> (r - 1, c)
getAdv (Dir DiaU Pos) = \(r, c) -> (r + 1, c + 1)
getAdv (Dir DiaU Neg) = \(r, c) -> (r - 1, c - 1)
getAdv (Dir DiaD Pos) = \(r, c) -> (r + 1, c - 1)
getAdv (Dir DiaD Neg) = \(r, c) -> (r - 1, c + 1)

getWord ::
  -- | How to advance to the next character
  ((Int, Int) -> (Int, Int)) ->
  -- | Number of characters
  Int ->
  -- | Starting coordinate
  (Int, Int) ->
  -- | Word grid from which to fetch characters
  WordGrid ->
  -- | String if it exists
  Maybe String
getWord adv n' (r', c') wg = sequence $ go n' (r', c') []
  where
    go :: Int -> (Int, Int) -> [Maybe Char] -> [Maybe Char]
    go 0 _ accum = reverse accum
    go n (r, c) accum = go (n - 1) (adv (r, c)) (getElem wg (r, c) : accum)

---- Word Grid ----------------------------------------------------------------

data WordGrid = WordGrid
  { wgRows :: Int,
    wgCols :: Int,
    wgData :: Vector Char
  }
  deriving (Eq, Show)

getElem :: WordGrid -> (Int, Int) -> Maybe Char
getElem wg (r, c)
  | inRange = Just $ wgData wg ! lindex
  | otherwise = Nothing
  where
    lindex = r * wgCols wg + c
    inRange = r >= 0 && r < wgRows wg && c >= 0 && c < wgCols wg

mkWordGrid' :: Int -> Int -> [Char] -> Maybe WordGrid
mkWordGrid' rows cols chars =
  let v = V.fromList chars
      n = rows * cols
   in if V.length v == n then Just (WordGrid rows cols v) else Nothing

mkWordGrid :: [[Char]] -> Maybe WordGrid
mkWordGrid ccs =
  let rows = length ccs
   in if rows == 0
        then Nothing
        else
          let cols = length (head ccs)
           in if (cols == 0) || not (all ((cols ==) . length) ccs)
                then Nothing
                else mkWordGrid' rows cols (concat ccs)

---- Parsing ------------------------------------------------------------------

type Parser = Parsec Void Text

inputFile :: FilePath
inputFile = "../inputs/input-day-04.txt"

readInput :: IO WordGrid
readInput = do
  txt <- readFile inputFile
  let res = parse parseLines inputFile txt
  ccs <- case res of
    Left errorBundle -> do
      putStrLn "ERROR: Could not parse input file."
      putStrLn $ errorBundlePretty errorBundle
      exitFailure
    Right xxs -> pure xxs
  case mkWordGrid ccs of
    Nothing -> do
      putStrLn "ERROR: Could not process chars as a word grid."
      exitFailure
    Just wg -> pure wg

parseLines :: Parser [[Char]]
parseLines = some parseLine <* eof

parseLine :: Parser [Char]
parseLine = some (anySingleBut '\n') <* newline