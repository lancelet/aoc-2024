{-# LANGUAGE NamedFieldPuns #-}

module P19 (main, mainEx1) where

import Data.List (isPrefixOf)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text.IO (readFile)
import Data.Void (Void)
import System.Exit (exitFailure)
import Text.Megaparsec
  ( Parsec,
    eof,
    errorBundlePretty,
    many,
    optional,
    parse,
    sepBy1,
    some,
    (<|>),
  )
import Text.Megaparsec.Char (char, newline)
import Prelude hiding (readFile)

main :: IO ()
main = do
  putStrLn "Day 19"
  part1
  part2

part1 :: IO ()
part1 = do
  putStrLn "Part 1"
  let inputFile = "../inputs/input-day-19.txt"
  doc <- loadFile inputFile
  let nPossible = countPossibleDesigns doc
  putStrLn $ "Number of possible designs: " <> show nPossible

part2 :: IO ()
part2 = do
  putStrLn "Part 2"
  let inputFile = "../inputs/input-day-19.txt"
  doc <- loadFile inputFile
  let nPerms = docPerms doc
  putStrLn $ "Number of permutations: " <> show nPerms

mainEx1 :: IO ()
mainEx1 = do
  putStrLn "Example 1"
  let inputFile = "../inputs/input-day-19-example.txt"
  doc <- loadFile inputFile
  print doc
  let nPossible = countPossibleDesigns doc
  let nPerm = countDesignPermutations doc
  putStrLn $ "Number of possible designs: " <> show nPossible
  putStrLn $ "Number of design perturbations: " <> show nPerm

---- Spans Approach -----------------------------------------------------------

-- | Find all permutations in a document.
docPerms :: Doc -> Int
docPerms Doc {patterns, designs} = sum $ countPerms patterns <$> designs

-- | A Span describes a token (list of Stripe), and where it appears in the
--   input string.
data Span = Span
  { token :: !Stripes,
    start :: !Int,
    len :: !Int
  }
  deriving (Eq, Ord, Show)

-- | Find the permutations for one design.
countPerms :: [Stripes] -> Stripes -> Int
countPerms pats design =
  let spans = findAllSpans pats design
      col_map = buildColMap spans
   in countPermsFromCol col_map (length design) 0

-- | Count permutations starting from a column.
countPermsFromCol ::
  -- | Map from starting column to spans that start at that column.
  Map Int [Span] ->
  -- | Length of the input.
  Int ->
  -- | Starting column.
  Int ->
  -- | Number of permutations.
  Int
countPermsFromCol col_map input_length = fst . go Map.empty
  where
    go :: Map Span Int -> Int -> (Int, Map Span Int)
    go cache i =
      if i >= input_length
        then (1, cache)
        else case Map.lookup i col_map of
          Nothing -> (0, cache)
          Just spans -> foldr spanFoldFn (0, cache) spans

    spanFoldFn :: Span -> (Int, Map Span Int) -> (Int, Map Span Int)
    spanFoldFn spn@Span {start, len} (old_count, cache) =
      case Map.lookup spn cache of
        Nothing ->
          let (n', cache') = go cache (start + len)
              n = old_count + n'
           in (n, Map.insert spn n cache')
        Just hit -> (hit, cache)

-- | Build the map from start column indices to lists of spans.
buildColMap :: [Span] -> Map Int [Span]
buildColMap = Map.fromListWith (<>) . fmap (\s -> (start s, [s]))

-- | Find all spans for the patterns in a given input.
findAllSpans :: [Stripes] -> Stripes -> [Span]
findAllSpans patterns input = concatMap (findSpans input) patterns

-- | Find all spans in an input.
--
--   For a given token (`tok`) this finds all the places in the input where
--   it occurs.
findSpans :: Stripes -> Stripes -> [Span]
findSpans input tok = go 0 input
  where
    lenTok = length tok

    go _i [] = []
    go i ss =
      if tok `isPrefixOf` ss
        then Span tok i lenTok : go (i + 1) (tail ss)
        else go (i + 1) (tail ss)

---- Tree Approach ------------------------------------------------------------

-- | Count all possible design permutations.
countDesignPermutations :: Doc -> Int
countDesignPermutations Doc {patterns, designs} =
  length $ concatMap (constructDesignAll patterns) designs

-- | Count the number of possible designs.
countPossibleDesigns :: Doc -> Int
countPossibleDesigns Doc {patterns, designs} =
  length $ filter (designIsPossible patterns) designs

-- | Check if a design is possible.
designIsPossible :: [Stripes] -> Stripes -> Bool
designIsPossible ps = not . null . constructDesignAll ps

-- | Construct a design in all possible ways.
constructDesignAll :: [Stripes] -> Stripes -> [[Stripes]]
constructDesignAll ps design = concatMap go (splitPrefixes ps design)
  where
    go :: (Stripes, Stripes) -> [[Stripes]]
    go (pre, suf) =
      case suf of
        [] -> [[pre]]
        _ -> (pre :) <$> concatMap go (splitPrefixes ps suf)

-- | Split a list into its prefix and suffix lists.
--
--   For example:
--
--   >>> splitPrefixes ["b", "bwu"] "bwurrg"
--   [("b", "wurrg"), ("bwu", "rrg")]
splitPrefixes :: (Eq a) => [[a]] -> [a] -> [([a], [a])]
splitPrefixes prefixes xs = mapMaybe (`splitPrefix` xs) prefixes

-- | Split a single list into its prefix and suffix, if the prefix matches.
--
--   For example:
--
--   >>> splitPrefix "bwu" "bwurrg"
--   Just ("bwu", "rrg")
splitPrefix :: (Eq a) => [a] -> [a] -> Maybe ([a], [a])
splitPrefix prefix xs =
  if prefix `isPrefixOf` xs
    then Just (splitAt (length prefix) xs)
    else Nothing

---- Types --------------------------------------------------------------------

data Doc = Doc
  { patterns :: [Stripes],
    designs :: [Stripes]
  }
  deriving (Eq, Show)

type Stripes = [Stripe]

type Stripe = Char

---- Parsing ------------------------------------------------------------------

loadFile :: FilePath -> IO Doc
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

parseDocument :: Parser Doc
parseDocument =
  Doc
    <$> (parsePatterns <* newline)
    <*> (newline *> some (parsePattern <* newline))
    <* optional newline
    <* eof

parsePatterns :: Parser [[Stripe]]
parsePatterns = parsePattern `sepBy1` (char ',' *> many (char ' '))

parsePattern :: Parser [Stripe]
parsePattern = some parseStripe

parseStripe :: Parser Stripe
parseStripe = char 'w' <|> char 'u' <|> char 'b' <|> char 'r' <|> char 'g'