module P05 (main) where

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Text.IO (readFile)
import Data.Void (Void)
import System.Exit (exitFailure)
import Text.Megaparsec (Parsec, eof, errorBundlePretty, optional, parse, sepBy1, some, try)
import Text.Megaparsec.Char (char, newline)
import Text.Megaparsec.Char.Lexer (decimal)
import Prelude hiding (readFile)

main :: IO ()
main = do
  putStrLn "Day 05"
  doc <- readInput
  let part1Sum = part1 doc
  let part2Sum = part2 doc
  putStrLn $ "Sum of valid middle pages: " <> show part1Sum
  putStrLn $ "Sum of fixed middle pages: " <> show part2Sum

---- Checking Correct Updates -------------------------------------------------

-- | Type of a page.
type Page = Int

-- | Map from a page, to all the pages that must appear AFTER it, not before it.
type AfterMap = Map Page PageSet

-- | Set of pages.
type PageSet = IntSet

-- | From a set of ordering rules, create the "AfterMap"
mkAfterMap :: [OrderRule] -> AfterMap
mkAfterMap = foldr insert Map.empty
  where
    insert :: OrderRule -> AfterMap -> AfterMap
    insert (OrderRule before after) =
      Map.insertWith IntSet.union before (IntSet.singleton after)

-- Check if a single UpdatePages is valid.
updatePagesIsValid :: AfterMap -> UpdatePages -> Bool
updatePagesIsValid am (UpdatePages ps) = go IntSet.empty ps
  where
    go :: PageSet -> [Int] -> Bool
    go _ [] = True
    go beforeSet (x : xs) =
      let afterSet = Map.findWithDefault IntSet.empty x am
          u = IntSet.intersection beforeSet afterSet
       in (IntSet.null u && go (IntSet.insert x beforeSet) xs)

-- Find the middle page.
middlePage :: UpdatePages -> Int
middlePage (UpdatePages ps) = ps !! ((length ps - 1) `div` 2)

part1 :: Document -> Int
part1 (Document orderRules updatePages) =
  let am = mkAfterMap orderRules
      validUpdates = filter (updatePagesIsValid am) updatePages
      middlePages = middlePage <$> validUpdates
   in sum middlePages

fixUpdatePages :: AfterMap -> UpdatePages -> UpdatePages
fixUpdatePages am p =
  if updatePagesIsValid am p
    then p
    else fixUpdatePages am (evolveUpdatePages am p)

evolveUpdatePages :: AfterMap -> UpdatePages -> UpdatePages
evolveUpdatePages am (UpdatePages ps) =
  UpdatePages $ go IntSet.empty [] ps
  where
    go :: PageSet -> [Int] -> [Int] -> [Int]
    go _ accum [] = reverse accum
    go before accum (x : xs) =
      let after = Map.findWithDefault IntSet.empty x am
          u = IntSet.intersection before after
       in if IntSet.null u
            then go (IntSet.insert x before) (x : accum) xs
            else x : (reverse accum ++ xs)

part2 :: Document -> Int
part2 (Document orderRules updatePages) =
  let am = mkAfterMap orderRules
      invalidUpdates = filter (not . updatePagesIsValid am) updatePages
      fixedUpdates = fixUpdatePages am <$> invalidUpdates
      middlePages = middlePage <$> fixedUpdates
   in sum middlePages

---- Parsing ------------------------------------------------------------------

data Document = Document [OrderRule] [UpdatePages] deriving (Eq, Show)

data OrderRule = OrderRule !Int !Int deriving (Eq, Show)

newtype UpdatePages = UpdatePages [Int] deriving (Eq, Show)

type Parser = Parsec Void Text

inputFile :: FilePath
inputFile = "../inputs/input-day-05.txt"

readInput :: IO Document
readInput = do
  txt <- readFile inputFile
  let res = parse parseDocument inputFile txt
  case res of
    Left errorBundle -> do
      putStrLn "ERROR: Could not parse input file."
      putStrLn $ errorBundlePretty errorBundle
      exitFailure
    Right doc -> pure doc

parseDocument :: Parser Document
parseDocument =
  Document
    <$> parseOrderRuleList
    <* optional newline
    <*> parseUpdatePagesList
    <* optional newline
    <* eof

parseOrderRuleList :: Parser [OrderRule]
parseOrderRuleList = some parseOrderRule

parseUpdatePagesList :: Parser [UpdatePages]
parseUpdatePagesList = some parseUpdatePages

parseOrderRule :: Parser OrderRule
parseOrderRule =
  OrderRule <$> decimal <* char '|' <*> decimal <* newline

parseUpdatePages :: Parser UpdatePages
parseUpdatePages =
  UpdatePages <$> sepBy1 (try decimal) (try (char ',')) <* newline
