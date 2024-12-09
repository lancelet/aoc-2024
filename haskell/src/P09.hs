{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module P09 (main) where

import Control.Monad (when)
import Control.Monad.ST (ST, runST)
import Data.Char (digitToInt)
import Data.Ord (comparing)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Data.Text (Text)
import Data.Text.IO (readFile)
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as VAI
import Data.Vector.Generic.Mutable (PrimMonad)
import qualified Data.Vector.Mutable as VM
import Data.Void (Void)
import System.Exit (exitFailure)
import Text.Megaparsec (Parsec, eof, errorBundlePretty, optional, parse)
import Text.Megaparsec.Char (digitChar, newline)
import Prelude hiding (readFile)

main :: IO ()
main = do
  putStrLn "Day 9"
  inputMap <- readInput
  let part1Result = part1 inputMap
  putStrLn $ "Part 1 result: " <> show part1Result
  let part2Result = part2 inputMap
  putStrLn $ "Part 2 result: " <> show part2Result

---- Part 1 -------------------------------------------------------------------

part1 :: InputMap -> Int
part1 inputMap =
  let diskString = inputMapToDiskString inputMap
      compacted = compact diskString
      cs = checkSum compacted
   in cs

---- Part 2 -------------------------------------------------------------------

part2 :: InputMap -> Int
part2 inputMap =
  let cfs = inputMapToCFileSys inputMap
      ids = FileId <$> [0 .. (nFiles cfs - 1)]
      cfs' = foldr tryMove cfs ids
      diskString = cFileSysToDiskString cfs'
   in checkSum diskString

---- File locations Part 2 ----------------------------------------------------

newtype FileId = FileId Int deriving (Eq, Show)

-- | Contiguous file.
data CFile = CFile
  { cfileId :: !FileId,
    cfileStart :: !Int,
    cfileLength :: !Int
  }
  deriving (Eq, Show)

-- | Filesystem with only contiguous files.
data CFileSys = CFileSys
  { cfilesysSize :: !Int,
    cfilesysFiles :: !(V.Vector CFile)
  }
  deriving (Eq, Show)

-- | Try moving one file.
tryMove :: FileId -> CFileSys -> CFileSys
tryMove file_id fs =
  case getFile file_id fs of
    Nothing -> error "Unexpected; could not find file to move!"
    Just file ->
      case findFreeInside (cfileLength file) fs of
        Just ofs | ofs < cfileStart file -> moveFile (file_id, ofs) fs
        _ -> fs

-- | Return number of files in the filesystem.
nFiles :: CFileSys -> Int
nFiles (CFileSys _ files) = V.length files

-- | Get a file by its id.
getFile :: FileId -> CFileSys -> Maybe CFile
getFile file_id (CFileSys _ files) =
  let len = V.length files
      go idx =
        if idx < len
          then
            let file = files V.! idx
                cur_file_id = cfileId file
             in if cur_file_id == file_id
                  then Just file
                  else go (idx + 1)
          else Nothing
   in go 0

-- | Find the index of first free space by size.
--
-- This must be space between files; space at the end is not counted.
findFreeInside :: Int -> CFileSys -> Maybe Int
findFreeInside sz (CFileSys _ files) = go (V.toList files)
  where
    go :: [CFile] -> Maybe Int
    go [] = Nothing
    go [_] = Nothing
    go (x : y : xs) =
      let interspace = cfileStart y - cfileStart x - cfileLength x
       in if interspace >= sz
            then Just (cfileStart x + cfileLength x)
            else go (y : xs)

-- | Move a file in the filesystem.
moveFile ::
  -- | ID of the file and its new offset.
  (FileId, Int) ->
  -- | Input filesystem.
  CFileSys ->
  -- \ Output filesystem; files sorted by offset.
  CFileSys
moveFile (file_id, new_offset) (CFileSys sz files) =
  let len = V.length files
   in runST $ do
        mfiles <- V.thaw files
        let loop i =
              when (i < len) $ do
                file <- VM.read mfiles i
                if cfileId file == file_id
                  then do
                    VM.write mfiles i (CFile file_id new_offset (cfileLength file))
                    pure ()
                  else loop (i + 1)
        loop 0
        VAI.sortBy (comparing cfileStart) mfiles
        newFiles <- V.freeze mfiles
        pure $ CFileSys sz newFiles

-- | Convert an InputMap to a CFileSys.
inputMapToCFileSys :: InputMap -> CFileSys
inputMapToCFileSys im = CFileSys sz (V.fromList files)
  where
    sz :: Int
    sz = inputMapSize im

    files :: [CFile]
    files = go 0 0 im

    go :: Int -> Int -> InputMap -> [CFile]
    go offset file_id (InputMapSing n) =
      [CFile (FileId file_id) offset n]
    go offset file_id (InputMapCons file_size space_size remainder) =
      CFile (FileId file_id) offset file_size
        : go (offset + file_size + space_size) (file_id + 1) remainder

cFileSysToDiskString :: CFileSys -> DiskString
cFileSysToDiskString (CFileSys _ files) =
  DiskString . V.fromList $ go (V.toList files)
  where
    go :: [CFile] -> [Block]
    go [] = []
    go [x] =
      let FileId file_id = cfileId x
          nfile = cfileLength x
       in replicate nfile (BlockFile file_id)
    go (x : y : xs) =
      let FileId file_id = cfileId x
          nfile = cfileLength x
          nspace = cfileStart y - cfileStart x - cfileLength x
       in replicate nfile (BlockFile file_id)
            <> replicate nspace BlockEmpty
            <> go (y : xs)

---- File locations Part 1 ----------------------------------------------------

data Block
  = BlockEmpty
  | BlockFile !Int
  deriving (Eq, Show)

newtype DiskString = DiskString (V.Vector Block) deriving (Eq, Show)

-- | Decompress the input map into a disk string.
inputMapToDiskString :: InputMap -> DiskString
inputMapToDiskString inputMap = DiskString $ V.fromList $ go 0 inputMap
  where
    go :: Int -> InputMap -> [Block]
    go file_number (InputMapSing file_size) =
      replicate file_size (BlockFile file_number)
    go file_number (InputMapCons file_size space_size remainder) =
      replicate file_size (BlockFile file_number)
        <> replicate space_size BlockEmpty
        <> go (file_number + 1) remainder

-- | Compact the disk string.
compact :: DiskString -> DiskString
compact (DiskString blocks) = runST $
  do
    mb <- V.thaw blocks
    start_ref <- newSTRef 0
    end_ref <- newSTRef (VM.length mb - 1)
    let go = do
          moved <- moveSingleBlock mb (start_ref, end_ref)
          when moved go
    go
    DiskString <$> V.freeze mb

checkSum :: DiskString -> Int
checkSum (DiskString blocks) = sum $ fmap f $ indexed $ V.toList blocks
  where
    f :: (Int, Block) -> Int
    f (_, BlockEmpty) = 0
    f (i, BlockFile file_id) = i * file_id

indexed :: [a] -> [(Int, a)]
indexed = go 0
  where
    go :: Int -> [a] -> [(Int, a)]
    go _ [] = []
    go i (x : xs) = (i, x) : go (i + 1) xs

moveSingleBlock ::
  VM.MVector s Block ->
  (STRef s Int, STRef s Int) ->
  ST s Bool
moveSingleBlock mb (start_ref, end_ref) = do
  start <- readSTRef start_ref
  end <- readSTRef end_ref
  maybe_empty <- nextEmptyBlock mb start
  maybe_file <- lastFileBlock mb end

  case (maybe_empty, maybe_file) of
    (Just empty_loc, Just file_loc) | file_loc >= empty_loc -> do
      block_file <- VM.read mb file_loc
      VM.write mb empty_loc block_file
      VM.write mb file_loc BlockEmpty
      writeSTRef start_ref empty_loc
      writeSTRef end_ref file_loc
      pure True
    _ -> pure False

-- | Return the next empty block from a vector, starting at i.
--
-- Returns `Nothing` if there are no more empty blocks.
nextEmptyBlock ::
  (PrimMonad m, s ~ VM.PrimState m) =>
  VM.MVector s Block ->
  Int ->
  m (Maybe Int)
nextEmptyBlock mb i = do
  x <- VM.readMaybe mb i
  case x of
    Nothing -> pure Nothing
    Just BlockEmpty -> pure $ Just i
    Just (BlockFile _) -> nextEmptyBlock mb (i + 1)

-- | Returns the last file block, starting at i.
lastFileBlock ::
  (PrimMonad m, s ~ VM.PrimState m) =>
  VM.MVector s Block ->
  Int ->
  m (Maybe Int)
lastFileBlock mb i =
  if i < 0
    then pure Nothing
    else do
      x <- VM.read mb i
      case x of
        BlockFile _ -> pure $ Just i
        BlockEmpty -> lastFileBlock mb (i - 1)

---- InputMap data type -------------------------------------------------------

data InputMap
  = InputMapSing !Int
  | InputMapCons
      { _imcFile :: !Int,
        _imcSpace :: !Int,
        _imcRem :: !InputMap
      }
  deriving (Eq, Show)

-- | Total size of an input map.
inputMapSize :: InputMap -> Int
inputMapSize im =
  case im of
    InputMapSing n -> n
    InputMapCons f s r -> f + s + inputMapSize r

---- Parsing ------------------------------------------------------------------

type Parser = Parsec Void Text

inputFile :: FilePath
inputFile = "../inputs/input-day-09.txt"

readInput :: IO InputMap
readInput = do
  txt <- readFile inputFile
  let parseResult = parse parseDocument inputFile txt
  case parseResult of
    Left errorBundle -> do
      putStrLn "ERROR: Could not parse input file."
      putStrLn $ errorBundlePretty errorBundle
      exitFailure
    Right inputMap -> pure inputMap

parseDocument :: Parser InputMap
parseDocument = parseInputMap <* optional newline <* eof

parseInputMap :: Parser InputMap
parseInputMap = do
  file_size <- digitToInt <$> digitChar
  space_opt <- fmap digitToInt <$> optional digitChar
  case space_opt of
    Nothing -> pure $ InputMapSing file_size
    Just space -> InputMapCons file_size space <$> parseInputMap
