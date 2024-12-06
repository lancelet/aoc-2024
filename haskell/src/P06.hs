module P06 (main) where

import Data.Bits (shiftL, (.&.), (.|.))
import Data.Functor (($>))
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import Data.Text.IO (readFile)
import Data.Traversable (forM)
import qualified Data.Vector.Storable as VS
import Data.Void (Void)
import Data.Word (Word8)
import Foreign (Storable (alignment, peek, poke, sizeOf), castPtr)
import Foreign.Ptr (Ptr)
import System.Exit (exitFailure)
import Text.Megaparsec (MonadParsec (eof), Parsec, errorBundlePretty, optional, parse, some, (<|>))
import Text.Megaparsec.Char (char, newline)
import Prelude hiding (readFile)

main :: IO ()
main = do
  putStrLn "Day 06"
  initialMap <- readInput
  (roomMap, guardPos) <-
    case initialMapToRoomMap initialMap of
      Just (r, p) -> pure (r, p)
      Nothing -> do
        putStrLn "ERROR: Could not convert initial map to a room map."
        exitFailure
  let part1Result = part1 roomMap guardPos
  putStrLn "For Part 2:"
  part2Result <- part2IO roomMap guardPos
  putStrLn $ "Part 1 result: " <> show part1Result
  putStrLn $ "Part 2 result: " <> show part2Result

data Square = Empty | Obstacle deriving (Eq, Show)

data GuardOrientation
  = GuardUp
  | GuardDown
  | GuardLeft
  | GuardRight
  deriving (Eq, Show)

data GuardPos = GuardPos !Int !Int !GuardOrientation deriving (Eq, Show)

data LoopStatus = LoopUnknown | Loop | NoLoop deriving (Eq, Show)

data RoomMap = RoomMap
  { rmRows :: !Int,
    rmCols :: !Int,
    rmData :: !(VS.Vector Square)
  }
  deriving (Eq, Show)

data GuardVisitMap = GuardVisitMap
  { gmRows :: !Int,
    gmCols :: !Int,
    gmData :: !(VS.Vector Bool)
  }
  deriving (Eq, Show)

data RichGuardVisitMap = RichGuardVisitMap
  { rgmRows :: !Int,
    rgmCols :: !Int,
    rgmData :: !(VS.Vector Word8)
  }

part1 :: RoomMap -> GuardPos -> Int
part1 rm gp =
  let gm = findGuardVisitMap rm gp
   in guardVisitUniquePos gm

part2 :: RoomMap -> GuardPos -> Int
part2 rm gp =
  let gm = findGuardVisitMap rm gp
   in length $ filter (hasLoop gp) $ mapsWithExtraObstacles gp gm rm

part2IO :: RoomMap -> GuardPos -> IO Int
part2IO rm gp = do
  let gm = findGuardVisitMap rm gp
      candidates = mapsWithExtraObstacles gp gm rm
      n = length candidates
  putStrLn $ "Candidate count: " <> show n

  successes <- forM (indexed candidates) $ \(i, candidate) -> do
    let res = hasLoop gp candidate
    putStrLn $ "Processing " <> show i <> " of " <> show n <> " : " <> show res
    pure res

  pure $ length $ filter id successes

indexed :: [a] -> [(Int, a)]
indexed = go 0
  where
    go _ [] = []
    go i (x : xs) = (i, x) : go (i + 1) xs

findGuardVisitMap :: RoomMap -> GuardPos -> GuardVisitMap
findGuardVisitMap rm gp = go gp (emptyGuardVisitMap rm)
  where
    go :: GuardPos -> GuardVisitMap -> GuardVisitMap
    go gp' gm' =
      case updateGuardVisitMap rm gp' gm' of
        Nothing -> gm'
        Just (gp'', gm'') -> go gp'' gm''

findRichGuardVisitMap :: RoomMap -> GuardPos -> (LoopStatus, RichGuardVisitMap)
findRichGuardVisitMap rm gp = go gp (emptyRichGuardVisitMap rm)
  where
    go :: GuardPos -> RichGuardVisitMap -> (LoopStatus, RichGuardVisitMap)
    go gp' rgm' =
      case updateRichGuardVisitMap rm gp' rgm' of
        (ls, Nothing) -> (ls, rgm')
        (_, Just (gp'', rgm'')) -> go gp'' rgm''

updateRichGuardVisitMap ::
  RoomMap ->
  GuardPos ->
  RichGuardVisitMap ->
  (LoopStatus, Maybe (GuardPos, RichGuardVisitMap))
updateRichGuardVisitMap rm gp rgm =
  case nextGuardPos rm gp of
    Nothing -> (NoLoop, Nothing)
    Just gp'@(GuardPos r c o) ->
      if rgmHasOrientation rgm (r, c) o
        then (Loop, Nothing)
        else (LoopUnknown, Just (gp', rgmAddOrientation rgm (r, c) o))

mapsWithExtraObstacles :: GuardPos -> GuardVisitMap -> RoomMap -> [RoomMap]
mapsWithExtraObstacles (GuardPos gpr gpc _) gm rmOrig =
  catMaybes
    [ rmSetObstacleIfNotSet rmOrig (r, c)
      | r <- [0 .. rmRows rmOrig - 1],
        c <- [0 .. rmCols rmOrig - 1],
        not ((r == gpr) && (c == gpc)), -- cannot be guard's starting pos
        gmGetUnsafe gm (r, c)
    ]

rmSetObstacleIfNotSet :: RoomMap -> (Int, Int) -> Maybe RoomMap
rmSetObstacleIfNotSet rm (r, c) =
  let lindex = r * rmCols rm + c
      d = rmData rm
   in if d VS.! lindex == Obstacle
        then Nothing
        else Just $ rm {rmData = VS.force $ d VS.// [(lindex, Obstacle)]}

hasLoop :: GuardPos -> RoomMap -> Bool
hasLoop gp rm =
  case findRichGuardVisitMap rm gp of
    (Loop, _) -> True
    (NoLoop, _) -> False
    (LoopUnknown, _) -> error "Unexpected case: should find loop status"

updateGuardVisitMap ::
  RoomMap ->
  GuardPos ->
  GuardVisitMap ->
  Maybe (GuardPos, GuardVisitMap)
updateGuardVisitMap rm gp gm =
  case nextGuardPos rm gp of
    Nothing -> Nothing
    Just gp'@(GuardPos r c _) -> Just (gp', gmSetUnsafe gm (r, c))

gmSetUnsafe :: GuardVisitMap -> (Int, Int) -> GuardVisitMap
gmSetUnsafe gm (r, c) =
  let lindex = r * gmCols gm + c
      newData = VS.force $ gmData gm VS.// [(lindex, True)]
   in gm {gmData = newData}

gmGetUnsafe :: GuardVisitMap -> (Int, Int) -> Bool
gmGetUnsafe gm (r, c) =
  let lindex = r * gmCols gm + c
   in gmData gm VS.! lindex

nextGuardPos :: RoomMap -> GuardPos -> Maybe GuardPos
nextGuardPos rm (GuardPos r c o) =
  let (r', c') = case o of
        GuardUp -> (r - 1, c)
        GuardDown -> (r + 1, c)
        GuardLeft -> (r, c - 1)
        GuardRight -> (r, c + 1)
   in if not $ rmValidCoords rm (r', c')
        then Nothing
        else
          Just $
            if rmIsObstacle rm (r', c')
              then GuardPos r c (turnRight o)
              else GuardPos r' c' o

turnRight :: GuardOrientation -> GuardOrientation
turnRight go = case go of
  GuardUp -> GuardRight
  GuardRight -> GuardDown
  GuardDown -> GuardLeft
  GuardLeft -> GuardUp

emptyGuardVisitMap :: RoomMap -> GuardVisitMap
emptyGuardVisitMap rm =
  let n = rmRows rm * rmCols rm
   in GuardVisitMap (rmRows rm) (rmCols rm) (VS.fromList $ replicate n False)

emptyRichGuardVisitMap :: RoomMap -> RichGuardVisitMap
emptyRichGuardVisitMap rm =
  let n = rmRows rm * rmCols rm
   in RichGuardVisitMap (rmRows rm) (rmCols rm) (VS.fromList $ replicate n 0)

guardOrientationToWord8Bit :: GuardOrientation -> Word8
guardOrientationToWord8Bit g =
  case g of
    GuardUp -> shiftL 1 0
    GuardRight -> shiftL 1 1
    GuardDown -> shiftL 1 2
    GuardLeft -> shiftL 1 3

rgmHasOrientation ::
  RichGuardVisitMap ->
  (Int, Int) ->
  GuardOrientation ->
  Bool
rgmHasOrientation rgm (r, c) o =
  let lindex = r * rgmCols rgm + c
      os = rgmData rgm VS.! lindex
   in (os .&. guardOrientationToWord8Bit o) > 0

rgmAddOrientation ::
  RichGuardVisitMap ->
  (Int, Int) ->
  GuardOrientation ->
  RichGuardVisitMap
rgmAddOrientation rgm (r, c) o =
  let lindex = r * rgmCols rgm + c
      os = rgmData rgm VS.! lindex
      os' = os .|. guardOrientationToWord8Bit o
      d = VS.force $ rgmData rgm VS.// [(lindex, os')]
   in rgm {rgmData = d}

guardVisitUniquePos :: GuardVisitMap -> Int
guardVisitUniquePos gm = length $ filter id $ VS.toList (gmData gm)

rmElem :: RoomMap -> (Int, Int) -> Maybe Square
rmElem rm (r, c) =
  let valid = rmValidCoords rm (r, c)
      lindex = r * rmCols rm + c
   in if valid then Just (rmData rm VS.! lindex) else Nothing

rmIsObstacle :: RoomMap -> (Int, Int) -> Bool
rmIsObstacle rm rc = rmElem rm rc == Just Obstacle

rmValidCoords :: RoomMap -> (Int, Int) -> Bool
rmValidCoords rm (r, c) =
  let rValid = (r >= 0) && (r < rmRows rm)
      cValid = (c >= 0) && (c < rmCols rm)
   in rValid && cValid

initialMapToRoomMap :: InitialMap -> Maybe (RoomMap, GuardPos)
initialMapToRoomMap im =
  case initialGuardPos im of
    Nothing -> Nothing
    Just gp ->
      let convertInitialSquare s =
            case s of
              IEmpty -> Empty
              IObstacle -> Obstacle
              IGuard _ -> Empty
          rm =
            RoomMap
              (imRows im)
              (imCols im)
              (VS.map convertInitialSquare $ imData im)
       in Just (rm, gp)

instance Storable Square where
  sizeOf _ = sizeOf (undefined :: Bool)
  alignment _ = alignment (undefined :: Bool)
  peek ptr = do
    b <- peek (castPtr ptr :: Ptr Bool)
    pure $ if b then Obstacle else Empty
  poke ptr val = do
    let b = case val of
          Empty -> False
          Obstacle -> True
    poke (castPtr ptr :: Ptr Bool) b

guardOrientationToWord8 :: GuardOrientation -> Word8
guardOrientationToWord8 g = case g of
  GuardUp -> 0
  GuardDown -> 1
  GuardLeft -> 2
  GuardRight -> 3

word8ToGuardOrientation :: Word8 -> Maybe GuardOrientation
word8ToGuardOrientation w = case w of
  0 -> Just GuardUp
  1 -> Just GuardDown
  2 -> Just GuardLeft
  3 -> Just GuardRight
  _ -> Nothing

word8ToGuardOrientationUnsafe :: Word8 -> GuardOrientation
word8ToGuardOrientationUnsafe w =
  case word8ToGuardOrientation w of
    Just x -> x
    Nothing ->
      error $ "Could not convert " <> show w <> " to GuardOrientation"

---- Parsing ------------------------------------------------------------------

data InitialSquare
  = IEmpty
  | IObstacle
  | IGuard GuardOrientation
  deriving (Eq, Show)

instance Storable InitialSquare where
  sizeOf _ = sizeOf (undefined :: Word8)
  alignment _ = alignment (undefined :: Word8)
  peek ptr = do
    w8 <- peek (castPtr ptr :: Ptr Word8)
    pure $ case w8 of
      0 -> IEmpty
      1 -> IObstacle
      _ -> IGuard $ word8ToGuardOrientationUnsafe (w8 - 2)
  poke ptr val = do
    let w8 = case val of
          IEmpty -> 0
          IObstacle -> 1
          IGuard o -> 2 + guardOrientationToWord8 o
    poke (castPtr ptr :: Ptr Word8) w8

data InitialMap = InitialMap
  { imRows :: Int,
    imCols :: Int,
    imData :: VS.Vector InitialSquare
  }
  deriving (Eq, Show)

imElem :: InitialMap -> (Int, Int) -> Maybe InitialSquare
imElem im (row, col) =
  let rowValid = row >= 0 && row <= imRows im
      colValid = col >= 0 && col <= imCols im
      valid = rowValid && colValid
      lindex = row * imCols im + col
   in if valid then Just (imData im VS.! lindex) else Nothing

getInitGuardOrientation :: InitialSquare -> Maybe GuardOrientation
getInitGuardOrientation s =
  case s of
    IGuard o -> Just o
    _ -> Nothing

initialGuardPos :: InitialMap -> Maybe GuardPos
initialGuardPos im =
  let f rc = getInitGuardOrientation $ fromMaybe IEmpty $ imElem im rc

      mio :: [(Maybe GuardOrientation, (Int, Int))]
      mio =
        [ (f (r, c), (r, c))
          | r <- [0 .. imRows im - 1],
            c <- [0 .. imCols im - 1]
        ]

      isMatch ::
        (Maybe GuardOrientation, (Int, Int)) ->
        Maybe (GuardOrientation, (Int, Int))
      isMatch (Just go, (rr, cc)) = Just (go, (rr, cc))
      isMatch _ = Nothing

      goElem :: Maybe (GuardOrientation, (Int, Int))
      goElem = firstJust isMatch mio

      elemToGP :: (GuardOrientation, (Int, Int)) -> GuardPos
      elemToGP (go, (rr, cc)) = GuardPos rr cc go
   in elemToGP <$> goElem

firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust f xs =
  case xs of
    [] -> Nothing
    (a : as) ->
      case f a of
        Just x -> Just x
        Nothing -> firstJust f as

type Parser = Parsec Void Text

inputFile :: FilePath
inputFile = "../inputs/input-day-06.txt"

readInput :: IO InitialMap
readInput = do
  txt <- readFile inputFile
  let parseResult = parse parseSquare inputFile txt
  parsed <- case parseResult of
    Left errorBundle -> do
      putStrLn "ERROR: Could not parse input file."
      putStrLn $ errorBundlePretty errorBundle
      exitFailure
    Right r -> pure r
  case initialMapFromLists parsed of
    Nothing -> do
      putStrLn "ERROR: Could not convert input to an initial map."
      exitFailure
    Just initialMap -> pure initialMap

initialMapFromRowMajor :: Int -> Int -> [InitialSquare] -> Maybe InitialMap
initialMapFromRowMajor rows cols elems =
  let nExpected = rows * cols
      nElems = length elems
      sizeOk = nExpected == nElems
   in if sizeOk
        then Just $ InitialMap rows cols (VS.fromList elems)
        else Nothing

initialMapFromLists :: [[InitialSquare]] -> Maybe InitialMap
initialMapFromLists lists =
  let nRows = length lists
   in if nRows <= 0
        then Nothing
        else
          let nCols = length (head lists)
           in if nCols <= 0
                then Nothing
                else
                  let allOk = all ((nCols ==) . length) lists
                   in if allOk
                        then initialMapFromRowMajor nRows nCols (concat lists)
                        else Nothing

parseSquare :: Parser [[InitialSquare]]
parseSquare = some parseRow <* optional newline <* eof

parseRow :: Parser [InitialSquare]
parseRow = some parseInitialSquare <* newline

parseInitialSquare :: Parser InitialSquare
parseInitialSquare = parseEmpty <|> parseObstacle <|> parseGuard
  where
    parseEmpty = char '.' $> IEmpty
    parseObstacle = char '#' $> IObstacle
    parseGuard = IGuard <$> parseGuardOrientation
    parseGuardOrientation =
      (char '^' $> GuardUp)
        <|> (char 'v' $> GuardDown)
        <|> (char '>' $> GuardLeft)
        <|> (char '<' $> GuardRight)
