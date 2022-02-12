import Data.Char (digitToInt, isDigit)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord
import qualified Data.Set as Set
import qualified Data.Tuple as Tuple

main :: IO ()
main = do
  contents <- getContents
  let board = getBoard contents
  print ("res=", firstAllFlashed board 0)

firstAllFlashed :: Board -> Int -> Int
firstAllFlashed board stepNum
  | (Map.size . Map.filter (/= 0) $ tiles board) == 0 = stepNum
  | otherwise = firstAllFlashed (stepBoard board) stepNum + 1

stepBoard :: Board -> Board
stepBoard b@Board {tiles = btiles, numFlashed = nf} =
  newB {flashedSoFar = newFlashedSoFar, numFlashed = newNumFlashed}
  where
    newTiles = Map.map (+ 1) btiles
    newB@Board {flashedSoFar = fsf} = handleFlashes b {tiles = newTiles}
    newNumFlashed = Set.size fsf + nf
    newFlashedSoFar = Set.empty

handleFlashes :: Board -> Board
handleFlashes b@Board {tiles = btiles, flashedSoFar = fsf}
  | shouldFlashLen == 0 = b
  | otherwise =
    handleFlashes $ b {tiles = flashersAdjusted, flashedSoFar = newFlashedSoFar}
  where
    overNine = Map.keysSet . Map.filter (> 9) $ btiles
    shouldFlash = Set.difference overNine fsf
    shouldFlashLen = length shouldFlash
    newFlashedSoFar = Set.union shouldFlash fsf
    neighborsToFix =
      map (filter (not . (flip Set.member) fsf) . getNeighbors) $
      Set.toList shouldFlash
    neighborsAdjusted =
      foldr
        (\ns acc -> foldr (\n acc -> Map.adjust (+ 1) n acc) acc ns)
        btiles
        neighborsToFix
    flashersAdjusted =
      foldr
        (\ns acc -> Map.adjust (\x -> 0) ns acc)
        neighborsAdjusted
        shouldFlash

getBoard :: String -> Board
getBoard contents =
  Board
    { tiles = Map.fromList coords
    , numRows = lenRows
    , numCols = lenCols
    , numFlashed = 0
    , flashedSoFar = Set.empty
    }
  where
    rows = map (map digitToInt) $ lines contents :: [[Int]]
    lenCols = length . head $ rows
    lenRows = length rows
    colIndices = [0 .. lenCols - 1]
    rowIndices = [0 .. lenRows - 1]
    oneRow :: Int -> [Int] -> Row
    oneRow rowNum rowVals =
      zipWith (\col val -> ((col, rowNum), val)) colIndices rowVals
    coords = concat . zipWith oneRow rowIndices $ rows

getNeighbors :: Coord -> [Coord]
getNeighbors coord =
  [ (x + xdiff, y + ydiff)
  | let eachDir = [-1, 0, 1]
  , let (x, y) = coord
  , xdiff <- eachDir
  , ydiff <- eachDir
  , (not (xdiff == 0 && ydiff == 0))
  ]

type Coord = (Int, Int)

type Row = [(Coord, Int)]

data Board =
  Board
    { tiles :: Map.Map Coord Int
    , numRows :: Int
    , numCols :: Int
    , numFlashed :: Int
    , flashedSoFar :: Set.Set Coord
    }
  deriving (Show)

debugBoard =
  getBoard
    "\
\5483143223\n\
\2745854711\n\
\5264556173\n\
\6141336146\n\
\6357385478\n\
\4167524645\n\
\2176841721\n\
\6882881134\n\
\4846848554\n\
\5283751526\n\
\"
