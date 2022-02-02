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
  -- mapM_ print $ Map.toList . tiles $ board
  let lowestPoints = lowPoints board
  let biggestBasins =
        product .
        take 3 .
        reverse . List.sort . map (\lp -> length $ biggestNeighbors lp board) $
        lowestPoints
  print ("res=", biggestBasins)

debugBoard =
  "2199943210\n\
             \3987894921\n\
             \9856789892\n\
             \876789678\n\
             \9899965678"

lowPoints :: Board -> [Coord]
lowPoints board = Map.keys boardLowPoints
  where
    boardLowPoints =
      Map.filterWithKey (\coord val -> isLowPoint coord board) $ tiles board

getBoard :: String -> Board
getBoard contents =
  Board {tiles = Map.fromList coords, numRows = lenRows, numCols = lenCols}
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

isLowPoint :: Coord -> Board -> Bool
isLowPoint coord board = allLess
  where
    neighbors = getNeighbors coord
    boardTiles = tiles board
    thisTile = Map.lookup coord boardTiles
    allLess =
      all
        (\c ->
           let coordVal = Map.lookup c boardTiles
               emptyTile = Maybe.isNothing coordVal
            in coordVal > thisTile || emptyTile)
        neighbors

getNeighbors :: Coord -> [Coord]
getNeighbors coord = neighbors
  where
    neighbors =
      [ (x + xdiff, y + ydiff)
      | let eachDir = [(1, 0), (-1, 0), (0, 1), (0, -1)]
      , let (x, y) = coord
      , (xdiff, ydiff) <- eachDir
      ]

biggestNeighbors :: Coord -> Board -> Set.Set Coord
biggestNeighbors coord board = Set.union justCoord friends
  where
    neighbors = getNeighbors coord
    boardTiles = tiles board
    thisTile = Map.lookup coord boardTiles
    bigger =
      filter
        (\c ->
           let coordVal = Map.lookup c boardTiles
            in coordVal > thisTile && coordVal < Just 9)
        neighbors
    justCoord = Set.insert coord Set.empty
    friends =
      foldr
        (\c acc -> Set.union acc $ biggestNeighbors c board)
        justCoord
        bigger

type Coord = (Int, Int)

type Row = [(Coord, Int)]

data Board =
  Board
    { tiles :: Map.Map Coord Int
    , numRows :: Int
    , numCols :: Int
    }
  deriving (Show)

windowed m = foldr (zipWith (:)) (repeat []) . take m . tail
