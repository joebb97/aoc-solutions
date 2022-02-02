import Data.Char (digitToInt, isDigit)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Ord as Ord
import qualified Data.Set as Set
import qualified Data.Tuple as Tuple
import qualified Data.Maybe as Maybe

main :: IO ()
main = do
  contents <- getContents
  let board = getBoard contents
  -- mapM_ print $ Map.toList . tiles $ board
  let lowestPoints = lowPoints board
  print ("res=", lowestPoints, sum . map (+1) $ lowestPoints)

lowPoints :: Board -> [Int]
lowPoints board = Map.elems boardLowPoints
    where boardLowPoints = Map.filterWithKey (\coord val -> isLowPoint coord board) $ tiles board

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
    neighbors =
      [ (x + xdiff, y + ydiff)
      | let eachDir = [(1, 0), (-1, 0), (0, 1), (0, -1)]
      , let (x, y) = coord
      , (xdiff, ydiff) <- eachDir
      ]
    boardTiles = tiles board
    thisTile = Map.lookup coord boardTiles
    allLess =
      all
        (\c ->
           let coordVal = Map.lookup c boardTiles
               emptyTile = Maybe.isNothing coordVal
            in coordVal > thisTile || emptyTile)
        neighbors

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
