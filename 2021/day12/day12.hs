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
  print(contents)

getGraph :: String -> Graph
getGraph contents = Map.empty

type Graph = Map.Map String (Set.Set String) 
