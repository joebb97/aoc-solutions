import Control.Monad
import Data.Char (digitToInt, isDigit)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord
import qualified Data.Set as Set
import qualified Data.Tuple as Tuple
import qualified Data.Either as Either

main :: IO ()
main = do
  contents <- getContents
  let res = sum . Either.lefts $ map badPoints $ lines contents
  print res

badPoints :: String -> InspectState
badPoints line = res
  where
    res = foldM (handleParen) [] line

handleParen :: [Char] -> Char -> InspectState
handleParen curStack c = do
  let isClose = Map.lookup c closeParenScores
  case (isClose, curStack) of
    (Just score, []) -> Left score
    (Just score, x:xs) -> 
        let closing = Map.lookup x parenInfos
        in if closing == Just c then
            return xs
        else
            Left score
    (Nothing, _) -> return (c:curStack)

parenInfos = Map.fromList parensPairs
  where
    parensPairs = [('(', ')'), ('{', '}'), ('<', '>'), ('[', ']')]

closeParenScores = Map.fromList [(')', 3), (']', 57), ('}', 1197), ('>', 25137)]
    

-- Current stack
type NoMismatch =
  [Char]

-- Number of points
type Mismatch =
  Int

type InspectState = Either Mismatch NoMismatch
