import Control.Monad
import Data.Char (digitToInt, isDigit)
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord
import qualified Data.Set as Set
import qualified Data.Tuple as Tuple

main :: IO ()
main = do
  contents <- getContents
  let res =
        List.sort . Maybe.catMaybes . map missingParenScores . lines $ contents
  let med = res !! (length res `div` 2)
  print med

missingParenScores :: String -> Maybe Int
missingParenScores parens = do
  pruned <- pruneMatches parens
  neededParens <- sequence $ map (lfun parenInfos) pruned
  scores <- sequence . map (lfun closeParenScores) $ neededParens
  return $ foldl (\acc score -> (acc * 5) + score) 0 scores
  where
    lfun aMap = (flip Map.lookup) aMap

pruneMatches :: String -> InspectState
pruneMatches line = res
  where
    res = foldM (handleParen) [] line

handleParen :: [Char] -> Char -> InspectState
handleParen curStack c = do
  let isClose = Map.lookup c closeParenScores
  case (isClose, curStack) of
    (Just score, []) -> Nothing
    (Just score, x:xs) ->
      let closing = Map.lookup x parenInfos
       in if closing == Just c
            then return xs
            else Nothing
    (Nothing, _) -> return (c : curStack)

parenInfos = Map.fromList parensPairs
  where
    parensPairs = [('(', ')'), ('{', '}'), ('<', '>'), ('[', ']')]

closeParenScores = Map.fromList [(')', 1), (']', 2), ('}', 3), ('>', 4)]

type InspectState = Maybe String
