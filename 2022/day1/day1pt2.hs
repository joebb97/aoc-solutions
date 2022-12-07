import Data.List

main = do
  contents <- getContents
  let elfLists =
        map (map read) . split (== "") . lines $ contents :: [[Int]]
  let biggest = 
        sum . take 3 . sortBy (flip compare) . map sum $ elfLists
  print biggest

split :: (Eq a) => (a -> Bool) -> [a] -> [[a]]
split _ [] = []
split pred strings = part : split pred newStrings
  where
    leftover = dropWhile pred strings
    (part, newStrings) = break pred leftover
