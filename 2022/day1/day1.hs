main = do
  contents <- getContents
  let elfLists =
        map (map read) . split (== "") . lines $ contents :: [[Int]]
  let biggest = 
        foldr1 max . map sum $ elfLists
  print biggest

split :: (Eq a) => (a -> Bool) -> [a] -> [[a]]
split _ [] = []
split pred strings = part : split pred newStrings
  where
    leftover = dropWhile pred strings
    (part, newStrings) = break pred leftover
