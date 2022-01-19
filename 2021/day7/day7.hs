import Data.Char (digitToInt, isDigit)
import qualified Data.List as List
import qualified Data.Ord as Ord

main :: IO ()
main = do
  contents <- getContents
  let intList = toIntList contents
  let means = [f . mean $ l | f <- [floor, ceiling], let l = intList]
  let res =
        minimum
          [sum . map (f m) $ intList | m <- means, let f = distanceFromPos]
  print ("res=", res)

mean :: (Num a, Integral a) => [a] -> Float
mean list = sum asFracs / fromIntegral (length asFracs)
  where
    asFracs = map fromIntegral list :: [Float]

medianWith :: (Num a, Integral a) => (a -> a) -> [a] -> (a, a)
medianWith func list = middleElt sorted
  where
    sorted = List.sortOn snd elts
    funcApplied = List.map func list
    elts = zip list funcApplied

middleElt :: [a] -> a
middleElt list = list !! (div len 2)
  where
    len = length list

-- NOTE: Change for part 2
distanceFromPos :: (Num a, Integral a) => a -> a -> a
distanceFromPos pos = distModel . abs . subtract pos

-- distanceFromPos pos = abs . subtract pos
distModel :: (Num a, Integral a) => a -> a
distModel num = (num * (num + 1)) `div` 2

toIntList :: String -> [Int]
toIntList =
  map read .
  words .
  map
    (\c ->
       if c == ','
         then ' '
         else c)
-- NOTE: I thought the mode was what was needed, turns out it's the median.
-- The code below does work though.
-- import qualified Data.Map as Map
-- mode :: (Ord a, Num a) => [a] -> a
-- mode = fst . List.maximumBy (Ord.comparing snd) . Map.toList . modalities
-- modalities :: Ord k => Num a => [k] -> Map.Map k a
-- modalities = Map.fromListWith (+) . map (\a -> (a, 1))
