import Data.Char (digitToInt, isDigit)
import System.Environment (getArgs)

main :: IO ()
main = do
  contents <- getContents
  args <- getArgs
  let numDays = (+ 1) $ read $ head args :: Int
  let intList = initialCounts $ toIntList contents
  let days = last $ take numDays $ iterate applyDay intList
  let asStr = show $ intList
  putStrLn $ "res = " ++ (show $ sum days)

daysIndices = [0 .. 8]

initialCounts :: [Int] -> [Int]
initialCounts arg = map (\pos -> (length $ only pos arg)) daysIndices

only :: Int -> [Int] -> [Int]
only x = filter (== x)

rotateLeft :: [Int] -> [Int]
rotateLeft list = xs ++ [x]
  where
    x:xs = list

applyDay :: [Int] -> [Int]
applyDay timers = zipWith updateTimers daysIndices rotated
  where
    numZeros = head timers
    rotated = rotateLeft timers
    updateTimers idx timer =
      if idx == 6
        then timer + numZeros
        else timer

toIntList :: String -> [Int]
toIntList = map digitToInt . filter isDigit
