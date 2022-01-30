import Data.Char (digitToInt, isDigit)
import qualified Data.List as List
import qualified Data.Ord as Ord

main :: IO ()
main = do
  contents <- getContents
  let lineWords = map words . lines $ contents
  let measurements = (map lineToMeasurement) $ lineWords
  let outputs = map output measurements
  let total = sum . map numUniq $ outputs
  print total

numUniq l = length . filter matchDigitPattern $ l

matchDigitPattern pat
  | numUniq == 2 || numUniq == 3 || numUniq == 4 || numUniq == 7 = True
  | otherwise = False
  where
    numUniq = length . List.nub $ pat

lineToMeasurement line =
  let tup = break (== "|") line
   in Measurement {patterns = fst tup, output = drop 1 $ snd tup}

data Measurement =
  Measurement
    { patterns :: [String]
    , output :: [String]
    }
  deriving (Show)
