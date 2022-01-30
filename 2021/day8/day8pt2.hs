import Data.Char (digitToInt, isDigit)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Ord as Ord
import qualified Data.Set as Set
import qualified Data.Tuple as Tuple
import Types

main :: IO ()
main = do
  contents <- getContents
  let measurements = getMeasurements contents
  let outDigits = map parseOutput measurements
  let res = fmap sum $ sequence outDigits
  print ("res=", res)

debugMeasurements =
  getMeasurements
    "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf\n"

parseOutput :: Measurement -> Maybe Int
parseOutput meas = do
  outDigits <- sequence digits
  let asStr = concat . map show $ outDigits
  return (read asStr)
  where
    mapping = solveWireMappings meas
    digits = map (applyMapping mapping) $ output meas

getMeasurements :: String -> [Measurement]
getMeasurements contents =
  let lineWords = map words . lines $ contents
      measurements = (map lineToMeasurement) $ lineWords
   in measurements

applyMapping :: FinalMapping -> String -> Maybe Int
applyMapping fm pat = do
  required <- sequence . map (\c -> Map.lookup c fm) $ pat
  let asSet = Set.fromList required
  let dig = head . Map.keys $ Map.filter (asSet ==) digitInfosSet
  return dig

solveWireMappings :: Measurement -> FinalMapping
solveWireMappings meas = finalMapping
  where
    (knowns, unknowns) = List.partition (matchDigitPattern) . patterns $ meas
    knownGuesses =
      pruneMappings $
      foldr (mergeWireMappings . head) initMappings $
      map possibleAssignments knowns
    unknownGuesses = map (possibleAssignments) unknowns
    solved =
      Map.toList $
      head $
      filter isSolved $
      map pruneMappings $
      foldl
        (\pas cur -> filter isValid $ mergeGuesses cur pas)
        [knownGuesses]
        unknownGuesses
    finalMapping = Map.fromList $ map (\(a, b) -> (head b, a)) solved

isValid :: WireMappings -> Bool
isValid wm = noneEmpty && (length lenOneStrs == length asSet)
  where
    lenOneStrs = filter ((== 1) . length) $ Map.elems wm
    asSet = Set.fromList lenOneStrs
    noneEmpty = all ((> 0) . length) $ Map.elems wm

pruneMappings :: WireMappings -> WireMappings
pruneMappings wm =
  snd $
  until
    (\(old, new) -> old == new)
    (\(_, b) -> (b, pruneMappingsOnce b))
    (wm, prunedwm)
  where
    prunedwm = pruneMappingsOnce wm

pruneMappingsOnce :: WireMappings -> WireMappings
pruneMappingsOnce wm = Map.fromList newList
  where
    asList = Map.toList wm
    allShortest = Set.fromList $ filter ((<= 2) . length) $ map snd asList
    filterShortest :: String -> WireMapping -> WireMapping
    filterShortest curShortest tup@(pos, pat)
      | length pat > shortestLen =
        (pos, filter (\c -> not (elem c curShortest)) pat)
      | otherwise = tup
      where
        shortestLen = length curShortest
    newList =
      Set.foldr (\cur acc -> map (filterShortest cur) acc) asList allShortest

isSolved :: WireMappings -> Bool
isSolved = all (\e -> length e == 1) . Map.elems

initMappings :: WireMappings
initMappings = Map.fromList mappings
  where
    mappings = [(pos, pat) | let pat = ['a' .. 'g'], pos <- allWirePos]

assignWire :: WireMappings -> WirePos -> Wire -> WireMappings
assignWire curMappings pos setTo = newMappings
  where
    newPair :: WirePos -> [Wire] -> [Wire]
    newPair k v =
      if k == pos
        then [setTo]
        else filter (/= setTo) v
    newMappings = Map.mapWithKey (newPair) curMappings

possibleDigitInfos :: String -> DigitInfos
possibleDigitInfos pat = ans
  where
    len = length pat
    ans = Map.filter (\v -> length v == len) digitInfos

possibleAssignments :: String -> [WireMappings]
possibleAssignments pat = map infoToAssignents lists
  where
    infos = possibleDigitInfos pat
    lists = Map.toList infos
    infoToAssignents :: DigitInfo -> WireMappings
    infoToAssignents info
        -- List.nub
     = Map.fromList [(pos, pat) | let (_, required) = info, pos <- required]

numUniq l = length . filter matchDigitPattern $ l

uniqDigitInfos = uniqRequired
  where
    uniqRequired = Map.filterWithKey isTarget digitInfos
    isTarget x _ = x == 2 || x == 3 || x == 4 || x == 7

matchDigitPattern pat = length pat `Map.member` uniqDigitInfos

digitInfos =
  Map.fromList
    [ (0, [Top, TopLeft, BotLeft, Bot, BotRight, TopRight])
    , (1, [TopRight, BotRight])
    , (2, [Top, TopRight, Mid, BotLeft, Bot])
    , (3, [Top, TopRight, Mid, BotRight, Bot])
    , (4, [TopLeft, Mid, TopRight, BotRight])
    , (5, [Top, TopLeft, Mid, BotRight, Bot])
    , (6, filter (/= TopRight) allWirePos)
    , (7, [Top, TopRight, BotRight])
    , (8, allWirePos)
    , (9, filter (/= BotLeft) allWirePos)
    ]

digitInfosSet = Map.map Set.fromList digitInfos

allWirePos :: [WirePos]
allWirePos = [minBound .. maxBound]

lineToMeasurement :: [String] -> Measurement
lineToMeasurement line =
  let tup = break (== "|") line
   in Measurement {patterns = fst tup, output = drop 1 $ snd tup}

mergeGuesses :: [WireMappings] -> [WireMappings] -> [WireMappings]
mergeGuesses x y = [mergeWireMappings l r | l <- x, r <- y]

mergeWireMappings :: WireMappings -> WireMappings -> WireMappings
mergeWireMappings x y = Map.unionWith (mergePossibleWires) x y

mergePossibleWires :: [Wire] -> [Wire] -> [Wire]
mergePossibleWires l r = filter (\w -> elem w r) l
