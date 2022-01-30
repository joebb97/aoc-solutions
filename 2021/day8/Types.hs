module Types where
import qualified Data.Map as Map

type Wire = Char

type FinalMapping = Map.Map Wire WirePos
type WireMappings = Map.Map WirePos [Wire]

type WireMapping = (WirePos, [Wire])

type DigitInfos = Map.Map Int [WirePos]

type DigitInfo = (Int, [WirePos])

data Measurement =
  Measurement
    { patterns :: [String]
    , output :: [String]
    }
  deriving (Show)

data WirePos
  = TopRight
  | BotRight
  | Mid
  | Bot
  | Top
  | BotLeft
  | TopLeft
  deriving (Show, Eq, Enum, Ord, Bounded)
