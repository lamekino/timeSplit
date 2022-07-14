module Types where

import Data.List (intercalate)

-- TODO: make this a dataclass which included the end timestamp
type Metadata = (Timestamp, Song)

data Timestamp
  = Hour Int Int Int
  | Minute Int Int
  | Second Int
  | Beginning
  deriving (Eq, Ord)

repTs :: [Int] -> String
repTs = intercalate ":" . map (show' 10)
  where
    show' lb n = if n >= lb then show n else "0" ++ show n

instance Show Timestamp where
  show (Hour x y z) = "Hour: " ++ repTs [x, y, z]
  show (Minute x y) = "Minute: " ++ repTs [x, y]
  show (Second x) = "Second: " ++ repTs [0, x]
  show Beginning = "Beginning: 00:00"

data Song = Song
  { artist :: Maybe String,
    title :: String,
    number :: Int
  }
  deriving (Eq)

instance Show Song where
  show Song {artist = Nothing, title = name, number = n} =
    show n ++ ". " ++ name
  show Song {artist = Just person, title = name, number = n} =
    show n ++ ". " ++ person ++ " - " ++ name
