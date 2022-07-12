module Types where

data Timestamp
  = Hour Int Int Int
  | Minute Int Int
  | Second Int
  | Beginning
  deriving (Show, Eq, Ord)

data Song = Song
  { artist :: Maybe String,
    title :: String
  }
  deriving (Show, Eq)
