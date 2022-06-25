module TimeSplit where

import Data.Char

type Title = String

data Timestamp
  = Hour Int Int Int
  | Minute Int Int
  deriving (Show, Eq, Ord)

isTimestamp :: Char -> Bool
isTimestamp ch = isDigit ch || ch == ':'

wordsP :: (Char -> Bool) -> String -> [String]
wordsP p xs = words [if p x then ' ' else x | x <- xs]

-- TODO: make sure each item between : is correct length
splitTimes :: String -> [Int]
splitTimes = map read . wordsP (== ':')

timestampOf :: [Int] -> Maybe Timestamp
timestampOf [x, y, z] = Just $ Hour x y z
timestampOf [x, y]    = Just $ Minute x y
timestampOf _         = Nothing

biTupleMap :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
biTupleMap f g (a, b) = (f a, g b)

-- TODO: optimize this (have no clue what the complexity is of this)
tokenizeInput :: ([String], [String]) -> ([Maybe Timestamp], [Title])
tokenizeInput = biTupleMap (map tsFromString) (map cleanString)
  where
    cleanString = dropWhile (not . isAlpha)
    tsFromString = timestampOf . splitTimes

prettyPrint :: ([Maybe Timestamp], [Title]) -> IO ()
prettyPrint (x:xs, y:ys) = do
  putStrLn (show x ++ "\t" ++ show y)
  prettyPrint (xs, ys)
prettyPrint _ = return ()

main :: IO ()
main = do
  input <- getContents
  let pairs = unzip $ map (span isTimestamp) $ lines input
  prettyPrint $ tokenizeInput pairs
