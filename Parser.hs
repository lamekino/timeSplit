module Parser where

import qualified Control.Exception as E
import Data.Bifunctor (bimap)
import Data.Char (isAlpha)

type Title = String

data Timestamp
  = Hour Int Int Int
  | Minute Int Int
  deriving (Show, Eq, Ord)

wordsP :: (Char -> Bool) -> String -> [String]
wordsP p xs = words [if p x then ' ' else x | x <- xs]

-- FIXME: make assert more... part of program? idk
splitTimes :: String -> [Int]
splitTimes = validts . map read . wordsP (== ':')
  where
    validts xs = E.assert (all (< 60) $ drop 1 xs) xs

timestampOf :: [Int] -> Maybe Timestamp
timestampOf [x, y, z] = Just $ Hour x y z
timestampOf [x, y] = Just $ Minute x y
timestampOf _ = Nothing

parseInput :: ([String], [String]) -> ([Maybe Timestamp], [Title])
parseInput = bimap (map tsFromString) (map cleanString)
  where
    cleanString = dropWhile (not . isAlpha)
    tsFromString = timestampOf . splitTimes
