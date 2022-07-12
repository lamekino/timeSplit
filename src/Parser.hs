module Parser
  ( parse,
    tokenize,
    Tokens,
    Parsed,
  )
where

import Data.Bifunctor (bimap)
import Data.Char (isAlpha, isDigit)
import Types

type Tokens = ([String], [String])

-- TODO: make parsed = ([ts], [Song])
type Parsed = ([Timestamp], [String])

-- | Split into words on a predicate
wordsP :: (Char -> Bool) -> String -> [String]
wordsP p xs = words [if p x then ' ' else x | x <- xs]

-- | Make a timestamp formated like "HH:MM:SS" into an array of integers.
-- Ensures all minutes and seconds are < 60
splitTimes :: String -> [Int]
splitTimes t = validate $ map read $ wordsP (== ':') t
  where
    validate xs =
      if all (< 60) $ drop 1 xs
        then xs
        else error ("Invalid timestamp: " ++ t)

-- | Take the input, make the lines, split when the end of timestamp is reached
-- finally, unzip the list of tuples
tokenize :: String -> Tokens
tokenize = unzip . map splitLine . lines
  where
    splitLine = span (\ch -> isDigit ch || ch == ':')

-- | Takes an array of size 2 or 3 and gives a Timestamp
timestampOf :: [Int] -> Timestamp
timestampOf [x, y, z] =
  case [x, y, z] of
    [0, 0, 0] -> Beginning
    [0, 0, a] -> Second a
    [0, a, b] -> Minute a b
    [a, b, c] -> Hour a b c
timestampOf [x, y] = timestampOf [0, x, y]
timestampOf xs = error ("Failed to read timestamp: " ++ show xs)

-- TODO: make this give a pair of [ts], [song]
parse :: Tokens -> Parsed
parse = bimap (map tsFromString) (map getTitle)
  where
    getTitle = dropWhile (not . isAlpha)
    tsFromString = timestampOf . splitTimes
