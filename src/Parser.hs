module Parser where

import qualified Control.Exception as E
import Data.Bifunctor (bimap)
import Data.Char (isAlpha, isDigit)
import Types

type Tokens = ([String], [String])

timestampOf :: [Int] -> Timestamp
timestampOf [x, y, z] =
  case [x, y, z] of
    [0, 0, 0] -> Beginning
    [0, 0, a] -> Second a
    [0, a, b] -> Minute a b
    [a, b, c] -> Hour a b c
timestampOf [x, y] = timestampOf [0, x, y]
timestampOf xs = error ("Failed to read timestamp " ++ show xs)

-- TODO: make parsed = ([ts], [Song])
type Parsed = ([Timestamp], [String])

wordsP :: (Char -> Bool) -> String -> [String]
wordsP p xs = words [if p x then ' ' else x | x <- xs]

-- FIXME: make assert more... part of program? idk
splitTimes :: String -> [Int]
splitTimes = validts . map read . wordsP (== ':')
  where
    validts xs = E.assert (all (< 60) $ drop 1 xs) xs

-- Take the input, make the lines, split when the end of timestamp is reached
-- finally, unzip the list of tuples
tokenize :: String -> Tokens
tokenize = unzip . map splitLine . lines
  where
    splitLine = span (\ch -> isDigit ch || ch == ':')

-- TODO: make this give a pair of [ts], [song]
parse :: Tokens -> Parsed
parse = bimap (map tsFromString) (map getTitle)
  where
    getTitle = dropWhile (not . isAlpha)
    tsFromString = timestampOf . splitTimes
