module Parser
  ( parse,
    tokenize,
    tsFromString,
  )
where

import Data.Bifunctor (bimap, second)
import Data.Char (isAlpha, isDigit)
import Data.List.Split (splitOn)
import Types

type Token = (String, String)

-- | Make a timestamp formated like "HH:MM:SS" into an array of integers.
-- Ensures all minutes and seconds are < 60
splitTimes :: String -> [Int]
splitTimes = validate . map read . splitOn ":"
  where
    validate xs =
      if all (< 60) $ drop 1 xs
        then xs
        else error ("Invalid timestamp: " ++ repTs xs)

-- | Take the input, make the lines, split when the end of timestamp is reached
-- finally, unzip the list of tuples
tokenize :: String -> [Token]
tokenize = map (second clean . splitTitle) . lines
  where
    -- split the title when the timestamp ends
    splitTitle = span (\ch -> isDigit ch || ch == ':')
    -- remove the leading whitespace from title
    -- TODO: remove tailing whitespace
    clean = dropWhile (not . isAlpha)

-- | Takes an array of size 2 or 3 and gives a Timestamp
timestampOf :: [Int] -> Timestamp
timestampOf [x, y, z] =
  case [x, y, z] of
    [0, 0, 0] -> Beginning
    [0, 0, a] -> Second a
    [0, a, b] -> Minute a b
    [a, b, c] -> Hour a b c
timestampOf [x, y] = timestampOf [0, x, y]
timestampOf xs = error ("Failed to read timestamp: " ++ repTs xs)

-- | Converts a "HH:MM:SS" string to a Timestamp
tsFromString :: String -> Timestamp
tsFromString = timestampOf . splitTimes

-- | Attempts to get the artist info from a song title, if it is separated by
-- `sep`
getTitle :: String -> Int -> String -> Song
getTitle sep n cs =
  case splitOn sep cs of
    [a, t] ->
      -- Artist, title found
      Song
        { number = n,
          title = t,
          artist = Just a
        }
    _ ->
      -- Anything else gets treated as not found
      Song
        { number = n,
          title = cs,
          artist = Nothing
        }

-- | Parses a list of tokens and gives a list of pairs of timestamps with songs
parse :: String -> [Token] -> [Metadata]
parse sep = zipWith parseToken [1 ..]
  where
    -- parses a (String Timestamp, String Title) -> (Timestamp, Song)
    parseToken idx tk = bimap tsFromString (getTitle sep idx) tk
