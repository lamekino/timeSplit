module Audio where

import Control.Monad (void)
import Data.List (zip4)
import System.Process (runCommand)
import Types

type Seconds = Int

type InputFile = FilePath

type OutputFile = FilePath

type Extension = String

-- | Convert Timestamp to a raw second value
fromTimestamp :: Timestamp -> Seconds
fromTimestamp ts =
  case ts of
    (Hour h m s) -> h * 3600 + m * 60 + s
    (Minute m s) -> m * 60 + s
    (Second x) -> x
    Beginning -> 0

-- | Creates output file from a song, given a file extension
outputFile :: Extension -> Song -> FilePath
outputFile ext = flip (++) ext . show

-- | Gives the differences of adjacencent timestamps
timeDifferences :: [Timestamp] -> [Seconds]
timeDifferences ts = foldr (\(x, y) a -> y - x : a) [] (pair times)
  where
    pair xs = zip (0 : xs) xs
    times = map fromTimestamp ts

-- | Runs FFmpeg, see:
-- https://newbedev.com/using-ffmpeg-to-cut-audio-from-to-position
-- WARN: this uses SOOO much CPU it should honestly crash my computer
-- TODO: use `createProcess` instead, so stdout is hidden
-- TODO: make this native instead of calling external executable
runFFmpeg :: (InputFile, OutputFile, Int, Int) -> IO ()
runFFmpeg (input, output, seek, len) =
  guard $
    "ffmpeg -c mp3"
      ++ " -ss "
      ++ show seek
      ++ " -i "
      ++ show input
      ++ " -to "
      ++ show len
      ++ " "
      ++ show (outputDir ++ output)
  where
    guard = if not working then putStrLn else void . runCommand
    working = True -- dummy variable
    outputDir = "data/out/"

-- FIXME: append the ending time to the list of time in timedifferences
splitFile :: [Metadata] -> InputFile -> Timestamp -> IO ()
splitFile ms input endTime =
  mapM_ runFFmpeg $
    zip4
      (repeat input)
      (map (outputFile inputExt) songs)
      (map fromTimestamp times)
      (timeDifferences times')
  where
    times = map fst ms
    times' = drop 1 times ++ [endTime]
    songs = map snd ms
    -- TODO: make extension match input
    inputExt = ".mp3"
