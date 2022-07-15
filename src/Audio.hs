module Audio where

import Control.Monad (void)
import Data.List (zip4)
import qualified System.Process as P
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

-- | Gives the differences of adjacencent timestamps
timeDifferences :: [Timestamp] -> [Seconds]
timeDifferences ts = foldr (\(x, y) a -> y - x : a) [] (pair times)
  where
    pair xs = zip (0 : xs) xs
    times = map fromTimestamp ts

-- | Creates output file from a song, given a file extension
-- PERF: this runs O(n) whereas the rest of the functions on metadata are O(1)
outputFile :: Extension -> Song -> FilePath
outputFile ext = flip (++) ext . show

ffmpegCmd :: (InputFile, OutputFile, Int, Int) -> (FilePath, [String])
ffmpegCmd (input, output, seek, len) =
  ( "ffmpeg",
    [ "-c",
      "mp3",
      "-ss",
      show seek,
      "-i",
      input, -- TODO: make this a realpath
      "-to",
      show len,
      outputDir ++ output
    ]
  )
  where
    -- TODO: make this user input
    outputDir = "data/out/"

-- | Runs FFmpeg, see:
-- https://newbedev.com/using-ffmpeg-to-cut-audio-from-to-position
-- TODO: make this native instead of calling external executable
runFFmpeg :: (InputFile, OutputFile, Int, Int) -> IO ()
runFFmpeg = run' . ffmpegCmd
  where
    -- Guards for running
    working :: Bool
    working = True

    -- Boiler plate for now
    run' :: (FilePath, [String]) -> IO ()
    run' xs =
      if not working
        then (print . runNothing) xs
        else void . P.waitForProcess =<< run xs

    -- Makes it so a process runs without outputing to the console
    silenceOutput :: P.CreateProcess -> P.CreateProcess
    silenceOutput p =
      p
        { P.std_err = P.NoStream -- NOTE: FFmpeg prints to stderr, why idk
        }

    -- Guard for printing out the resulting process of `run`
    -- TODO: remove guard to make this the default action of this function
    runNothing :: (FilePath, [String]) -> P.CreateProcess
    runNothing = silenceOutput . uncurry P.proc

    run :: (FilePath, [String]) -> IO P.ProcessHandle
    run args =
      (\(_, _, _, h) -> h) -- drop everything but the handle
        <$> (P.createProcess . silenceOutput . uncurry P.proc) args

splitFile :: [Metadata] -> InputFile -> Timestamp -> IO ()
splitFile ms input endTime =
  -- PERF: this in total is O(n^4), can be brought to O(n^3) with outputFile optimization
  mapM_ runFFmpeg $
    zip4 -- O(n) (all the lists are the same size)
      (repeat input) -- O(1)
      (map (outputFile inputExt) songs) -- O(n^2)...?
      (map fromTimestamp times) -- O(n)
      (timeDifferences times') -- O(n)
  where
    times = map fst ms
    times' = drop 1 times ++ [endTime] -- shift the list with the last timestamp
    songs = map snd ms
    -- TODO: make extension match input
    inputExt = ".mp3"
