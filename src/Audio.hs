module Audio where

import Control.Monad (void)
import Data.List (zip4)
import qualified System.Process as P
import Types

type Seconds = Int

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
-- outputFile :: Extension -> Song -> FilePath
-- outputFile ext = flip (++) ext . show
ffmpegCmd :: (FilePath, FilePath, Int, Int) -> (FilePath, [String])
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
runFFmpeg :: (FilePath, FilePath, Int, Int) -> IO ()
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
    -- NOTE: FFmpeg prints to stderr, why idk
    silenceOutput :: P.CreateProcess -> P.CreateProcess
    silenceOutput proc = proc {P.std_err = P.NoStream}

    -- Guard for printing out the resulting process of `run`
    -- TODO: remove guard to make this the default action of this function
    runNothing :: (FilePath, [String]) -> P.CreateProcess
    runNothing = silenceOutput . uncurry P.proc

    run :: (FilePath, [String]) -> IO P.ProcessHandle
    run args =
      (\(_, _, _, h) -> h) -- drop everything but the handle
        <$> (P.createProcess . silenceOutput . uncurry P.proc) args

splitFile :: [Metadata] -> FilePath -> FilePath -> Timestamp -> IO ()
splitFile ms output input endTime =
  mapM_ runFFmpeg $
    zip4
      (repeat input)
      (map (outputFrom output) songs)
      (map fromTimestamp times)
      (timeDifferences times')
  where
    times = map fst ms
    times' = drop 1 times ++ [endTime] -- shift the list with the last timestamp
    songs = map snd ms
    inputExt = ".mp3" -- TODO: make extension match input, find codec?
    -- FIXME: this is very lazy
    outputFrom :: FilePath -> Song -> FilePath
    outputFrom dir song = dir ++ (flip (++) inputExt . show) song
