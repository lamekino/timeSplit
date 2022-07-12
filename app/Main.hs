import Audio ()
import Data.Maybe ()
import Parser as P
import qualified Sound.File.Sndfile as SND
import System.Environment (getArgs)

help :: String
help =
  "timesplit [audiofile] [timestamps]\n\
  \a program to split an audiofile"

-- | Takes the program arguments and gives the path to the audio and the path to
-- the timestamps
processArgs :: [String] -> (FilePath, Maybe FilePath)
processArgs args =
  case args of
    [x, y] -> (x, Just y)
    [x] -> (x, Nothing)
    [] -> error help
    _ -> error ("Invaild arguements " ++ unlines args)

-- | Takes the possible filepath and returns the parsed version
parseInput :: Maybe FilePath -> IO P.Parsed
parseInput fp = P.parse . P.tokenize <$> maybe getContents readFile fp

-- pp :: P.Parsed -> IO ()
-- pp (x : xs, y : ys) = do
--   putStrLn (show x ++ "\t" ++ show y)
--   pp (xs, ys)
-- pp _ = return ()

main :: IO ()
main = do
  (audioPath, timePath) <- processArgs <$> getArgs

  parsedTime <- parseInput timePath

  audioHandle <-
    SND.openFile audioPath SND.ReadMode
      =<< SND.getFileInfo audioPath

  SND.hClose audioHandle
