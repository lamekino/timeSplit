import Audio
import Control.Monad (when)
import Data.Maybe (fromMaybe)
import Parser
import System.Environment (getArgs)
import System.Exit
import Types

data Arguements = Arguments
  { helpFlag :: Bool,
    audioPath :: Maybe FilePath,
    endingTime :: Maybe Timestamp,
    timestampFile :: Maybe FilePath,
    outputDirectory :: Maybe FilePath,
    songSeparator :: Maybe String
  }
  deriving (Eq, Show)

helpExit :: ExitCode -> IO ()
helpExit code = putStrLn helpMsg >> exitWith code
  where
    helpMsg =
      unlines
        [ "timesplit usage:",
          "\t-l <timestamp in HH:MM:SS>  sets the length of the song (required).",
          "\t-i <audio file>             the audio file to split (required).",
          "\t-d <dir>                    set the output directory.",
          "\t-t <txt>                    read a file with the timestamps/titles instead of stdin.",
          "\t-S <delim>                  sets the separator between artist and song name.",
          "\t-h                          print this help message and exit."
        ]

defaultSep :: String
defaultSep = " - "

processsArgs :: Arguements -> [String] -> Arguements
processsArgs arg cmdline =
  case cmdline of
    ("-h" : _) -> arg {helpFlag = True}
    -- TODO: is there a way to drop 'as' from this?
    -- TODO: make this less repetitive
    ("-S" : sep : as) -> processsArgs arg {songSeparator = Just sep} as
    ("-d" : path : as) -> processsArgs arg {outputDirectory = Just path} as
    ("-i" : path : as) -> processsArgs arg {audioPath = Just path} as
    ("-l" : stamp : as) -> processsArgs arg {endingTime = Just $ tsFromString stamp} as
    ("-t" : path : as) -> processsArgs arg {timestampFile = Just path} as
    _ -> arg

-- | Takes the file contents of timestamps and titles and parses it
parseInput :: String -> IO String -> IO [Metadata]
parseInput sep inp = parse sep . tokenize <$> inp

main :: IO ()
main = do
  args <-
    processsArgs
      -- default arguements
      Arguments
        { helpFlag = False,
          audioPath = Nothing,
          endingTime = Nothing,
          timestampFile = Nothing,
          outputDirectory = Nothing,
          songSeparator = Nothing
        }
      <$> getArgs

  -- handle help arguement
  print args

  when (helpFlag args) $
    helpExit ExitSuccess

  -- required arguments
  let albumEnd = fromMaybe (error "required time") (endingTime args)
      audioFile = fromMaybe (error "required audio file") (audioPath args)

  -- optional arguemnts
  let outputDir = fromMaybe "." (outputDirectory args)
      songSep = fromMaybe defaultSep (songSeparator args)

  -- parse from either stdin or file depending on arguemnts
  parsed <-
    let parseWith = parseInput songSep
     in maybe
          (parseWith getContents)
          (parseWith . readFile)
          (timestampFile args)

  return ()

-- splitFile parsed audioFile albumEnd
