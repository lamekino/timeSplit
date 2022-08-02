import Audio
import Control.Monad (unless, when)
import Data.Maybe (fromMaybe)
import Parser
import System.Directory (doesDirectoryExist, doesFileExist)
import System.Environment (getArgs)
import System.Exit
import System.IO (hPutStr, stderr)
import Types

data HelpType
  = Pass
  | Requested
  | OnError
  deriving (Eq, Show)

data Arguements = Arguments
  { helpFlag :: HelpType,
    audioPath :: Maybe FilePath,
    endingTime :: Maybe Timestamp,
    timestampFile :: Maybe FilePath,
    outputDirectory :: Maybe FilePath,
    songSeparator :: Maybe String
  }
  deriving (Eq, Show)

putErr :: String -> IO ()
putErr = hPutStr stderr

helpExit :: ExitCode -> IO ()
helpExit code = puts helpMsg >> exitWith code
  where
    puts =
      if code == ExitSuccess
        then putStr
        else putErr
    helpMsg =
      unlines
        [ "timesplit usage:",
          "\t-l <timestamp>   sets the length of the song (required).",
          "\t-i <audio file>  the audio file to split (required).",
          "\t-d <dir>         set the output directory.",
          "\t-t <txt>         read a file with the timestamps/titles instead of stdin.",
          "\t-S <delim>       sets the separator between artist and song name.",
          "\t-h               print this help message and exit."
        ]

defaultSep :: String
defaultSep = " - "

processsArgs :: Arguements -> [String] -> Arguements
processsArgs arg cmdline =
  case cmdline of
    ("-h" : _) -> arg {helpFlag = Requested}
    -- TODO: is there a way to drop 'as' from this?
    -- TODO: make this less repetitive
    ("-S" : sep : as) -> processsArgs arg {songSeparator = Just sep} as
    ("-d" : path : as) -> processsArgs arg {outputDirectory = Just path} as
    ("-i" : path : as) -> processsArgs arg {audioPath = Just path} as
    ("-l" : stamp : as) -> processsArgs arg {endingTime = Just $ tsFromString stamp} as
    ("-t" : path : as) -> processsArgs arg {timestampFile = Just path} as
    _ -> arg {helpFlag = OnError}

-- | Takes the file contents of timestamps and titles and parses it
parseInput :: String -> IO String -> IO [Metadata]
parseInput sep inp = parse sep . tokenize <$> inp

main :: IO ()
main = do
  args <-
    processsArgs
      Arguments
        { helpFlag = Pass,
          audioPath = Nothing,
          endingTime = Nothing,
          timestampFile = Nothing,
          outputDirectory = Nothing,
          songSeparator = Nothing
        }
      <$> getArgs

  -- handle help arguement
  when (helpFlag args == Requested) $
    helpExit ExitSuccess

  when (helpFlag args == OnError) $
    helpExit $ ExitFailure 1

  -- required arguments
  let albumEnd = fromMaybe (error "required time") (endingTime args)
      audioFile = fromMaybe (error "required audio file") (audioPath args)

  -- optional arguemnts
  let outputDir = fromMaybe "." (outputDirectory args) ++ "/"
      songSep = fromMaybe defaultSep (songSeparator args)

  -- file checks
  -- TODO: bind this with unless
  outputExists <- doesDirectoryExist outputDir
  inputExists <- doesFileExist audioFile

  unless outputExists $
    die $ "directroy does not exist: " ++ outputDir

  unless inputExists $
    die $ "audio does not exist: " ++ audioFile

  -- parse from either stdin or file depending on arguemnts
  parsed <-
    let parseWith = parseInput songSep
     in maybe
          (parseWith getContents)
          (parseWith . readFile)
          (timestampFile args)

  splitFile parsed outputDir audioFile albumEnd
