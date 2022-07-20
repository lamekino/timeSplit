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
helpExit code =
  let helpMsg =
        -- FIXME: write this
        "Something useful"
   in putStrLn helpMsg >> exitWith code

defaultSep :: String
defaultSep = " - "

processsArgs :: Arguements -> [String] -> Arguements
processsArgs arg cmdline =
  case cmdline of
    ("-h" : _) -> arg {helpFlag = True}
    -- TODO: is there a way to drop 'as' from this?
    -- TODO: make this less repetitive
    ("-o" : path : as) -> processsArgs arg {outputDirectory = Just path} as
    ("-S" : sep : as) -> processsArgs arg {songSeparator = Just sep} as
    ("-e" : stamp : as) -> processsArgs arg {endingTime = Just $ tsFromString stamp} as
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
  when (helpFlag args) $
    helpExit ExitSuccess

  -- required arguments
  let albumEnd = fromMaybe (error "required time") (endingTime args)
  let audioFile = fromMaybe (error "required audio file") (audioPath args)

  -- optional arguemnts
  let outputDir = fromMaybe "." (outputDirectory args)
  let songSep = fromMaybe defaultSep (songSeparator args)

  -- parse from either stdin or file depending on arguemnts
  parsed <-
    let parseWith = parseInput songSep
     in maybe
          (parseWith getContents)
          (parseWith . readFile)
          (timestampFile args)

  splitFile parsed audioFile albumEnd
