import Audio
import Control.Monad (when)
import Data.Maybe (fromJust, fromMaybe, isNothing)
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

defaultSep :: String
defaultSep = " - "

helpMsg :: String
helpMsg = "Something useful"

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
parseWith :: IO String -> IO [Metadata]
parseWith inp = parse defaultSep . tokenize <$> inp

main :: IO ()
main = do
  args <-
    processsArgs
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
  when (helpFlag args) $ do
    putStrLn helpMsg
    exitSuccess

  -- required arguments
  let albumEnd = fromMaybe (error "required time") (endingTime args)
  let audioFile = fromMaybe (error "required audio file") (audioPath args)

  -- optional arguemnts
  let outputDir = fromMaybe "." (outputDirectory args)
  let songSep = fromMaybe defaultSep (songSeparator args)

  -- parse from either stdin or file depending on arguemnts
  parsed <-
    maybe
      (parseWith getContents)
      (parseWith . readFile)
      (timestampFile args)

  splitFile parsed audioFile albumEnd

  return ()
