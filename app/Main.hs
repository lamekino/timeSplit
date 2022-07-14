import Audio
import Parser
import System.Environment (getArgs)
import Types

help :: String
help = "WIP"

defaultSep :: String
defaultSep = " - "

-- | Takes the program arguments and gives the path to the audio and the path to
-- the timestamps
processArgs :: [String] -> (FilePath, Maybe FilePath, Timestamp)
processArgs args =
  case args of
    [x, y, z] -> (x, Just z, tsFromString y)
    [x, y] -> (x, Nothing, tsFromString y)
    _ -> error ("Invaild arguements " ++ unlines args)

-- | Takes the possible filepath and returns the parsed version
parseInput ::
  (FilePath -> IO String) ->
  IO String ->
  Maybe FilePath ->
  IO [Metadata]
parseInput success _ (Just fp) = parse defaultSep . tokenize <$> success fp
parseInput _ fallback Nothing = parse defaultSep . tokenize <$> fallback

main :: IO ()
main = do
  (audioPath, timePath, endTime) <- processArgs <$> getArgs

  parsed <- parseInput readFile getContents timePath

  splitFile parsed audioPath endTime

  return ()
