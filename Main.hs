import Data.Char (isDigit)
import qualified Parser as P

prettyPrint :: (Show a, Show b) => ([a], [b]) -> IO ()
prettyPrint (x : xs, y : ys) = do
  putStrLn (show x ++ "\t" ++ show y)
  prettyPrint (xs, ys)
prettyPrint _ = return ()

-- Take the input, make the lines, split when the end of timestamp is reached
-- finally, unzip the list of tuples
tokenize :: String -> ([String], [String])
tokenize = unzip . map (span isTimestamp) . lines
  where
    isTimestamp ch = isDigit ch || ch == ':'

main :: IO ()
main = do
  input <- getContents
  let pairs = tokenize input
  prettyPrint $ P.parseInput pairs
