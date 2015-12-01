import Scanner
import System.Environment
import System.Directory

main :: IO()
main = getArgs >>= parse

parse :: [String] -> IO ()
parse [] = error "Invalid cmdline arguments, usage: iml.exe <file>"
parse f = do
  exists <- doesFileExist $ head f
  if exists then
    do
      contents <- readFile $ head f
      print $ tokenize contents
  else
    error "File does not exist"
