import Scanner
import Parser

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
      let tokens = tokenize contents
      do
        mapM_ print tokens
        let cSynTree = parser tokens
        print cSynTree
  else
    error "File does not exist"
