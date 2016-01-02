import Scanner
import Parser
import StaticAnalysis
import CodeGenerator

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
        print "-------- Tokens --------"
        mapM_ print tokens

        let aSynTree = parser tokens
        print "-------- Abstract syntax tree --------"
        print aSynTree

        let analyzed = analyze aSynTree
        print "-------- Type checked abstract syntax tree --------"
        print analyzed

        let code = genCode analyzed
        print "-------- Generated instructions --------"
        mapM_ print code
  else
    error "File does not exist"
