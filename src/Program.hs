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
        putStrLn "-------- Tokens --------"
        mapM_ print tokens

        let aSynTree = parser tokens
        putStrLn "-------- Abstract syntax tree --------"
        print aSynTree

        let analyzed = analyze aSynTree
        putStrLn "-------- Type checked abstract syntax tree --------"
        print analyzed

        let code = genCode analyzed
        putStrLn "-------- Generated instructions --------"
        mapM_ putStrLn code
  else
    error "File does not exist"
