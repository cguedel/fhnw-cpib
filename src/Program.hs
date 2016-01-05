import Scanner
import Parser
import StaticAnalysis
import CodeGenerator

import System.Environment
import System.Directory

main :: IO()
main = getArgs >>= parse

parse :: [String] -> IO ()
parse (fIn : fOut : _) = do
  exists <- doesFileExist fIn
  if exists then
    do
      contents <- readFile fIn
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

        let (ctx, code) = genCode analyzed
        let indexed = zip code [0..]
        let numbered = map (\(c, i) -> "[" ++ show i ++ "] " ++ show c) indexed
        putStrLn "-------- Generated instructions --------"
        mapM_ putStrLn numbered

        putStrLn "-------- Context --------"
        print ctx

        putStrLn ("-------- Writing generated code to file " ++ fOut ++ "--------")
        writeFile fOut $ unlines $ map show code
  else
    error "File does not exist"
parse _ = error "Invalid cmdline arguments, usage: iml.exe <file> <output>"
