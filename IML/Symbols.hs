module IML.Symbols where

  import Prelude hiding (LT, GT)
  import qualified Data.Map as M

  import IML.Tokens
  import IML.Operators

  symbols :: M.Map String Token
  symbols = M.fromList [("/=", RELOPR NE),
                        (":=", BECOMES),
                        ("<=", RELOPR LE),
                        (">=", RELOPR GE),
                        ("<", RELOPR LT),
                        (">", RELOPR GT),
                        ("&&", BOOLOPR AND),
                        ("||", BOOLOPR OR),
                        ("&?", BOOLOPR CAND),
                        ("|?", BOOLOPR COR),
                        ("+", ADDOPR PLUS),
                        ("-", ADDOPR MINUS),
                        ("*", MULTOPR TIMES),
                        ("%", MULTOPR MOD),
                        ("/", MULTOPR DIV),
                        ("(", LPAREN),
                        (")", RPAREN),
                        (",", COMMA),
                        (";", SEMICOLON),
                        (":", COLON)]

  isSymbol :: Char -> Bool
  isSymbol c = c `elem` "(){}[],;:*%+-=/&|?!<>."

  makeSymbolToken :: String -> Token
  makeSymbolToken w = symbols M.! w
