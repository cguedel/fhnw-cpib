module IML.Keywords where

  import qualified Data.Map as M
  import Data.Maybe

  import IML.Tokens
  import IML.Types
  import IML.ChangeMode
  import IML.MechMode
  import IML.Operators
  import IML.Value
  import IML.FlowMode

  keywords :: M.Map String Token
  keywords = M.fromList [("bool", TYPE BOOL),
              ("call", CALL),
              ("const", CHANGEMODE CONST),
              ("copy", MECHMODE COPY),
              ("debugin", DEBUGIN),
              ("debugout", DEBUGOUT),
              ("div", MULTOPR DIV),
              ("do", DO),
              ("else", ELSE),
              ("endfun", ENDFUN),
              ("endif", ENDIF),
              ("endproc", ENDPROC),
              ("endprogram", ENDPROGRAM),
              ("endwhile", ENDWHILE),
              ("false", LITERAL (BoolVal False)),
              ("fun", FUN),
              ("global", GLOBAL),
              ("if", IF),
              ("in", FLOWMODE IN),
              ("init", INIT),
              ("inout", FLOWMODE INOUT),
              ("int32", TYPE INT32),
              ("local", LOCAL),
              ("mod", MULTOPR MOD),
              ("not", NOT),
              ("out", FLOWMODE OUT),
              ("proc", PROC),
              ("program", PROGRAM),
              ("ref", MECHMODE REF),
              ("returns", RETURNS),
              ("skip", SKIP),
              ("then", THEN),
              ("true", LITERAL (BoolVal True)),
              ("var", CHANGEMODE VAR),
              ("while", WHILE)]

  isKeyword :: String -> Bool
  isKeyword w = isJust $ M.lookup w keywords

  makeKeywordToken :: String -> Token
  makeKeywordToken w = keywords M.! w
