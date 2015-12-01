module Scanner(tokenize) where

  import Prelude hiding (LT, GT, EQ)
  import IML
  import Data.Char

  tokenize :: String -> [Token]
  tokenize cs = s0 (cs, 1, [])

  s0 :: (String, Int, [Token]) -> [Token]

  -- RelOpr
  s0 ('<' : '=' : cs, l, accu) = s0 (cs, l, (RELOPR, Just (OprAttrib LE)) : accu)
  s0 ('<'       : cs, l, accu) = s0 (cs, l, (RELOPR, Just (OprAttrib LT)) : accu)
  s0 ('>' : '=' : cs, l, accu) = s0 (cs, l, (RELOPR, Just (OprAttrib GE)) : accu)
  s0 ('>'       : cs, l, accu) = s0 (cs, l, (RELOPR, Just (OprAttrib GT)) : accu)
  s0 ('/' : '=' : cs, l, accu) = s0 (cs, l, (RELOPR, Just (OprAttrib NE)) : accu)
  s0 ('='       : cs, l, accu) = s0 (cs, l, (RELOPR, Just (OprAttrib EQ)) : accu)

  -- BoolOpr
  s0 ('&' : '?' : cs, l, accu) = s0 (cs, l, (BOOLOPR, Just (OprAttrib CAND)) : accu)
  s0 ('|' : '?' : cs, l, accu) = s0 (cs, l, (BOOLOPR, Just (OprAttrib COR)) : accu)

  -- MultOpr
  s0 ('*' : cs, l, accu) = s0 (cs, l, (MULTOPR, Just (OprAttrib TIMES)) : accu)

  -- AddOpr
  s0 ('+' : cs, l, accu) = s0 (cs, l, (ADDOPR, Just (OprAttrib PLUS)) : accu)
  s0 ('-' : cs, l, accu) = s0 (cs, l, (ADDOPR, Just (OprAttrib MINUS)) : accu)

  -- Symbols
  s0 (':' : '=' : cs, l, accu) = s0 (cs, l, (BECOMES, Nothing) : accu)
  s0 (':'       : cs, l, accu) = s0 (cs, l, (COLON, Nothing) : accu)
  s0 (';'       : cs, l, accu) = s0 (cs, l, (SEMICOLON, Nothing) : accu)
  s0 (','       : cs, l, accu) = s0 (cs, l, (COMMA, Nothing) : accu)
  s0 ('('       : cs, l, accu) = s0 (cs, l, (LPAREN, Nothing) : accu)
  s0 (')'       : cs, l, accu) = s0 (cs, l, (RPAREN, Nothing) : accu)

  -- Line breaks
  s0 ('\r' : '\n' : cs, l, accu) = s0 (cs, l + 1, accu)
  s0 ('\n'        : cs, l, accu) = s0 (cs, l + 1, accu)
  s0 ('\r'        : cs, l, accu) = s0 (cs, l + 1, accu)

  -- Catch All
  s0 (c : cs, l, accu)
    | isAlpha c = s0 (s1 (cs, [c], l, accu))
    | isDigit c = s0 (s2 (cs, digitToInt c, l, accu))
    | isSpace c = s0 (cs, l, accu)
    | otherwise = error("Lexical error on line " ++ show l ++ ", cannot tokenize '" ++ [c] ++ "'")

  s0 ([], _, accu) = reverse ((SENTINEL, Nothing) : accu)

  -- Identifiers
  s1 :: (String, String, Int, [Token]) -> (String, Int, [Token])
  s1 (c:cs, accu', l, accu)
    | isAlphaNum c = s1 (cs, accu' ++ [c], l, accu)
    | otherwise = (c:cs, l, s4 accu' : accu)
  s1 ([], accu', l, accu) = ([], l, s4 accu' : accu)

  -- Integer literals
  s2 :: (String, Int, Int, [Token]) -> (String, Int, [Token])
  s2 ('\'' : cs, accu', l, accu) = s2 (cs, accu', l, accu)
  s2 ('/'  : cs, accu', l, accu) = s3 (cs, accu', 0, l, accu)
  s2 (c : cs, accu', l, accu)
    | isDigit c = s2 (cs, 10 * accu' + digitToInt c, l, accu)
    | otherwise = (c : cs, l, (LITERAL, Just (ALitAttrib accu')) : accu)
  s2([], accu', l, accu) = ([], l, (LITERAL, Just (ALitAttrib accu')) : accu)

  -- Ratio literals
  s3 :: (String, Int, Int, Int, [Token]) -> (String, Int, [Token])
  s3 ('\'' : cs, num, denum, l, accu) = s3 (cs, num, denum, l, accu)
  s3 (c : cs, num, denum, l, accu)
    | isDigit c = s3 (cs, num, 10 * denum + digitToInt c, l, accu)
    | otherwise = (c : cs, l, (LITERAL, Just (RLitAttrib num denum)) : accu)
  s3([], num, denum, l, accu) = ([], l, (LITERAL, Just (RLitAttrib num denum)) : accu)

  -- Keywords
  s4 :: String -> Token
  s4 key =
    case key of
      "bool"        -> (TYPE,       Just (TypeAttrib BOOL))
      "call"        -> (CALL,       Nothing)
      "ceil"        -> (RATIOOPR,   Just (OprAttrib CEIL))
      "const"       -> (CHANGEMODE, Just (ChangeModeAttrib CONST))
      "copy"        -> (MECHMODE,   Just (MechModeAttrib COPY))
      "debugin"     -> (DEBUGIN,    Nothing)
      "debougout"   -> (DEBUGOUT,   Nothing)
      "denum"       -> (RATIOOPR,   Just (OprAttrib DENUM))
      "divE"        -> (MULTOPR,    Just (OprAttrib DIV_E))
      "do"          -> (DO,         Nothing)
      "else"        -> (ELSE,       Nothing)
      "endfun"      -> (ENDFUN,     Nothing)
      "endif"       -> (ENDIF,      Nothing)
      "endproc"     -> (ENDPROC,    Nothing)
      "endprogram"  -> (ENDPROGRAM, Nothing)
      "endwhile"    -> (ENDWHILE,   Nothing)
      "false"       -> (LITERAL,    Just (BLitAttrib False))
      "floor"       -> (RATIOOPR,   Just (OprAttrib FLOOR))
      "fun"         -> (FUN,        Nothing)
      "global"      -> (GLOBAL,     Nothing)
      "if"          -> (IF,         Nothing)
      "init"        -> (INIT,       Nothing)
      "int"         -> (TYPE,       Just (TypeAttrib INT))
      "local"       -> (LOCAL,      Nothing)
      "modE"        -> (MULTOPR,    Just (OprAttrib MOD_E))
      "not"         -> (NOTOPR,     Nothing)
      "num"         -> (RATIOOPR,   Just (OprAttrib NUM))
      "proc"        -> (PROC,       Nothing)
      "program"     -> (PROGRAM,    Nothing)
      "ratio"       -> (TYPE,       Just (TypeAttrib RATIO))
      "ref"         -> (MECHMODE,   Just (MechModeAttrib REF))
      "returns"     -> (RETURNS,    Nothing)
      "round"       -> (RATIOOPR,   Just (OprAttrib ROUND))
      "skip"        -> (SKIP,       Nothing)
      "then"        -> (THEN,       Nothing)
      "true"        -> (LITERAL,    Just (BLitAttrib True))
      "var"         -> (CHANGEMODE, Just (ChangeModeAttrib VAR))
      "while"       -> (WHILE,      Nothing)
      _             -> (IDENT,      Just (IdentAttrib key))
