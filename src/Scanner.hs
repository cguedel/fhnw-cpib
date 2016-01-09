module Scanner(tokenize) where

  import Prelude hiding (LT, GT, EQ)
  import IML
  import Data.Char

  tokenize :: String -> [Token]
  tokenize cs = s0 (cs, 1, [])

  isLineBreak :: Char -> Bool
  isLineBreak '\r' = True
  isLineBreak '\n' = True
  isLineBreak _ = False

  s0 :: (String, Int, [Token]) -> [Token]

  -- RelOpr
  s0 ('<' : '=' : cs, l, accu) = s0 (cs, l, (RELOPR, Just (RelOprAttrib LessEq), (l, 0)) : accu)
  s0 ('<'       : cs, l, accu) = s0 (cs, l, (RELOPR, Just (RelOprAttrib Less), (l, 0)) : accu)
  s0 ('>' : '=' : cs, l, accu) = s0 (cs, l, (RELOPR, Just (RelOprAttrib GreaterEq), (l, 0)) : accu)
  s0 ('>'       : cs, l, accu) = s0 (cs, l, (RELOPR, Just (RelOprAttrib Greater), (l, 0)) : accu)
  s0 ('/' : '=' : cs, l, accu) = s0 (cs, l, (RELOPR, Just (RelOprAttrib NotEq), (l, 0)) : accu)
  s0 ('='       : cs, l, accu) = s0 (cs, l, (RELOPR, Just (RelOprAttrib Equal), (l, 0)) : accu)

  -- BoolOpr
  s0 ('&' : '?' : cs, l, accu) = s0 (cs, l, (BOOLOPR, Just (BOprAttrib CAnd), (l, 0)) : accu)
  s0 ('|' : '?' : cs, l, accu) = s0 (cs, l, (BOOLOPR, Just (BOprAttrib Cor), (l, 0)) : accu)

  -- MultOpr
  s0 ('*' : cs, l, accu) = s0 (cs, l, (MULTOPR, Just (MultOprAttrib Times), (l, 0)) : accu)

  -- AddOpr
  s0 ('+' : cs, l, accu) = s0 (cs, l, (ADDOPR, Just (AddOprAttrib Plus), (l, 0)) : accu)
  s0 ('-' : cs, l, accu) = s0 (cs, l, (ADDOPR, Just (AddOprAttrib Minus), (l, 0)) : accu)

  -- Symbols
  s0 (':' : '=' : cs, l, accu) = s0 (cs, l, (BECOMES, Nothing, (l, 0)) : accu)
  s0 (':'       : cs, l, accu) = s0 (cs, l, (COLON, Nothing, (l, 0)) : accu)
  s0 (';'       : cs, l, accu) = s0 (cs, l, (SEMICOLON, Nothing, (l, 0)) : accu)
  s0 (','       : cs, l, accu) = s0 (cs, l, (COMMA, Nothing, (l, 0)) : accu)
  s0 ('('       : cs, l, accu) = s0 (cs, l, (LPAREN, Nothing, (l, 0)) : accu)
  s0 (')'       : cs, l, accu) = s0 (cs, l, (RPAREN, Nothing, (l, 0)) : accu)

  -- Line breaks
  s0 ('/'  : '/'  : cs, l, accu) = s0 (dropWhile (not . isLineBreak) cs, l, accu)

  -- Catch All
  s0 (c : cs, l, accu)
    | isAlpha c     = s0 (s1 (cs, [c], l, accu))
    | isDigit c     = s0 (s2 (cs, digitToInt c, l, accu))
    | isLineBreak c = s0 (cs, l + 1, accu)
    | isSpace c     = s0 (cs, l, accu)
    | otherwise     = error("Lexical error on line " ++ show l ++ ", cannot tokenize '" ++ [c] ++ "'")

  s0 ([], _, accu) = reverse ((SENTINEL, Nothing, (0, 0)) : accu)

  -- Identifiers
  s1 :: (String, String, Int, [Token]) -> (String, Int, [Token])
  s1 (c:cs, accu', l, accu)
    | isAlphaNum c = s1 (cs, accu' ++ [c], l, accu)
    | otherwise = (c:cs, l, s4 (accu', l, 0) : accu)
  s1 ([], accu', l, accu) = ([], l, s4 (accu', l, 0) : accu)

  -- Integer literals
  s2 :: (String, Int, Int, [Token]) -> (String, Int, [Token])
  s2 ('\'' : cs, accu', l, accu) = s2 (cs, accu', l, accu)
  s2 ('/'  : cs, accu', l, accu) = s3 (cs, accu', 0, l, accu)
  s2 (c : cs, accu', l, accu)
    | isDigit c = s2 (cs, 10 * accu' + digitToInt c, l, accu)
    | otherwise = (c : cs, l, (ALITERAL, Just (ALitAttrib accu'), (l, 0)) : accu)
  s2([], accu', l, accu) = ([], l, (ALITERAL, Just (ALitAttrib accu'), (l, 0)) : accu)

  -- Ratio literals
  s3 :: (String, Int, Int, Int, [Token]) -> (String, Int, [Token])
  s3 ('\'' : cs, num, denum, l, accu) = s3 (cs, num, denum, l, accu)
  s3 (c : cs, num, denum, l, accu)
    | isDigit c = s3 (cs, num, 10 * denum + digitToInt c, l, accu)
    | otherwise = (c : cs, l, (RLITERAL, Just (RLitAttrib num denum), (l, 0)) : accu)
  s3([], num, denum, l, accu) = ([], l, (RLITERAL, Just (RLitAttrib num denum), (l, 0)) : accu)

  -- Keywords
  s4 :: (String, Int, Int) -> Token
  s4 (key, l, c) = s5 (key, (l, c))

  s5 :: (String, Position) -> Token
  s5 (key, p) =
    case key of
      "asRatio"     -> (TYPECOPR,   Just (TypeAttrib RatioType), p)
      "bool"        -> (TYPE,       Just (TypeAttrib BoolType), p)
      "call"        -> (CALL,       Nothing, p)
      "ceil"        -> (RATIOOPR,   Just (ROprAttrib Ceil), p)
      "const"       -> (CHANGEMODE, Just (ChangeModeAttrib CONST), p)
      "copy"        -> (MECHMODE,   Just (MechModeAttrib COPY), p)
      "debugin"     -> (DEBUGIN,    Nothing, p)
      "debugout"    -> (DEBUGOUT,   Nothing, p)
      "denom"       -> (RATIOOPR,   Just (ROprAttrib Denom), p)
      "divE"        -> (MULTOPR,    Just (MultOprAttrib Div), p)
      "do"          -> (DO,         Nothing, p)
      "else"        -> (ELSE,       Nothing, p)
      "endfun"      -> (ENDFUN,     Nothing, p)
      "endif"       -> (ENDIF,      Nothing, p)
      "endproc"     -> (ENDPROC,    Nothing, p)
      "endprogram"  -> (ENDPROGRAM, Nothing, p)
      "endwhile"    -> (ENDWHILE,   Nothing, p)
      "false"       -> (BLITERAL,   Just (BLitAttrib False), p)
      "floor"       -> (RATIOOPR,   Just (ROprAttrib Floor), p)
      "fun"         -> (FUN,        Nothing, p)
      "global"      -> (GLOBAL,     Nothing, p)
      "if"          -> (IF,         Nothing, p)
      "init"        -> (INIT,       Nothing, p)
      "int"         -> (TYPE,       Just (TypeAttrib IntType), p)
      "local"       -> (LOCAL,      Nothing, p)
      "modE"        -> (MULTOPR,    Just (MultOprAttrib Mod), p)
      "not"         -> (NOT,        Nothing, p)
      "num"         -> (RATIOOPR,   Just (ROprAttrib Num), p)
      "proc"        -> (PROC,       Nothing, p)
      "program"     -> (PROGRAM,    Nothing, p)
      "ratio"       -> (TYPE,       Just (TypeAttrib RatioType), p)
      "ref"         -> (MECHMODE,   Just (MechModeAttrib REF), p)
      "returns"     -> (RETURNS,    Nothing, p)
      "round"       -> (RATIOOPR,   Just (ROprAttrib Round), p)
      "skip"        -> (SKIP,       Nothing, p)
      "then"        -> (THEN,       Nothing, p)
      "true"        -> (BLITERAL,   Just (BLitAttrib True), p)
      "var"         -> (CHANGEMODE, Just (ChangeModeAttrib VAR), p)
      "while"       -> (WHILE,      Nothing, p)
      _             -> (IDENT,      Just (IdentAttrib key), p)
