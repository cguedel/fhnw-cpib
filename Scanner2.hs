import IML.Tokens2

scanner :: String -> [Token]
scanner cs = s0 (cs, [])

s0 :: (String, [Token]) -> [Token]
-- Relative Operators
s0 ('<':'=' : cs, accu) = s0 (cs, (RELOPR, Just (ROprAttrib LessEq)): accu)
s0 ('<'     : cs, accu) = s0 (cs, (RELOPR, Just (ROprAttrib Less)) : accu)
s0 ('>':'=' : cs, accu) = s0 (cs, (RELOPR, Just (ROprAttrib GreaterEq)): accu)
s0 ('>'     : cs, accu) = s0 (cs, (RELOPR, Just (ROprAttrib Greater)): accu)
s0 ('='     : cs, accu) = s0 (cs, (RELOPR, Just (ROprAttrib Equal)): accu)
s0 ('/':'=' : cs, accu) = s0 (cs, (RELOPR, Just (ROprAttrib NotEqual)): accu)
-- Bool Operators
s0 ('&':'&' : cs, accu) = s0 (cs, (BOOLOPR, Just (BOprAttrib And)): accu)
s0 ('&':'?' : cs, accu) = s0 (cs, (BOOLOPR, Just (BOprAttrib Cand)): accu)
s0 ('|':'|' : cs, accu) = s0 (cs, (BOOLOPR, Just (BOprAttrib Or)): accu)
s0 ('|':'?' : cs, accu) = s0 (cs, (BOOLOPR, Just (BOprAttrib Cor)): accu)
-- Symbols
s0 ('('     : cs, accu) = s0 (cs, (LPAREN, Nothing): accu)
s0 (')'     : cs, accu) = s0 (cs, (RPAREN, Nothing): accu)
s0 (','     : cs, accu) = s0 (cs, (COMMA, Nothing): accu)
s0 (';'     : cs, accu) = s0 (cs, (SEMICOLON, Nothing): accu)
s0 (':'     : cs, accu) = s0 (cs, (COLON, Nothing): accu)
-- Multiopr
s0 ('*'     : cs, accu) = s0 (cs, (MULTOPR, Just (MOprAttrib Times)): accu)
-- Addopr
s0 ('+'     : cs, accu) = s0 (cs, (ADDOPR, Just (AOprAttrib Plus)): accu)
s0 ('-'     : cs, accu) = s0 (cs, (ADDOPR, Just (MOprAttrib Minus)): accu)
s0 (c:cs, accu)
  | isAlpha c = s0 (s1 (cs, [c], accu))
  | isDigit c = s0 (s2 (cs, digitToInt c, accu))
  | isSpace c = s0 (cs, accu)
  | otherwise = error ("!!SHOT ME OR HELP ME!! Lexical error: " ++ [c])
s0 ([], accu) = reverse ((SENTINEL, Nothing):accu)

s1 :: (String, String, [Token]) -> (String, [Token])
s1 (c:cs, accu', accu)
  | isAlpha c = s1 (cs, c:accu', accu)
  | otherwise = (c:cs, (IDENT, IdentAttrib accu'):accu)

s2 :: (String, Int, [Token]) -> (String, [Token])
s2 (c:cs, accu', accu)
  | isDigit c = s2 (cs, 10 * accu' + digitToInt c, accu)
  | otherwise = (c:cs, (ALITERAL, Just (ALitAttrib accu')):accu)
s2 ([], accu', accu) = ([], (ALITERAL, Just (ALitAttrib accu')):accu)
