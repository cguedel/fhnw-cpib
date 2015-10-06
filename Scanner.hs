module Scanner (tokenize) where

  import IML.Tokens
  import IML.Value
  import IML.Keywords
  import IML.Symbols

  import Data.Char hiding (isSymbol)

  tokenize :: String -> [Token]
  tokenize [] = []
  tokenize (c : cs)
    | isLiteralStart c = makeLiteral c cs
    | isSymbol c = makeSymbol (c:cs)
    | isAlpha c = makeIdent (c:cs)
    | isSpace c = tokenize cs
    | otherwise = error $ "Cannot tokenize " ++ [c]

  isLiteralStart :: Char -> Bool
  isLiteralStart c = c `elem` "+-(" || isDigit c

  isLiteralPart :: Char -> Bool
  isLiteralPart c = c `elem` "/)" || isDigit c

  makeSymbol :: String -> [Token]
  makeSymbol cs =
    let (str, cs') = span isSymbol cs
    in makeSymbolToken str : tokenize cs'

  convertStringToInt :: String -> Int
  convertStringToInt str = fromInteger $ read str

  makeLiteral :: Char -> String -> [Token]
  makeLiteral c cs =
      let (digs, cs') = span isLiteralPart cs
      in
        case digs of
          [] ->
            (case () of
              _ | isDigit c -> LITERAL (Int32Val $ convertStringToInt [c])
                | otherwise -> makeSymbolToken [c]) : tokenize cs'
          _ -> makeLiteralWithSign c digs : tokenize cs'

  makeLiteralWithSign :: Char -> String -> Token
  makeLiteralWithSign c cs =
      case c of
        '+' -> LITERAL (Int32Val $ convertStringToInt cs)
        '(' -> makeRationalLiteral cs
        _ -> LITERAL (Int32Val $ convertStringToInt (c:cs))

  makeRationalLiteral :: String -> Token
  makeRationalLiteral str =
    let (numerator, denominator) = span isDigit str
    in LITERAL (RationalVal (convertStringToInt numerator)
                            (convertStringToInt $ init (tail denominator)))

  makeIdent :: String -> [Token]
  makeIdent cs =
      let (w, cs') = span isAlphaNum cs
      in
        (case() of
          _ | isKeyword w -> makeKeywordToken w
            | otherwise -> IDENT w) : tokenize cs'
