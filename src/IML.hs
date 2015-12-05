module IML where

  type Position = (Int, Int) -- Line number, column number
  type Token = (Terminal, Maybe Attribute, Position)

  data Attribute
    = ALitAttrib Int
    | BLitAttrib Bool
    | RLitAttrib Int Int
    | IdentAttrib String
    | AOprAttrib ArithOperator
    | BOprAttrib BoolOperator
    | ROprAttrib RatioOperator
    | RelOprAttrib RelOperator
    | TypeAttrib Type
    | ChangeModeAttrib ChangeMode
    | MechModeAttrib MechMode
    deriving (Show, Eq)

  data ArithOperator
    = Times
    | Div
    | Mod
    | Plus
    | Minus
    deriving (Show, Eq)

  data RelOperator
    = LessEq
    | Less
    | NotEq
    | Equal
    | Greater
    | GreaterEq
    deriving (Eq, Show)

  data RatioOperator
    = Num
    | Denom
    | Floor
    | Ceil
    | Round
    deriving (Eq, Show)

  data BoolOperator
    = Cand
    | Cor
    deriving (Eq, Show)

  data Type
    = BOOL | INT | RATIO
    deriving (Show, Eq)

  data MechMode
    = COPY | REF
    deriving (Show, Eq)

  data ChangeMode
    = CONST | VAR
    deriving (Show, Eq)

  data Terminal
    = LPAREN
    | RPAREN
    | COMMA
    | SEMICOLON
    | COLON
    | BECOMES
    | ARITHOPR
    | BOOLOPR
    | RATIOOPR
    | RELOPR
    | TYPE
    | CALL
    | CHANGEMODE
    | MECHMODE
    | DEBUGIN
    | DEBUGOUT
    | DO
    | ELSE
    | ENDFUN
    | ENDIF
    | ENDPROC
    | ENDPROGRAM
    | ENDWHILE
    | ALITERAL
    | BLITERAL
    | RLITERAL
    | FUN
    | GLOBAL
    | IF
    | INIT
    | LOCAL
    | NOT
    | PROC
    | PROGRAM
    | RETURNS
    | SKIP
    | THEN
    | WHILE
    | SENTINEL
    | IDENT
    deriving (Show, Eq)
