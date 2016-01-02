module IML where

  type Position = (Int, Int) -- Line number, column number
  type Token = (Terminal, Maybe Attribute, Position)

  data Operator
    = Not
    | Times
    | Div
    | Mod
    | Plus
    | Minus
    | Less
    | GreaterEq
    | Equal
    | NotEq
    | Greater
    | LessEq
    | CAnd
    | Cor
    | Denom
    | Num
    | Floor
    | Ceil
    | Round
    deriving (Show, Eq)

  data Attribute
    = ALitAttrib Int
    | BLitAttrib Bool
    | RLitAttrib Int Int
    | IdentAttrib String
    | AOprAttrib Operator
    | BOprAttrib Operator
    | ROprAttrib Operator
    | RelOprAttrib Operator
    | TypeAttrib Type
    | ChangeModeAttrib ChangeMode
    | MechModeAttrib MechMode
    deriving (Show, Eq)

  data Type
    = BoolType | IntType | RatioType
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
