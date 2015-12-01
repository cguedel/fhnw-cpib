module IML where

  type Token = (Terminal, Maybe Attribute)

  data Attribute
    = ALitAttrib Int
    | BLitAttrib Bool
    | RLitAttrib Int Int
    | IdentAttrib String
    | OprAttrib Operator
    | TypeAttrib Type
    | ChangeModeAttrib ChangeMode
    | MechModeAttrib MechMode
    deriving (Show, Eq)

  data Operator
    = TIMES | DIV_E | MOD_E
    | PLUS | MINUS
    | LT | GE | EQ | NE | GT | LE
    | NOT | CAND | COR
    | NUM | DENUM | FLOOR | CEIL | ROUND
    deriving (Show, Eq)

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
    | MULTOPR
    | ADDOPR
    | RELOPR
    | BOOLOPR
    | RATIOOPR
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
    | LITERAL
    | FUN
    | GLOBAL
    | IF
    | INIT
    | LOCAL
    | NOTOPR
    | PROC
    | PROGRAM
    | RETURNS
    | SKIP
    | THEN
    | WHILE
    | SENTINEL
    | IDENT
    deriving (Show, Eq)
