module IML.Enumerations where

  data FlowMode = IN | INOUT | OUT
    deriving (Show, Eq)

  data MechMode = COPY | REF
    deriving (Show, Eq)

  data ChangeMode = CONST | VAR
    deriving (Show, Eq)

  data Value = BoolVal Bool | Int64Val Int | RationalVal Int Int
    deriving (Show, Eq)

  data Type = BOOL | INT64
    deriving (Show, Eq)

  data Operator = TIMES
      | DIV_E | MOD_E | DIV_F | MOD_F | DIV_T | MOD_T
      | PLUS | MINUS
      | LT | GE | EQ | NE | GT | LE
      | NOTOPR | AND | OR | CAND | COR
