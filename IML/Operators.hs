module IML.Operators where

  data AddOpr = PLUS | MINUS
    deriving (Show, Eq)

  data MultOpr = TIMES | DIV | MOD
    deriving (Show, Eq)

  data RelOpr = EQ | NE | LT | GT | LE | GE
    deriving (Show, Eq)

  data BoolOpr = AND | OR | CAND | COR
    deriving (Show, Eq)
