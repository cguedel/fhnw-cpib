module IML.Tokens where

type Token = (Terminal, Maybe Attribute)

data Terminal
 = LITERAL
  | IDENT
  | LPAREN -- (
  | RPAREN -- )
  | COMMA -- ,
  | SEMICOLON -- ;
  | COLON -- :
  | BECOMES -- :=
  | MULTOPR -- TIMES -- *
  | ADDOPR PLUS -- +
  | ADDOPR MINUS -- -
  | RELOPR EQ -- =
  | RELOPR NE -- /=
  | RELOPR LT -- <
  | RELOPR GT -- >
  | RELOPR LE -- <=
  | RELOPR GE -- >=
  | BOOLOPR AND -- && (omit)
  | BOOLOPR OR -- || (omit)
  | BOOLOPR CAND -- &?
  | BOOLOPR COR -- |?
  | TYPE BOOL -- bool
  | CALL -- call
  | CHANGEMODE CONST -- const
  | MECHMODE COPY -- const
  | DEBUGIN -- debugin
  | DEBUGOUT -- debugout
  | MUTLTOPR DIV_E -- divE
  | DO -- do
  | ELSE -- else
  | ENFUN -- endfun
  | ENDIF -- endif
  | ENDPROC -- endproc
  | ENDPROGRAM -- endprogram
  | ENDWHILE -- endwhile
  | LITERAL BoolVal false -- false
  | FUN -- fun
  | GLOBAL -- global
  | IF -- if
  | FLOWMODE IN -- [in]
  | INIT -- init
  | FLOWMODE INOUT -- [inout]
  | TYPE INT64 -- INT64
  | LOCAL -- local
  | MULTIOPR MOD_E -- modE
  | NOTOPR -- not
  | FLOWMODE OUT -- [out]
  | PROC -- proc
  | PROGRAM -- program
  | MECHMODE REF -- ref
  | RETURNS -- returns
  | SKIP -- skip
  | THEN -- then
  | LITERAL BoolVal true -- true
  | CHANGEMODE VAR -- var
  | WHILE -- while
    deriving (Show, Eq)

data Attribute
  = ALitAttrib Int
  | BLitAttrib Bool
  | IdentAttrib Ident
  | ROprAttrib Opr
