module IML.Tokens2 where

  import IML.Enumerations

  type Token = (Terminal, Maybe Attribute)

  data Terminal
    = LITERAL
 --  | LITERAL BoolVal true -- true
 --  | LITERAL BoolVal false -- false
     | IDENT
     | LPAREN -- (
     | RPAREN -- )
     | COMMA -- ,
     | SEMICOLON -- ;
     | COLON -- :
     | BECOMES -- :=
     | MULTOPR -- TIMES -- *
--    | MULTIOPR MOD_E -- modE
--  | MUTLTOPR DIV_E -- divE
     | ADDOPR -- PLUS -- +
--  | ADDOPR MINUS -- -
     | RELOPR -- EQ -- =
--  | RELOPR NE -- /=
--  | RELOPR LT -- <
--  | RELOPR GT -- >
--  | RELOPR LE -- <=
--  | RELOPR GE -- >=
     | BOOLOPR -- AND -- && (omit)
--  | BOOLOPR OR -- || (omit)
--  | BOOLOPR CAND -- &?
--  | BOOLOPR COR -- |?
     | TYPE -- BOOL -- bool
--    | TYPE INT64 -- INT64
     | CALL -- call
     | CHANGEMODE -- CONST -- const
  --  | CHANGEMODE VAR -- var
     | MECHMODE --COPY -- const
  --  | MECHMODE REF -- ref
     | DEBUGIN -- debugin
     | DEBUGOUT -- debugout
     | DO -- do
     | ELSE -- else
     | ENFUN -- endfun
     | ENDIF -- endif
     | ENDPROC -- endproc
     | ENDPROGRAM -- endprogram
     | ENDWHILE -- endwhile
     | FUN -- fun
     | GLOBAL -- global
     | IF -- if
     | FLOWMODE -- IN -- [in]
  --  | FLOWMODE -- INOUT -- [inout]
  --  | FLOWMODE OUT -- [out]
     | INIT -- init
     | LOCAL -- local
     | NOTOPR -- not
     | PROC -- proc
     | PROGRAM -- program
     | RETURNS -- returns
     | SKIP -- skip
     | THEN -- then
     | WHILE -- while
    deriving (Show, Eq)

  data Attribute
    = ALitAttrib Int
    | BLitAttrib Bool
    | IdentAttrib Ident
