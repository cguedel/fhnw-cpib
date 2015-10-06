module IML.Tokens where

  import IML.Operators
  import IML.Value
  import IML.Types
  import IML.ChangeMode
  import IML.MechMode
  import IML.FlowMode

  data Token = LPAREN | RPAREN | COMMA | SEMICOLON | COLON | BECOMES
    | MULTOPR MultOpr | ADDOPR AddOpr | RELOPR RelOpr | BOOLOPR BoolOpr
    | LITERAL Value | IDENT String | CALL | TYPE Type | CHANGEMODE ChangeMode
    | MECHMODE MechMode | DEBUGIN | DEBUGOUT | DO | ELSE | ENDFUN | ENDIF
    | ENDPROC | ENDPROGRAM | ENDWHILE | FUN | GLOBAL | IF | FLOWMODE FlowMode
    | INIT | LOCAL | NOT | PROC | PROGRAM | RETURNS | SKIP | THEN | WHILE
    deriving (Show, Eq)
