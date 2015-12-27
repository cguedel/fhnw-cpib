module Parser where

  import IML
  import AbstractSyntax
  import ParserCombinators

  type ParserT = Parser Token

  tokenP :: Terminal -> ParserT Token
  tokenP term = P (\inp ->
    case parse itemP inp of
      [] -> []
      [((term', attrib, pos), out)] ->
        [((term', attrib, pos), out) | term' == term])

  tP :: Terminal -> ParserT ()
  tP term =
    do
      _ <- tokenP term
      return ()

  identP :: ParserT Ident
  identP = do (IDENT, Just (IdentAttrib ident), _) <- tokenP IDENT; return ident

  typedIdentP :: ParserT TypedIdent
  typedIdentP = do
    (IDENT, Just (IdentAttrib ident), _) <- tokenP IDENT
    tP COLON
    (TYPE, Just (TypeAttrib t), _) <- tokenP TYPE;
    return (ident, t)

  aLitP :: ParserT Int
  aLitP = do (ALITERAL, Just (ALitAttrib val), _) <- tokenP ALITERAL; return val

  bLitP :: ParserT Bool
  bLitP = do (BLITERAL, Just (BLitAttrib val), _) <- tokenP BLITERAL; return val

  rLitP :: ParserT Ratio
  rLitP = do (RLITERAL, Just (RLitAttrib num denom), _) <- tokenP RLITERAL; return (num, denom)

  arithOprP :: ParserT ArithOperator
  arithOprP = do (ARITHOPR, Just (AOprAttrib opr), _) <- tokenP ARITHOPR; return opr

  relOprP :: ParserT RelOperator
  relOprP = do (RELOPR, Just (RelOprAttrib opr), _) <- tokenP RELOPR; return opr

  boolOprP :: ParserT BoolOperator
  boolOprP = do (BOOLOPR, Just (BOprAttrib opr), _) <- tokenP BOOLOPR; return opr

  ratioOprP :: ParserT RatioOperator
  ratioOprP = do (RATIOOPR, Just (ROprAttrib opr), _) <- tokenP RATIOOPR; return opr

  litAExprP :: Parser Token ArithExpr
  litAExprP = do val <- aLitP; return (LitAExpr val)

  idAExprP :: Parser Token ArithExpr
  idAExprP = do ident <- identP; return (IdAExpr ident)

  idBExprP :: Parser Token BoolExpr
  idBExprP = do ident <- identP; return (IdBExpr ident)

  arithExprP :: ParserT ArithExpr
  arithExprP =
        litAExprP
    +++ idAExprP
    +++ dyaAExprP

  dyaAExprP1 :: ParserT (ArithOperator, ArithExpr)
  dyaAExprP1 =
    do
      tP LPAREN
      aExpr1 <- arithExprP
      aOpr   <- arithOprP
      return (aOpr, aExpr1)

  dyaAExprP2 :: (ArithOperator, ArithExpr) -> ParserT ArithExpr
  dyaAExprP2 (aOpr, aExpr1) =
    do
      aExpr2 <- arithExprP
      tP RPAREN
      return (DyaAExpr aOpr aExpr1 aExpr2)

  dyaAExprP :: ParserT ArithExpr
  dyaAExprP =
    dyaAExprP1 >>= dyaAExprP2

  boolExprP :: ParserT BoolExpr
  boolExprP =
        litBExprP
    +++ idBExprP
    +++ relBExprP
    +++ negBExprP
    +++ dyaBExprP

  litBExprP :: Parser Token BoolExpr
  litBExprP = do val <- bLitP; return (LitBExpr val)

  relBExprP :: Parser Token BoolExpr
  relBExprP =
    do
      tP LPAREN
      aExpr1 <- arithExprP
      rOpr   <- relOprP
      aExpr2 <- arithExprP
      tP RPAREN
      return (RelBExpr rOpr aExpr1 aExpr2)

  negBExprP :: Parser Token BoolExpr
  negBExprP =
    do
      tP NOT
      bExpr <- boolExprP
      return (NegBExpr bExpr)

  dyaBExprP :: Parser Token BoolExpr
  dyaBExprP =
    do
      tP LPAREN
      bExpr1 <- boolExprP
      bOpr   <- boolOprP
      bExpr2 <- boolExprP
      tP RPAREN
      return (DyaBExpr bOpr bExpr1 bExpr2)

  changeModeP :: Parser Token ChangeMode
  changeModeP = do (CHANGEMODE, Just (ChangeModeAttrib mode), _) <- tokenP CHANGEMODE; return mode

  mechModeP :: Parser Token MechMode
  mechModeP = do (MECHMODE, Just (MechModeAttrib mode), _) <- tokenP MECHMODE; return mode

  paramP :: ParserT Parameter
  paramP =
    do
      mech   <- optC mechModeP
      change <- optC changeModeP
      ident  <- typedIdentP
      return (ident, mech, change)

  paramListP :: ParserT [Parameter]
  paramListP =
    do
      tP LPAREN
      params <- sepList0C paramP (tP COMMA) id
      tP RPAREN
      return params

  cpsDeclP :: ParserT Decl
  cpsDeclP = sepList0C declP (tP SEMICOLON) CpsDecl

  cpsStoDeclP :: ParserT Decl
  cpsStoDeclP = sepList1C stoDeclP (tP SEMICOLON) CpsDecl

  declP :: ParserT Decl
  declP = stoDeclP
      +++ funDeclP
      +++ procDeclP

  stoDeclP :: ParserT Decl
  stoDeclP =
    do
      mode  <- optC changeModeP
      ident <- typedIdentP
      return (StoDecl ident mode)

  localP :: ParserT Decl
  localP =
    do
      tP LOCAL
      cpsStoDeclP

  funDeclP :: ParserT Decl
  funDeclP =
    do
      tP FUN
      ident <- identP
      params <- paramListP
      tP RETURNS
      ret <- stoDeclP
      locals <- optC cpsStoDeclP
      tP DO
      cmd <- cpsCmdP
      tP ENDFUN
      return (FunDecl ident params ret locals cmd)

  procDeclP :: ParserT Decl
  procDeclP =
    do
      tP PROC
      ident <- identP
      params <- paramListP
      locals <- optC cpsStoDeclP
      tP DO
      cmd <- cpsCmdP
      tP ENDPROC
      return (ProcDecl ident params locals cmd)

  commandP :: ParserT Command
  commandP = skipCmdP
         +++ assiCmdP
         +++ condCmdP
         +++ whileCmdP
         +++ callCmdP
         +++ debugInCmdP
         +++ debugOutCmdP

  skipCmdP :: ParserT Command
  skipCmdP =
    do
       tP SKIP
       return SkipCmd

  assiCmdP :: ParserT Command
  assiCmdP =
    do
       ident <- identP
       tP BECOMES
       aExpr <- arithExprP
       return (AssiCmd ident aExpr)

  condCmdP :: ParserT Command
  condCmdP =
    do
       tP IF
       bExpr <- boolExprP
       tP THEN
       cmd1  <- cpsCmdP
       tP ELSE
       cmd2  <- cpsCmdP
       tP ENDIF
       return (CondCmd bExpr cmd1 cmd2)

  whileCmdP :: ParserT Command
  whileCmdP =
    do
       tP WHILE
       bExpr <- boolExprP
       tP DO
       cmd   <- cpsCmdP
       tP ENDWHILE
       return (WhileCmd bExpr cmd)

  callCmdP :: ParserT Command
  callCmdP = do tP CALL; ident <- identP; return (CallCmd ident);

  debugInCmdP :: ParserT Command
  debugInCmdP = do tP DEBUGIN; expr <- arithExprP; return (DebugInCmd expr);

  debugOutCmdP :: ParserT Command
  debugOutCmdP = do tP DEBUGOUT; expr <- arithExprP; return (DebugOutCmd expr);

  cpsCmdP :: ParserT Command
  cpsCmdP = sepList1C commandP (tP SEMICOLON) CpsCmd

  globalP :: ParserT Decl
  globalP = do tP GLOBAL; cpsDeclP;

  programP :: ParserT Program
  programP =
    do
       tP PROGRAM
       _ <- identP
       globals <- optC globalP
       tP DO
       cmd <- cpsCmdP
       tP ENDPROGRAM
       tP SENTINEL
       return (cmd, globals)

  parser :: [Token] -> Program
  parser ts =
    case parse programP ts of
      -- Pattern matches a command and an EMPTY list -> if the list is not empty, an error in the compiler was found
      -- If no command is returned, no valid syntax tree could be constructed -> syntax error
      [(prog, [])] -> prog
      [(x1, x2)] -> error $ "internal error" ++ show x1 ++ show x2
      _ -> error "syntax error"
