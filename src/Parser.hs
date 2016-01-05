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

  globalP :: ParserT Decl
  globalP = do tP GLOBAL; cpsDeclP;

  cpsDeclP :: ParserT Decl
  cpsDeclP = sepList0C declP (tP SEMICOLON) CpsDecl

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

  cpsStoDeclP :: ParserT Decl
  cpsStoDeclP = sepList1C stoDeclP (tP SEMICOLON) CpsDecl

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
      locals <- optC localP
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
      locals <- optC localP
      tP DO
      cmd <- cpsCmdP
      tP ENDPROC
      return (ProcDecl ident params locals cmd)

  exprP :: ParserT Expr
  exprP = dyadicExprP
      +++ factorExprP

  factorExprP :: ParserT Expr
  factorExprP = funCallExprP
            +++ literalExprP
            +++ storeExprP
            +++ monadicExprP
            +++ nestedExprP

  nestedExprP :: ParserT Expr
  nestedExprP =
    do
      tP LPAREN
      expr <- exprP
      tP RPAREN
      return expr

  literalExprP :: ParserT Expr
  literalExprP = boolLiteralP
             +++ intLiteralP
             +++ ratioLiteralP

  boolLiteralP :: ParserT Expr
  boolLiteralP =
    do
      (BLITERAL, Just (BLitAttrib val), _) <- tokenP BLITERAL
      return (LiteralExpr (BoolVal val))

  intLiteralP :: ParserT Expr
  intLiteralP =
    do
      (ALITERAL, Just (ALitAttrib val), _) <- tokenP ALITERAL
      return (LiteralExpr (IntVal val))

  ratioLiteralP :: ParserT Expr
  ratioLiteralP =
    do
      (RLITERAL, Just (RLitAttrib num denom), _) <- tokenP RLITERAL
      return (LiteralExpr (RatioVal (num, denom)))

  initP :: ParserT IsInitialization
  initP =
    do
      tP INIT
      return Initialization

  storeExprP :: ParserT Expr
  storeExprP =
    do
      ident <- identP
      i <- optC initP
      case i
        of
          Just _ -> return (StoreExpr ident Initialization)
          _ -> return (StoreExpr ident NoInitialization)

  funCallExprP :: ParserT Expr
  funCallExprP =
    do
      ident <- identP
      params <- exprListP
      return (FunCallExpr (ident, params))

  monadicExprP :: ParserT Expr
  monadicExprP = notExprP
             +++ plusExprP
             +++ ratioExprP

  notExprP :: ParserT Expr
  notExprP =
    do
      tP NOT
      expr <- exprP
      return (MonadicExpr Not expr)

  plusExprP :: ParserT Expr
  plusExprP =
    do
      (ARITHOPR, Just (AOprAttrib opr), _) <- tokenP ARITHOPR
      expr <- exprP
      case opr of
        IML.Plus -> return (MonadicExpr Plus expr)
        IML.Minus -> return (MonadicExpr Minus expr)
        _ -> error "Parse error, unknown monadic operator"

  ratioExprP :: ParserT Expr
  ratioExprP =
    do
      (RATIOOPR, Just (ROprAttrib opr), _) <- tokenP RATIOOPR
      expr <- exprP
      return (MonadicExpr opr expr)

  dyadicExprP :: ParserT Expr
  dyadicExprP = arithExprP
            +++ relExprP
            +++ boolExprP

  arithExprP :: ParserT Expr
  arithExprP =
    do
      lExpr <- factorExprP
      (ARITHOPR, Just (AOprAttrib opr), _) <- tokenP ARITHOPR
      rExpr <- exprP
      return (DyadicExpr opr lExpr rExpr)

  relExprP :: ParserT Expr
  relExprP =
    do
      lExpr <- factorExprP
      (RELOPR, Just (RelOprAttrib opr), _) <- tokenP RELOPR
      rExpr <- exprP
      return (DyadicExpr opr lExpr rExpr)

  boolExprP :: ParserT Expr
  boolExprP =
    do
      lExpr <- factorExprP
      (BOOLOPR, Just (BOprAttrib opr), _) <- tokenP BOOLOPR
      rExpr <- exprP
      return (DyadicExpr opr lExpr rExpr)

  exprListP :: ParserT [Expr]
  exprListP =
    do
      tP LPAREN
      expr <- sepList0C exprP (tP COMMA) id
      tP RPAREN
      return expr

  cpsCmdP :: ParserT Command
  cpsCmdP = sepList1C commandP (tP SEMICOLON) CpsCmd

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
      lExpr <- exprP
      tP BECOMES
      rExpr <- exprP
      return (AssiCmd lExpr rExpr)

  condCmdP :: ParserT Command
  condCmdP =
    do
      tP IF
      cond <- exprP
      tP THEN
      trueCmd <- cpsCmdP
      tP ELSE
      falseCmd <- cpsCmdP
      tP ENDIF
      return (CondCmd cond trueCmd falseCmd)

  whileCmdP :: ParserT Command
  whileCmdP =
    do
      tP WHILE
      cond <- exprP
      tP DO
      cmd <- cpsCmdP
      tP ENDWHILE
      return (WhileCmd cond cmd)

  callCmdP :: ParserT Command
  callCmdP =
    do
      tP CALL
      ident <- identP
      paramList <- exprListP
      return (ProcCallCmd (ident, paramList))

  debugInCmdP :: ParserT Command
  debugInCmdP =
    do
      tP DEBUGIN
      expr <- exprP
      return (DebugInCmd expr)

  debugOutCmdP :: ParserT Command
  debugOutCmdP =
    do
      tP DEBUGOUT
      expr <- exprP
      return (DebugOutCmd expr)

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
