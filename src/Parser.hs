module Parser where

  import IML
  import AbstractSyntax
  import ParserCombinators

  type ParserT = Parser Token

  tokenP :: Terminal -> ParserT Token
  tokenP term = P (\inp ->
    case parse itemP inp of
      [] -> []
      [((term', attrib, pos), out)] -> [((term', attrib, pos), out) | term' == term]
      ((_, _, _), _) : (_ : _) -> error "Internal error")

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

  --exprP :: ParserT Expr
  --exprP = term1P
      -- +++ factorExprP

  exprP :: ParserT Expr
  exprP =
    do
      term0 <- term0P
      return (termToExpr term0)

  termToExpr :: Term -> Expr
  termToExpr (Term0Term1RepBoolOpr term1 right) = repBoolOprFactorToExpr term1 right
  termToExpr (Term1Term2OptRelOpr term2 optRelOpr) = optRelOprFactorToExpr term2 optRelOpr
  termToExpr (Term2Term3RepAddOpr term3 right) = repAddOprFactorToExpr term3 right
  termToExpr (Term3FactorRepMultOpr factor right) = repMultOprFactorToExpr factor right

  factorToExpr :: Factor -> Expr
  factorToExpr (LiteralFactor value) = LiteralExpr value

  optRelOprFactorToExpr :: Term -> OptRelOprFactor -> Expr
  optRelOprFactorToExpr f (RelOprEps) = termToExpr f
  optRelOprFactorToExpr left (RelOprFactor factor operator) =
    let
      left' = termToExpr left
      right' = termToExpr factor
      in
        DyadicExpr operator left' right'

  repBoolOprFactorToExpr :: Term -> RepBoolOprFactor -> Expr
  repBoolOprFactorToExpr f (RepBoolOprFactorEps) = termToExpr f
  repBoolOprFactorToExpr left (RepBoolOprFactorOpr factor operator right) =
    let
      left' = termToExpr left
      right' = repBoolOprFactorToExpr factor right
      in
        DyadicExpr operator left' right'

  repAddOprFactorToExpr :: Term -> RepAddOprFactor -> Expr
  repAddOprFactorToExpr f (RepAddOprFactorEps) = termToExpr f
  repAddOprFactorToExpr left (RepAddOprFactorOpr factor operator right) =
    let
      left' = termToExpr left
      right' = repAddOprFactorToExpr factor right
      in
        DyadicExpr operator left' right'

  repMultOprFactorToExpr :: Factor -> RepMultOprFactor -> Expr
  repMultOprFactorToExpr f (RepMultOprFactorEps) = factorToExpr f
  repMultOprFactorToExpr left (RepMultOprFactorOpr factor operator right) =
      let
        left' = factorToExpr left
        right' = repMultOprFactorToExpr factor right
        in
          DyadicExpr operator left' right'

  data Factor
    = LiteralFactor Value

  data RepMultOprFactor
    = RepMultOprFactorOpr Factor Operator RepMultOprFactor
    | RepMultOprFactorEps

  data RepAddOprFactor
    = RepAddOprFactorOpr Term Operator RepAddOprFactor
    | RepAddOprFactorEps

  data RepBoolOprFactor
    = RepBoolOprFactorOpr Term Operator RepBoolOprFactor
    | RepBoolOprFactorEps

  data OptRelOprFactor
    = RelOprFactor Term Operator
    | RelOprEps

  data Term
    = Term0Term1RepBoolOpr Term RepBoolOprFactor
    | Term1Term2OptRelOpr Term OptRelOprFactor
    | Term2Term3RepAddOpr Term RepAddOprFactor
    | Term3FactorRepMultOpr Factor RepMultOprFactor

  term0P :: ParserT Term
  term0P =
    do
      term1 <- term1P
      repBoolOpr <- repBoolOprFactorP
      return (Term0Term1RepBoolOpr term1 repBoolOpr)

  term1P :: ParserT Term
  term1P =
    do
      term2 <- term2P
      optRelOpr <- optRelOprFactorP
      return (Term1Term2OptRelOpr term2 optRelOpr)

  term2P :: ParserT Term
  term2P =
    do
      term3 <- term3P
      repAddOpr <- repAddOprFactorP
      return (Term2Term3RepAddOpr term3 repAddOpr)

  term3P :: ParserT Term
  term3P =
    do
      factor <- factorP
      repMultOpr <- repMultOprFactorP
      return (Term3FactorRepMultOpr factor repMultOpr)

  optRelOprFactorP :: ParserT OptRelOprFactor
  optRelOprFactorP = relOprFactorP +++ relOprFactorEpsP

  relOprFactorP :: ParserT OptRelOprFactor
  relOprFactorP =
    do
      (RELOPR, Just (RelOprAttrib opr), _) <- tokenP RELOPR
      term2 <- term2P
      return (RelOprFactor term2 opr)

  relOprFactorEpsP :: ParserT OptRelOprFactor
  relOprFactorEpsP = return RelOprEps

  repBoolOprFactorP :: ParserT RepBoolOprFactor
  repBoolOprFactorP =
    repBoolOprFactorOprP +++
    repBoolOprFactorEpsP

  repBoolOprFactorOprP :: ParserT RepBoolOprFactor
  repBoolOprFactorOprP =
    do
      (BOOLOPR, Just (BOprAttrib opr), _) <- tokenP BOOLOPR
      term1 <- term1P
      repBoolOpr <- repBoolOprFactorP
      return (RepBoolOprFactorOpr term1 opr repBoolOpr)

  repBoolOprFactorEpsP :: ParserT RepBoolOprFactor
  repBoolOprFactorEpsP = return RepBoolOprFactorEps

  repAddOprFactorP :: ParserT RepAddOprFactor
  repAddOprFactorP =
    repAddOprFactorOprP +++
    repAddOprFactorEpsP

  repAddOprFactorOprP :: ParserT RepAddOprFactor
  repAddOprFactorOprP =
    do
      (ADDOPR, Just (AddOprAttrib opr), _) <- tokenP ADDOPR
      term3 <- term3P
      repAddOpr <- repAddOprFactorP
      return (RepAddOprFactorOpr term3 opr repAddOpr)

  repAddOprFactorEpsP :: ParserT RepAddOprFactor
  repAddOprFactorEpsP = return RepAddOprFactorEps

  repMultOprFactorP :: ParserT RepMultOprFactor
  repMultOprFactorP =
    repMultOprFactorOprP +++
    repMultOprFactorEpsP

  repMultOprFactorOprP :: ParserT RepMultOprFactor
  repMultOprFactorOprP =
    do
      (MULTOPR, Just (MultOprAttrib opr), _) <- tokenP MULTOPR
      factor <- factorP
      repMultOpr <- repMultOprFactorP
      return (RepMultOprFactorOpr factor opr repMultOpr)

  repMultOprFactorEpsP :: ParserT RepMultOprFactor
  repMultOprFactorEpsP = return RepMultOprFactorEps

  factorP :: ParserT Factor
  factorP =
    do
      (ALITERAL, Just (ALitAttrib val), _) <- tokenP ALITERAL
      return (LiteralFactor (IntVal val))

  factorExprP :: ParserT Expr
  factorExprP = funCallExprP
            +++ literalExprP
            +++ storeExprP
            +++ monadicExprP
            +++ nestedExprP
            +++ typeConvExprP

  typeConvExprP :: ParserT Expr
  typeConvExprP =
    do
      (TYPECOPR, Just (TypeAttrib RatioType), _) <- tokenP TYPECOPR
      expr <- exprP
      return (TypeConvExpr RatioType expr)

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
      (ADDOPR, Just (AddOprAttrib opr), _) <- tokenP ADDOPR
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
  dyadicExprP = addOprExprP
            +++ multOprExprP
            +++ relExprP
            +++ boolExprP

  multOprExprP :: ParserT Expr
  multOprExprP =
    do
      lExpr <- factorExprP
      (MULTOPR, Just (MultOprAttrib opr), _) <- tokenP MULTOPR
      rExpr <- exprP
      return (DyadicExpr opr lExpr rExpr)

  addOprExprP :: ParserT Expr
  addOprExprP =
    do
      lExpr <- factorExprP
      (ADDOPR, Just (AddOprAttrib opr), _) <- tokenP ADDOPR
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
