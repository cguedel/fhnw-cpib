module StaticAnalysis (analyze) where

  import IML
  import AbstractSyntax

  analyze :: Program -> Program
  analyze = scopeChecker . typeChecker

  scopeChecker :: Program -> Program
  scopeChecker (cmd, decl) = (cmd, decl)

  typeChecker :: Program -> Program
  typeChecker (cmd, decl) = (cmdChecker cmd, decl)

  cmdChecker :: Command -> Command
  cmdChecker (AssiCmd lExpr rExpr) =
    do
      let (lExpr',rExpr') = checkExprTypeCompatible (lExpr, rExpr)
      AssiCmd lExpr' rExpr'

  cmdChecker (CondCmd cond cmd1 cmd2) = CondCmd (checkBoolExpr cond) (cmdChecker cmd1) (cmdChecker cmd2)
  cmdChecker (WhileCmd cond cmd) = WhileCmd (checkBoolExpr cond) (cmdChecker cmd)

  cmdChecker (ProcCallCmd (ident, params)) = ProcCallCmd (ident, params)
  cmdChecker (DebugInCmd expr) = DebugInCmd (exprChecker expr)
  cmdChecker (DebugOutCmd expr) = DebugOutCmd (exprChecker expr)
  cmdChecker (CpsCmd cmds) = CpsCmd (map cmdChecker cmds)
  cmdChecker (SkipCmd) = SkipCmd

  exprChecker :: Expr -> Expr
  exprChecker x = x

  checkBoolExpr :: Expr -> Expr
  checkBoolExpr expr =
    if isBoolExpr expr then
      expr
    else
      error "Expected boolean expression"

  isBoolExpr :: Expr -> Bool
  isBoolExpr (LiteralExpr (BoolVal _)) = True
  isBoolExpr (LiteralExpr _) = False
  isBoolExpr (DyadicExpr opr expr1 expr2) = opr `elem` [CAnd, Cor, Less, LessEq, Equal, NotEq, Greater, GreaterEq] && isBoolExpr expr1 && isBoolExpr expr2
  isBoolExpr (MonadicExpr opr expr) = opr == Not && isBoolExpr expr
  isBoolExpr (StoreExpr ident _) = isBoolIdent ident
  isBoolExpr (FunCallExpr (ident, _)) = isBoolFunction ident
  isBoolExpr _ = False

  isBoolIdent :: Ident -> Bool
  isBoolIdent _ = True

  isBoolFunction :: Ident -> Bool
  isBoolFunction _ = True

  checkExprTypeCompatible :: (Expr, Expr) -> (Expr, Expr)
  checkExprTypeCompatible (e1, e2) = (e1, e2)
