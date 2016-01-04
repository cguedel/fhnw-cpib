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
  cmdChecker (ProcCallCmd (ident, params)) = ProcCallCmd (ident, params)
  cmdChecker (DebugInCmd expr) = DebugInCmd (exprChecker expr)
  cmdChecker (DebugOutCmd expr) = DebugOutCmd (exprChecker expr)
  cmdChecker (CpsCmd cmds) = CpsCmd (map cmdChecker cmds)
  cmdChecker (SkipCmd) = SkipCmd
  cmdChecker (CondCmd expr cmd1 cmd2) = CondCmd (exprChecker expr) (cmdChecker cmd1) (cmdChecker cmd2)
  cmdChecker (AssiCmd expr1 expr2) = AssiCmd (exprChecker expr1) (exprChecker expr2)

  exprChecker :: Expr -> Expr
  exprChecker (expr, _) = (exprTypeChecker expr, Just $ getExprType expr)

  exprTypeChecker :: ExprType -> ExprType
  exprTypeChecker (MonadicExpr opr expr) = MonadicExpr opr (exprChecker expr)
  exprTypeChecker (DyadicExpr opr expr1 expr2) = DyadicExpr opr (exprChecker expr1) (exprChecker expr2)
  exprTypeChecker (LiteralExpr val) = LiteralExpr val
  exprTypeChecker (FunCallExpr call) = FunCallExpr call
  exprTypeChecker (StoreExpr ident initialize) = StoreExpr ident initialize

  getExprType :: ExprType -> Type
  getExprType (LiteralExpr val) = getLitExprType val
  getExprType (MonadicExpr opr (expr, _))
    | isOperatorCompatible opr exprType = getOperatorResultType opr exprType
    | otherwise = error "Type error"
    where
      exprType = getExprType expr

  getExprType (DyadicExpr opr (expr1, _) (expr2, _))
    | isExprTypeCompatible expr1Type expr2Type && isOperatorCompatible opr compatibleType = getOperatorResultType opr compatibleType
    | otherwise = error $ "Type error in dyadic expression, " ++ show expr1Type ++ " not assignment compatible with " ++ show expr2Type
    where
      expr1Type = getExprType expr1
      expr2Type = getExprType expr2
      compatibleType = getCompatibleType expr1Type expr2Type
      _ = exprChecker (expr1, Nothing)
      _ = exprChecker (expr2, Nothing)

  getExprType (FunCallExpr _) = IntType
  getExprType (StoreExpr _ _) = RatioType

  getLitExprType :: Value -> Type
  getLitExprType (BoolVal _) = BoolType
  getLitExprType (RatioVal _) = RatioType
  getLitExprType (IntVal _) = IntType

  isRatioOperator :: Operator -> Bool
  isRatioOperator opr = opr `elem` [Num, Denom, Floor, Ceil, Round]

  isExprTypeCompatible :: Type -> Type -> Bool
  isExprTypeCompatible IntType RatioType = True
  isExprTypeCompatible RatioType IntType = True
  isExprTypeCompatible t1 t2 = t1 == t2

  isOperatorCompatible :: Operator -> Type -> Bool
  isOperatorCompatible opr t
    | opr `elem` [Plus, Minus, Times, Div, Mod, Less, LessEq, GreaterEq, Greater] = t == IntType || t == RatioType
    | opr `elem` [CAnd, Cor] = t == BoolType
    | otherwise = True

  getOperatorResultType :: Operator -> Type -> Type
  getOperatorResultType opr t
    | opr `elem` [Plus, Minus, Times, Mod] = t
    | opr == Div = RatioType -- Handle divisions as rational numbers -> needed for type inference
    | isRatioOperator opr = IntType
    | otherwise = BoolType

  getCompatibleType :: Type -> Type -> Type
  getCompatibleType IntType RatioType = RatioType
  getCompatibleType RatioType IntType = RatioType
  getCompatibleType t1 _ = t1
