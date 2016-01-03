module CodeGenerator where

  import AbstractSyntax
  import IML

  genCode :: Program -> [String]
  genCode (cmd, _) = reverse (genCodeCmd cmd ["Stop"])

  genCodeCmd :: Command -> [String] -> [String]
  genCodeCmd (CpsCmd cs) code = code ++ genCpsCmd cs
  genCodeCmd SkipCmd code = code

  genCodeCmd (DebugOutCmd (expr, Just t)) code =
    do
      let cmd = genOutputCmd t
      let exprCode = genCodeExpr (expr, Just t)
        in
          return cmd ++ exprCode ++ code

  genCpsCmd :: [Command] -> [String]
  genCpsCmd (c : cs) = genCpsCmd cs ++ genCodeCmd c []
  genCpsCmd [] = []

  genOutputCmd :: Type -> String
  genOutputCmd IntType = "OutputInt"
  genOutputCmd BoolType = "OutputBool"
  genOutputCmd RatioType = "OutputRatio"

  genCodeExpr :: Expr -> [String]
  genCodeExpr (MonadicExpr opr expr, Just IntType) = genRatioOprCode opr : genCodeExpr expr
  genCodeExpr (DyadicExpr opr expr1 expr2, Just RatioType) = genRatioOprCode opr : genCodeExpr expr2 ++ genCodeExpr expr1
  genCodeExpr (DyadicExpr opr expr1 expr2, Just IntType) = genIntOprCode opr : genCodeExpr expr2 ++ genCodeExpr expr1
  genCodeExpr (DyadicExpr opr expr1 expr2, Just BoolType) = genBoolOprCode opr : genCodeExpr expr2 ++ genCodeExpr expr1
  genCodeExpr (LiteralExpr val, Just RatioType) = ["LoadImRatio(" ++ genLiteral val ++ ")"]
  genCodeExpr (LiteralExpr val, _) = ["LoadImInt(" ++ genLiteral val ++ ")"]

  genRatioOprCode :: Operator -> String
  genRatioOprCode Denom = "DenomRatio"
  genRatioOprCode Num = "NumRatio"
  genRatioOprCode Floor = "FloorRatio"
  genRatioOprCode Ceil = "CeilRatio"
  genRatioOprCode Round = "RoundRatio"
  genRatioOprCode Plus = "AddRatio"

  genBoolOprCode :: Operator -> String
  genBoolOprCode Greater = "GtInt"

  genIntOprCode :: Operator -> String
  genIntOprCode Plus = "AddInt"

  genLiteral :: Value -> String
  genLiteral (IntVal i) = show i
  genLiteral (BoolVal b) = if b then "1" else "0"
  genLiteral (RatioVal (denom, num)) = show denom ++ "/" ++ show num