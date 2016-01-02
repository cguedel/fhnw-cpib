module CodeGenerator where

  import AbstractSyntax
  import IML

  genCode :: Program -> [String]
  genCode (cmd, _) = reverse (genCodeCmd cmd ["Stop"])

  genCodeCmd :: Command -> [String] -> [String]
  genCodeCmd (CpsCmd cs) code = code ++ genCodeCmd (head cs) []
  genCodeCmd SkipCmd code = code
  genCodeCmd (DebugOutCmd expr) code = ["OutputInt"] ++ genCodeExpr expr ++ code

  genCodeExpr :: Expr -> [String]
  genCodeExpr (DyadicExpr Plus e1 e2) = ["AddInt"] ++ genCodeExpr e2 ++ genCodeExpr e1
  genCodeExpr (LiteralExpr (IntVal i)) = ["LoadImInt(" ++ show i ++ ")"]
