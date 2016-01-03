module CodeGenerator where

  import AbstractSyntax
  import IML

  data Context = Context {
    loc :: Int,
    decls :: [Int]
  } deriving (Show)

  genCode :: Program -> [String]
  genCode (CpsCmd cs, _) = reverse $ "Stop" : genCpsCmd cs Context { loc = 0, decls = [] }
  genCode (_, _) = error "Internal error, expected CpsCmd"

    --reverse (genCodeCmd cmd ["Stop"])

  genCodeCmd :: Command -> Context -> [String]
  genCodeCmd (DebugOutCmd (expr, Just t)) _ =
    do
      let exprCode = genCodeExpr (expr, Just t)
      let cmd = genOutputCmd t
        in
          cmd : exprCode

  --genCodeCmd (CpsCmd cs) code = code ++ genCpsCmd cs 0
  --genCodeCmd SkipCmd code = code

  --genCodeCmd (DebugOutCmd (expr, Just t)) code =
  --  do
  --    let cmd = genOutputCmd t
  --    let exprCode = genCodeExpr (expr, Just t)
  --      in
  --        return cmd ++ exprCode ++ code

  --genCodeCmd (CondCmd expr cmd1 cmd2) code = genCodeCmd cmd2 [] ++ genCodeCmd cmd1 [] ++ ["CondJump(" ++ show (length code) ++ ")"] ++ genCodeExpr expr ++ code

  genCpsCmd :: [Command] -> Context -> [String]
  genCpsCmd (c : cs) ctx =
    do
      let instr = genCodeCmd c ctx
      let len = length instr
        in
          instr ++ genCpsCmd cs (incrLoc ctx len)
  genCpsCmd [] _ = []

  incrLoc :: Context -> Int -> Context
  incrLoc Context {loc = l, decls = a} i = Context { loc = l + i, decls = a}

  getLoc :: Context -> Int
  getLoc Context {loc = l} = l

  genOutputCmd :: Type -> String
  genOutputCmd IntType = "OutputInt"
  genOutputCmd BoolType = "OutputBool"
  genOutputCmd RatioType = "OutputRatio"

  genCodeExpr :: Expr -> [String]
  genCodeExpr (MonadicExpr opr expr, Just IntType)
    | opr `elem` [Denom,Num,Floor,Ceil,Round] = genRatioOprCode opr : exprCode
    | otherwise = "NegInt" : exprCode
    where
      exprCode = genCodeExpr expr

  genCodeExpr (MonadicExpr Minus expr, Just RatioType) = "NegRatio" : genCodeExpr expr
  genCodeExpr (MonadicExpr Not expr, Just BoolType) = "NegBool" : genCodeExpr expr
  genCodeExpr (DyadicExpr opr expr1 expr2, Just RatioType) = genRatioOprCode opr : genCodeExpr expr2 ++ genCodeExpr expr1
  genCodeExpr (DyadicExpr opr expr1 expr2, Just IntType) = genIntOprCode opr : genCodeExpr expr2 ++ genCodeExpr expr1
  genCodeExpr (DyadicExpr opr expr1 expr2, Just BoolType) = genBoolOprCode opr expr1 expr2 : genCodeExpr expr2 ++ genCodeExpr expr1
  genCodeExpr (LiteralExpr val, Just RatioType) = ["LoadImRatio(" ++ genLiteral val ++ ")"]
  genCodeExpr (LiteralExpr val, _) = ["LoadImInt(" ++ genLiteral val ++ ")"]

  genCodeExpr (MonadicExpr _ _, _) = error "Internal error (MonadicExpr)"
  genCodeExpr (x, Nothing) = error $ "Internal error, expr without type " ++ show x

  genRatioOprCode :: Operator -> String
  genRatioOprCode Denom = "DenomRatio"
  genRatioOprCode Num = "NumRatio"
  genRatioOprCode Floor = "FloorRatio"
  genRatioOprCode Ceil = "CeilRatio"
  genRatioOprCode Round = "RoundRatio"
  genRatioOprCode Plus = "AddRatio"
  genRatioOprCode Minus = "SubRatio"
  genRatioOprCode Div = "DivTruncRatio"
  genRatioOprCode Times = "MultRatio"
  genRatioOprCode opr = error "Internal error: expected rational arith operator, but got " ++ show opr

  genBoolOprCode :: Operator -> Expr -> Expr -> String
  genBoolOprCode opr (_, Just expr1Type) (_, Just expr2Type)
    | expr1Type == RatioType || expr2Type == RatioType = genBoolRatioOprCode opr
    | expr1Type == expr2Type = genIntOprCode opr
    | otherwise = error "Error: not implemented"
  genBoolOprCode _ _ _ = error "Internal error: Invalid expression"

  genBoolRatioOprCode :: Operator -> String
  genBoolRatioOprCode Greater = "GtRatio"
  genBoolRatioOprCode GreaterEq = "GeRatio"
  genBoolRatioOprCode Equal = "EqRatio"
  genBoolRatioOprCode NotEq = "NeRatio"
  genBoolRatioOprCode LessEq = "LeRatio"
  genBoolRatioOprCode Less = "LtRatio"
  genBoolRatioOprCode opr = error "Internal error: expected boolean rational operator, but got " ++ show opr

  genIntOprCode :: Operator -> String
  genIntOprCode Plus = "AddInt"
  genIntOprCode Minus = "SubInt"
  genIntOprCode Times = "MultInt"
  genIntOprCode Div = "DivTruncInt"
  genIntOprCode Greater = "GtInt"
  genIntOprCode GreaterEq = "GeInt"
  genIntOprCode Equal = "EqInt"
  genIntOprCode NotEq = "NeInt"
  genIntOprCode LessEq = "LeInt"
  genIntOprCode Less = "LtInt"
  genIntOprCode opr = error "Internal error: expected integer arith operator, but got " ++ show opr

  genLiteral :: Value -> String
  genLiteral (IntVal i) = show i
  genLiteral (BoolVal b) = if b then "1" else "0"
  genLiteral (RatioVal (denom, num)) = show denom ++ "/" ++ show num
