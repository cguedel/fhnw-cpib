module CodeGenerator where

  import AbstractSyntax
  import IML

  import qualified Data.Map as M

  data Context = Context {
    loc :: Int,
    decls :: M.Map String Int,
    vars :: M.Map String Int
  } deriving (Show)

  genCode :: Program -> [String]
  genCode (CpsCmd cs, decl) =
    let
      (ctx, declCmds) = genDecls decl
      programBody = genCpsCmd cs ctx
      --jumpToFirst = "" -- "UncondJump(" ++ show (getLoc ctx + 1) ++ ")"
      in
        reverse $ "Stop" : programBody ++ declCmds -- ++ [jumpToFirst]
  genCode (_, _) = error "Internal error, expected CpsCmd"

  genDecls :: Maybe Decl -> (Context, [String])
  genDecls Nothing = (Context { loc = 0, decls = M.empty, vars = M.empty }, [])
  genDecls (Just decl) = genDecl (Context { loc = 0, decls = M.empty, vars = M.empty }, decl)

  genDecl :: (Context, Decl) -> (Context, [String])
  genDecl (ctx, CpsDecl cs) = genCpsDecl (ctx, cs)
  genDecl (ctx, FunDecl ident params ret locals body) =
    let
      addr = getLoc ctx
      ctx' = insertDecl ctx (ident, addr)
      retCode = "Return(1)"
      bodyCode = genCodeCmd body ctx'
      code = genEndComment "fun" ident : retCode : bodyCode ++ [genBeginComment "fun" ident]
      ctx'' = incrLoc ctx' (length code - 2)
      in
        (ctx'', code)

  genDecl (ctx, StoDecl (ident, t) _) =
    let
      addr = getVarCount ctx
      ctx' = insertVar ctx (ident, addr)
      alloc = "AllocBlock(1)"
      addrLoad = genAddrLoad addr
      valLoad = genVarInit t
      store = "Store"
      code = store : valLoad : addrLoad : [alloc]
      ctx'' = incrLoc ctx' (length code)
      in
        (ctx'', code)

  genVarInit :: Type -> String
  genVarInit RatioType = "LoadImRatio(0/1)"
  genVarInit _ = "LoadImInt(0)"

  genBeginComment :: String -> String -> String
  genBeginComment t ident = "-- Begin of " ++ t ++ " " ++ ident ++ " --"

  genEndComment :: String -> String -> String
  genEndComment t ident = "-- End of " ++ t ++ " " ++ ident ++ " --"

  genCpsDecl :: (Context, [Decl]) -> (Context, [String])
  genCpsDecl (ctx, c : cs) =
    let
      (ctx', declCode) = genDecl (ctx, c)
      (ctx'', cpsDeclCode) = genCpsDecl (ctx', cs)
      in
        (ctx'', cpsDeclCode ++ declCode)
  genCpsDecl (ctx, []) = (ctx, [])

  genCodeCmd :: Command -> Context -> [String]
  genCodeCmd (CpsCmd cs) ctx = genCpsCmd cs ctx
  genCodeCmd (DebugOutCmd (expr, Just t)) ctx =
    let
      exprCode = genCodeExpr (expr, Just t) ctx
      cmd = genOutputCmd t
      in
        cmd : exprCode
  genCodeCmd (AssiCmd expr1 expr2) ctx =
    let
      ident = getIdent expr1
      addr = lookupVarAddr ctx ident
      loadAddr = genAddrLoad addr
      valueCode = genCodeExpr expr2 ctx
      store = "Store"
      in
        store : valueCode ++ [loadAddr]
  genCodeCmd cmd ctx = error $ "Internal error (cmd = " ++ show cmd ++ ", ctx = " ++ show ctx ++ ")"

  genAddrLoad :: Int -> String
  genAddrLoad addr = "LoadImInt(" ++ show addr ++ ")"

  getIdent :: Expr -> String
  getIdent (StoreExpr ident _, _) = ident

  lookupVarAddr :: Context -> String -> Int
  lookupVarAddr Context { vars = v } ident = v M.! ident

  getVarCount :: Context -> Int
  getVarCount Context { vars = v } = length v

  genCpsCmd :: [Command] -> Context -> [String]
  genCpsCmd (c : cs) ctx =
    do
      let instr = genCodeCmd c ctx
      let len = length instr
        in
          genCpsCmd cs (incrLoc ctx len) ++ instr
  genCpsCmd [] _ = []

  incrLoc :: Context -> Int -> Context
  incrLoc Context { loc = l, decls = a, vars = v } i = Context { loc = l + i, decls = a, vars = v }

  insertDecl :: Context -> (String, Int) -> Context
  insertDecl Context { loc = l, decls = m, vars = v } (ident, addr) = Context { loc = l, decls = M.insert ident addr m, vars = v }

  insertVar :: Context -> (String, Int) -> Context
  insertVar Context { loc = l, decls = m, vars = v } (ident, addr) = Context { loc = l, decls = m, vars = M.insert ident addr v }

  getLoc :: Context -> Int
  getLoc Context {loc = l} = l

  genOutputCmd :: Type -> String
  genOutputCmd IntType = "OutputInt"
  genOutputCmd BoolType = "OutputBool"
  genOutputCmd RatioType = "OutputRatio"

  genCodeExpr :: Expr -> Context -> [String]
  genCodeExpr (MonadicExpr opr expr, Just IntType) ctx
    | opr `elem` [Denom,Num,Floor,Ceil,Round] = genRatioOprCode opr : exprCode ctx
    | otherwise = "NegInt" : exprCode ctx
    where
      exprCode = genCodeExpr expr

  genCodeExpr (MonadicExpr Minus expr, Just RatioType) ctx = "NegRatio" : genCodeExpr expr ctx
  genCodeExpr (MonadicExpr Not expr, Just BoolType) ctx = "NegBool" : genCodeExpr expr ctx
  genCodeExpr (DyadicExpr opr expr1 expr2, Just RatioType) ctx = genRatioOprCode opr : genCodeExpr expr2 ctx ++ genCodeExpr expr1 ctx
  genCodeExpr (DyadicExpr opr expr1 expr2, Just IntType) ctx = genIntOprCode opr : genCodeExpr expr2 ctx ++ genCodeExpr expr1 ctx
  genCodeExpr (DyadicExpr opr expr1 expr2, Just BoolType) ctx = genBoolOprCode opr expr1 expr2 : genCodeExpr expr2 ctx ++ genCodeExpr expr1 ctx
  genCodeExpr (LiteralExpr val, Just RatioType) _ = ["LoadImRatio(" ++ genLiteral val ++ ")"]
  genCodeExpr (LiteralExpr val, _) _ = ["LoadImInt(" ++ genLiteral val ++ ")"]

  genCodeExpr (FunCallExpr call, _) _ = ["Call(1)"]
  genCodeExpr (StoreExpr ident _, _) ctx =
    let
      addr = lookupVarAddr ctx ident
      loadAddr = genAddrLoad addr
      deref = "Deref"
      code = deref : [loadAddr]
      ctx' = incrLoc ctx (length code)
      in
        code

  genCodeExpr (MonadicExpr _ _, _) _ = error "Internal error (MonadicExpr)"
  genCodeExpr (x, Nothing) _ = error $ "Internal error, expr without type " ++ show x

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
