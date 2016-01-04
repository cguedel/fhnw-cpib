module CodeGenerator where

  import AbstractSyntax
  import IML

  import qualified Data.Map as M

  data Variable = Variable {
    isGlobal :: Bool,
    addr :: Int
  } deriving (Show)

  data Context = Context {
    loc :: Int,
    decls :: M.Map String Int,
    vars :: M.Map String Int
  } deriving (Show)

  genCode :: Program -> [String]
  genCode (CpsCmd cs, decl) =
    let
      (ctx, declCmds) = genDecls decl Nothing
      programBody = genCpsCmd cs ctx
      --jumpToFirst = "" -- "UncondJump(" ++ show (getLoc ctx + 1) ++ ")"
      in
        reverse $ "Stop" : programBody ++ declCmds -- ++ [jumpToFirst]
  genCode (_, _) = error "Internal error, expected CpsCmd"

  genDecls :: Maybe Decl -> Maybe Context -> (Context, [String])
  genDecls Nothing Nothing = (Context { loc = 0, decls = M.empty, vars = M.empty }, [])
  genDecls Nothing (Just ctx) = (Context { loc = getLoc ctx, decls = M.empty, vars = M.empty }, [])
  genDecls (Just decl) _ =
    let
      emptyCtx = Context { loc = 0, decls = M.empty, vars = M.empty }
      (ctx', stoDeclCmds) = genStoDecl (emptyCtx, decl)
      (ctx'', routineDeclCmds) = genRoutineDecl (Context { loc = 1, decls = M.empty, vars = getVars ctx' }, decl)
      jump = genUncondJump (getLoc ctx'')
      in
        (ctx'', stoDeclCmds ++ routineDeclCmds ++ [jump])

  genStoDecl :: (Context, Decl) -> (Context, [String])
  genStoDecl (ctx, CpsDecl cs) = genCpsStoDecl (ctx, cs)
  genStoDecl (ctx, StoDecl (ident, t) _) =
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
  genStoDecl (ctx, _) = (ctx, [])

  genCpsStoDecl :: (Context, [Decl]) -> (Context, [String])
  genCpsStoDecl (ctx, c : cs) =
    let
      (ctx', declCode) = genStoDecl (ctx, c)
      (ctx'', cpsDeclCode) = genCpsStoDecl (ctx', cs)
      in
        (ctx'', cpsDeclCode ++ declCode)
  genCpsStoDecl (ctx, []) = (ctx, [])

  genCpsRoutineDecl :: (Context, [Decl]) -> (Context, [String])
  genCpsRoutineDecl (ctx, c : cs) =
    let
      (ctx', declCode) = genRoutineDecl (ctx, c)
      (ctx'', cpsDeclCode) = genCpsRoutineDecl (ctx', cs)
      in
        (ctx'', cpsDeclCode ++ declCode)
  genCpsRoutineDecl (ctx, []) = (ctx, [])

  genRoutineDecl :: (Context, Decl) -> (Context, [String])
  genRoutineDecl (ctx, CpsDecl cs) = genCpsRoutineDecl (ctx, cs)
  genRoutineDecl (ctx, FunDecl ident params ret locals body) =
    let
      addr = getLoc ctx
      ctx' = insertDecl ctx (ident, addr)
      (ident, t, cm) = getReturnTypeDecl ret
      localCtx = insertVar ctx' (ident, 0)
      retAlloc = "Store" : "LoadImInt(0)" : "LoadAddrRel(3)" : ["AllocBlock(1)"]
      bodyCode = genCodeCmd body localCtx
      retCode = "Return(-4)"
      code = retCode : bodyCode ++ retAlloc
      ctx'' = incrLoc ctx' (length code)
      in
        (ctx'', code)

  genRoutineDecl (ctx, ProcDecl ident params locals body) =
    let
      addr = getLoc ctx
      ctx' = insertDecl ctx (ident, addr)
      bodyCode = genCodeCmd body ctx'
      ret = "Return(-3)"
      code = ret : bodyCode
      ctx'' = incrLoc ctx' (length code)
      in
        (ctx'', code)

  genRoutineDecl (ctx, StoDecl _ _) = (ctx, [])

  getReturnTypeDecl :: Decl -> (String, Type, Maybe ChangeMode)
  getReturnTypeDecl (StoDecl (ident, t) cm) = (ident, t, cm)
  getReturnTypeDecl decl = error $ "Internal error: expected StoDecl, but got " ++ show decl

  genVarInit :: Type -> String
  genVarInit RatioType = "LoadImRatio(0/1)"
  genVarInit _ = "LoadImInt(0)"

  genUncondJump :: Int -> String
  genUncondJump addr = "UncondJump(" ++ show addr ++ ")"

  genCall :: Int -> String
  genCall addr = "Call(" ++ show addr ++ ")"

  genBeginComment :: String -> String -> String
  genBeginComment t ident = "-- Begin of " ++ t ++ " " ++ ident ++ " --"

  genEndComment :: String -> String -> String
  genEndComment t ident = "-- End of " ++ t ++ " " ++ ident ++ " --"

  genCpsCmd :: [Command] -> Context -> [String]
  genCpsCmd (c : cs) ctx =
    do
      let instr = genCodeCmd c ctx
      let len = length instr
        in
          genCpsCmd cs (incrLoc ctx len) ++ instr
  genCpsCmd [] _ = []

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
  genCodeCmd (ProcCallCmd (ident, params)) ctx =
    let
      addr = lookupRoutineAddr ctx ident
      call = genCall addr
      in
        [call]
  genCodeCmd SkipCmd _ = []
  genCodeCmd cmd ctx = error $ "Internal error (cmd = " ++ show cmd ++ ", ctx = " ++ show ctx ++ ")"

  genAddrLoad :: Int -> String
  genAddrLoad addr = "LoadImInt(" ++ show addr ++ ")"

  getIdent :: Expr -> String
  getIdent (StoreExpr ident _, _) = ident

  lookupVarAddr :: Context -> String -> Int
  lookupVarAddr Context { vars = v } ident = v M.! ident

  lookupRoutineAddr :: Context -> String -> Int
  lookupRoutineAddr Context { decls = d } ident = d M.! ident

  getVarCount :: Context -> Int
  getVarCount ctx = length (getVars ctx)

  getVars :: Context -> M.Map String Int
  getVars Context { vars = v } = v

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

  genCodeExpr (FunCallExpr call, _) _ = ["Call(15)"]
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
