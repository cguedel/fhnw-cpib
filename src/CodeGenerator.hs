module CodeGenerator (genCode) where

  import AbstractSyntax
  import IML

  import qualified Data.Map as M

  -- Exported functions
  genCode :: Program -> [String]
  genCode (CpsCmd cs, decl) =
    let
      (ctx, declCmds) = genDecls decl Nothing
      programBody = genCpsCmd cs ctx
      in
        reverse $ "Stop" : programBody ++ declCmds
  genCode (_, _) = error "Internal error, expected CpsCmd"

  -- Internal types
  data Variable = Variable {
    varIsGlobal :: Bool,
    varAddr :: Int,
    varType :: Type
  } deriving (Show)

  data Context = Context {
    ctxLoc :: Int,
    ctxRoutines :: M.Map String Int,
    ctxVars :: M.Map String Variable,
    ctxIsGlobal :: Bool
  } deriving (Show)

  -- Context related functions
  getVariableFromCtx :: Context -> String -> Variable
  getVariableFromCtx Context { ctxVars = v } ident = v M.! ident

  makeLocalCtx :: Context -> Context
  makeLocalCtx Context { ctxLoc = l, ctxRoutines = r, ctxVars = v } =
    Context { ctxLoc = l, ctxRoutines = r, ctxVars = v, ctxIsGlobal = False }

  lookupRoutineAddr :: Context -> String -> Int
  lookupRoutineAddr Context { ctxRoutines = d } ident = d M.! ident

  getVars :: Context -> M.Map String Variable
  getVars Context { ctxVars = v } = v

  getVarCount :: [Variable] -> Bool -> Int
  getVarCount vars isGlobal = length (filter (\Variable { varIsGlobal = g } -> g == isGlobal) vars)

  incrLoc :: Context -> Int -> Context
  incrLoc Context { ctxLoc = l, ctxRoutines = r, ctxVars = v, ctxIsGlobal = g } incr =
    Context { ctxLoc = l + incr, ctxRoutines = r, ctxVars = v, ctxIsGlobal = g }

  insertRoutine :: Context -> (String, Int) -> Context
  insertRoutine Context { ctxLoc = l, ctxRoutines = r, ctxVars = v, ctxIsGlobal = True } (ident, addr) =
    Context { ctxLoc = l, ctxRoutines = M.insert ident addr r, ctxVars = v, ctxIsGlobal = True }
  insertRoutine _ _ = error "Routines can only be declared in the global scope"

  insertVar :: Context -> (String, Type) -> Context
  insertVar Context { ctxLoc = l, ctxRoutines = r, ctxVars = v, ctxIsGlobal = g } (ident, t) =
    let variable = Variable { varAddr = getVarCount (M.elems v) g, varIsGlobal = g, varType = t }
    in Context { ctxLoc = l, ctxRoutines = r, ctxVars = M.insert ident variable v, ctxIsGlobal = g }

  getVariableType :: Variable -> Type
  getVariableType Variable { varType = t } = t

  getLoc :: Context -> Int
  getLoc Context { ctxLoc = l } = l

  -- Variables
  loadVariableAddr :: Variable -> String
  loadVariableAddr Variable { varIsGlobal = True, varAddr = a } = "LoadImInt(" ++ show a ++ ")"
  loadVariableAddr Variable { varIsGlobal = False, varAddr = a } = "LoadAddrRel(" ++ show (a + 3) ++ ")"

  initVariable :: Variable -> [String]
  initVariable var =
    let
      varT = getVariableType var
      varLoad = loadVariableAddr var
      varInit = genVarInit varT
    in
      "Store" : varInit : [varLoad]

  allocVariable :: Variable -> [String]
  allocVariable var = initVariable var ++ [allocBlock]

  allocBlock :: String
  allocBlock = "AllocBlock(1)"

  -- Declarations
  genDecls :: Maybe Decl -> Maybe Context -> (Context, [String])
  genDecls Nothing Nothing = (Context { ctxLoc = 1, ctxRoutines = M.empty, ctxVars = M.empty, ctxIsGlobal = True }, [])
  genDecls (Just decl) Nothing =
    let
      (emptyCtx, _) = genDecls Nothing Nothing
      (ctx', stoDeclCmds) = genStoDecl (emptyCtx, decl)
      (ctx'', routineDeclCmds) = genRoutineDecl (Context { ctxLoc = 1, ctxRoutines = M.empty, ctxVars = getVars ctx', ctxIsGlobal = True }, decl)
      jump = genUncondJump (getLoc ctx'')
      in
        (ctx'', stoDeclCmds ++ routineDeclCmds ++ [jump])
  genDecls Nothing (Just ctx) = (makeLocalCtx ctx, [])
  genDecls (Just decl) (Just ctx) =
    let
      localCtx = makeLocalCtx ctx
      (ctx', stoDeclCmds) = genStoDecl (localCtx, decl)
      in
        (ctx', stoDeclCmds)

  genStoDecl :: (Context, Decl) -> (Context, [String])
  genStoDecl (ctx, CpsDecl cs) = genCpsStoDecl (ctx, cs)
  genStoDecl (ctx, StoDecl (ident, t) _) =
    let
      ctx' = insertVar ctx (ident, t)
      variable = getVariableFromCtx ctx' ident
      code = allocVariable variable
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
      ctx' = insertRoutine ctx (ident, addr)
      (ctx'', declCmds) = genDecls locals (Just ctx')
      (ident, t, cm) = getReturnTypeDecl ret
      localCtx = insertVar ctx'' (ident, t)
      retAlloc = "Store" : "LoadImInt(0)" : "LoadAddrRel(3)" : ["AllocBlock(1)"]
      bodyCode = genCodeCmd body localCtx
      retCode = "Return(-4)"
      code = retCode : bodyCode ++ retAlloc
      ctx''' = incrLoc ctx' (length code)
      in
        (ctx'', code)

  genRoutineDecl (ctx, ProcDecl ident params locals body) =
    let
      addr = getLoc ctx
      ctx' = insertRoutine ctx (ident, addr)
      (ctx'', declCmds) = genDecls locals (Just ctx')
      (ctx''', paramCmds) = genParameters (zip [0..] params) ctx''
      bodyCode = genCodeCmd body ctx'''
      ret = "Return(-3)"
      code = ret : bodyCode ++ paramCmds ++declCmds
      ctx'''' = incrLoc ctx' (length code)
      in
        (ctx'''', code)

  genRoutineDecl (ctx, StoDecl _ _) = (ctx, [])

  genParameters :: [(Int, Parameter)] -> Context -> (Context, [String])
  genParameters [] ctx = (ctx, [])
  genParameters (p : ps) ctx =
    let
      (ctx', cmds) = genParameter p ctx
      (ctx'', psCmds) = genParameters ps ctx'
      in
        (ctx'', cmds ++ psCmds)

  genParameter :: (Int, Parameter) -> Context -> (Context, [String])
  genParameter (idx, ((ident, t), mechMode, changeMode)) ctx =
    let
      ctx' = insertVar ctx (ident, t)
      variable = getVariableFromCtx ctx' ident
      alloc = allocBlock
      loadVar = loadVariableAddr variable
      loadParam = "LoadAddrRel(" ++ show (-1 - idx) ++ ")"
      deref = "Deref"
      store = "Store"
      code = store : deref : loadParam : loadVar : [alloc]
      in
        (ctx', code)

  getReturnTypeDecl :: Decl -> (String, Type, Maybe ChangeMode)
  getReturnTypeDecl (StoDecl (ident, t) cm) = (ident, t, cm)
  getReturnTypeDecl decl = error $ "Internal error: expected StoDecl, but got " ++ show decl

  -- Commands
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
      variable = getVariableFromCtx ctx ident
      load = loadVariableAddr variable
      valueCode = genCodeExpr expr2 ctx
      store = "Store"
      in
        store : valueCode ++ [load]

  genCodeCmd (ProcCallCmd (ident, params)) ctx =
    let
      addr = lookupRoutineAddr ctx ident
      paramsToStack = genParamsToStack ctx params
      call = genCall addr
      in
        call : paramsToStack
  genCodeCmd SkipCmd _ = []
  genCodeCmd cmd ctx = error $ "Internal error (cmd = " ++ show cmd ++ ", ctx = " ++ show ctx ++ ")"

  genParamsToStack :: Context -> [Expr] -> [String]
  genParamsToStack _ [] = []
  genParamsToStack ctx (p : ps) = genParamToStack ctx p ++ genParamsToStack ctx ps

  genParamToStack :: Context -> Expr -> [String]
  genParamToStack ctx p = genCodeExpr p ctx

  getIdent :: Expr -> String
  getIdent (StoreExpr ident _, _) = ident

  genOutputCmd :: Type -> String
  genOutputCmd IntType = "OutputInt"
  genOutputCmd BoolType = "OutputBool"
  genOutputCmd RatioType = "OutputRatio"

  -- Expressions
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
      variable = getVariableFromCtx ctx ident
      load = loadVariableAddr variable
      deref = "Deref"
      code = deref : [load]
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

  -- Others
  genVarInit :: Type -> String
  genVarInit RatioType = "LoadImRatio(0/1)"
  genVarInit _ = "LoadImInt(0)"

  genUncondJump :: Int -> String
  genUncondJump addr = "UncondJump(" ++ show addr ++ ")"

  genCall :: Int -> String
  genCall addr = "Call(" ++ show addr ++ ")"
