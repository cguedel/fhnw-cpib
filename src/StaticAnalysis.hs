module StaticAnalysis where

  import IML
  import AbstractSyntax

  import qualified Data.Map as M
  import Data.Maybe

  -- Types
  type AnalyzedProgram = (Context, TypedCommand, TypedDecl)

  data Variable = Variable {
    varIsGlobal :: Bool,
    varAddr :: Int,
    varType :: Type,
    varChangeMode :: ChangeMode
  } deriving (Show)

  data Param = Param {
    parIdent :: String,
    parType :: Type,
    parMechMode :: MechMode,
    parChangeMode :: ChangeMode
  } deriving (Show)

  data Routine = Routine {
    routAddr :: Int,
    routType :: Maybe Type,
    routParams :: [Param],
    routCtx :: Context
  } deriving (Show)

  data Context = Context {
    ctxLoc :: Int,
    ctxRoutines :: M.Map String Routine,
    ctxVars :: M.Map String Variable,
    ctxIsGlobal :: Bool
  } deriving (Show)

  data TypedDecl
    = TypedCpsDecl [TypedDecl]
    | TypedStoDecl Ident
    | TypedFunDecl Ident [Param] TypedDecl TypedDecl TypedCommand
    | TypedProcDecl Ident [Param] TypedDecl TypedCommand
    deriving (Show)

  type ActualRoutineCall = (Ident, [ActualParameter])
  type ActualParameter = (MechMode, Either LExpr RExpr)

  type LExpr = (LExprType, Type)
  type RExpr = (RExprType, Type)

  data LExprType
    = StoreLExpr Ident ChangeMode MechMode
    deriving (Show)

  data RExprType
    = LiteralRExpr Value
    | DerefRExpr LExpr
    | FunCallRExpr ActualRoutineCall
    | MonadicRExpr Operator RExpr
    | DyadicRExpr Operator RExpr RExpr
    deriving (Show)

  data TypedCommand
    = TypedCpsCmd [TypedCommand]
    | TypedSkipCmd
    | TypedAssiCmd LExpr RExpr
    | TypedCondCmd RExpr TypedCommand TypedCommand
    | TypedWhileCmd RExpr TypedCommand
    | TypedProcCallCmd ActualRoutineCall
    | TypedDebugInCmd LExpr
    | TypedDebugOutCmd RExpr
    deriving (Show)

  -- Public functions
  analyze :: Program -> AnalyzedProgram
  analyze (CpsCmd cs, decls) =
    let
      (globalCtx, decls') = analyzeDecls decls
      cmd = analyzeCpsCmd cs globalCtx
      in
        (globalCtx, TypedCpsCmd cmd, decls')
  analyze (_, _) = error "Internal error: Expected CpsCmd"

  -- Context related stuff
  ctxMakeLocal :: Context -> Context
  ctxMakeLocal Context { ctxLoc = l, ctxRoutines = r, ctxVars = v, ctxIsGlobal = _ } =
    Context { ctxLoc = l, ctxRoutines = r, ctxVars = v, ctxIsGlobal = False }

  ctxAddVariable :: Context -> (String, Type, ChangeMode) -> Context
  ctxAddVariable Context { ctxLoc = l, ctxRoutines = r, ctxVars = v, ctxIsGlobal = g } (ident, t, cm) =
    let
      variable = Variable { varAddr = ctxGetScopeVarCount (M.elems v) g, varIsGlobal = g, varType = t, varChangeMode = cm }
      in
        Context { ctxLoc = l, ctxRoutines = r, ctxVars = M.insert ident variable v, ctxIsGlobal = g }

  ctxAddRoutine :: Context -> (String, Maybe Type, [Param], Context) -> Context
  ctxAddRoutine Context { ctxLoc = l, ctxRoutines = r, ctxVars = v, ctxIsGlobal = True } (ident, t, ps, ctx) =
    let
      routine = Routine { routAddr = 0, routType = t, routCtx = ctx, routParams = ps }
      in
        Context { ctxLoc = l, ctxRoutines = M.insert ident routine r, ctxVars = v, ctxIsGlobal = True }
  ctxAddRoutine _ _ = error "Internal error: Routines can only be added to a global context"

  ctxGetScopeVarCount :: [Variable] -> Bool -> Int
  ctxGetScopeVarCount vars isGlobal = length (filter (\Variable { varIsGlobal = g } -> g == isGlobal) vars)

  ctxCheckVarIsDefined :: Context -> Ident -> Bool
  ctxCheckVarIsDefined Context { ctxVars = v } ident
    | M.member ident v = True
    | otherwise = error $ "Undefined variable: " ++ ident

  ctxGetVariableType :: Context -> Ident -> Type
  ctxGetVariableType Context { ctxVars = v } ident =
    let
      Variable { varType = t } = v M.! ident
      in t

  ctxCheckRoutineIsDefined :: Context -> Ident -> Bool
  ctxCheckRoutineIsDefined Context { ctxRoutines = r } ident
    | M.member ident r = True
    | otherwise = error $ "Undefined function: " ++ ident

  ctxCheckIsProc :: Context -> Ident -> Bool
  ctxCheckIsProc ctx ident
    | isNothing retType = True
    | otherwise = error $ "Undefined procedure: " ++ ident
    where retType = ctxGetFunReturnType ctx ident

  ctxGetFunReturnType :: Context -> Ident -> Maybe Type
  ctxGetFunReturnType Context { ctxRoutines = r } ident =
    let
      Routine { routType = t } = r M.! ident
      in t

  -- Internal
  analyzeDecls :: Maybe Decl -> (Context, TypedDecl)
  analyzeDecls Nothing = (Context { ctxLoc = 0, ctxRoutines = M.empty, ctxVars = M.empty, ctxIsGlobal = True }, TypedCpsDecl [])
  analyzeDecls (Just (CpsDecl ds)) =
    let
      (emptyCtx, _) = analyzeDecls Nothing
      (ctx, decls') = analyzeCpsDecl ds emptyCtx
      in
        (ctx, TypedCpsDecl decls')
  analyzeDecls (Just _) = error "Internal error: Expected CpsDecl"

  analyzeDecl :: Decl -> Context -> (Context, TypedDecl)
  analyzeDecl (CpsDecl cs) ctx =
    let
      (ctx', cs') = analyzeCpsDecl cs ctx
      in
        (ctx', TypedCpsDecl cs')
  analyzeDecl (StoDecl (ident, t) cm) ctx =
    let
      ctx' = ctxAddVariable ctx (ident, t, getChangeMode cm)
      in
        (ctx', TypedStoDecl ident)
  analyzeDecl (FunDecl ident params returns locals body) ctx =
    let
      ctx' = ctxMakeLocal ctx
      retType = getFunRetType returns
      (ctx1, returns') = analyzeDecl returns ctx'
      (ctx2, locals') = analyzeCpsDecl (getCpsDecl locals) ctx1
      (ctx3, analyzedParams) = analyzeParams params ctx2
      ctx4 = ctxAddRoutine ctx (ident, Just retType, analyzedParams, ctx3)
      body' = analyzeCmd body ctx3
      in
        (ctx4, TypedFunDecl ident analyzedParams returns' (TypedCpsDecl locals') body')
  analyzeDecl (ProcDecl ident params locals body) ctx =
    let
      ctx' = ctxMakeLocal ctx
      (ctx1, locals') = analyzeCpsDecl (getCpsDecl locals) ctx'
      (ctx2, params') = analyzeParams params ctx1
      ctx3 = ctxAddRoutine ctx (ident, Nothing, params', ctx2)
      body' = analyzeCmd body ctx3
      in
        (ctx3, TypedProcDecl ident params' (TypedCpsDecl locals') body')

  analyzeCpsDecl :: [Decl] -> Context -> (Context, [TypedDecl])
  analyzeCpsDecl [] ctx = (ctx, [])
  analyzeCpsDecl (d : ds) ctx =
    let
      (ctx', d') = analyzeDecl d ctx
      (ctx'', ds') = analyzeCpsDecl ds ctx'
      in
        (ctx'', d' : ds')

  analyzeCmd :: Command -> Context -> TypedCommand
  analyzeCmd (CpsCmd cs) ctx = TypedCpsCmd (analyzeCpsCmd cs ctx)
  analyzeCmd SkipCmd _ = TypedSkipCmd
  analyzeCmd (DebugInCmd expr) ctx =
    let
      lExpr = getLExpr expr ctx
      in
        TypedDebugInCmd lExpr
  analyzeCmd (DebugOutCmd expr) ctx =
    let
      rExpr = getRExpr expr ctx
      in
        TypedDebugOutCmd rExpr
  analyzeCmd (AssiCmd expr1 expr2) ctx =
    let
      lExpr = getLExpr expr1 ctx
      rExpr = getRExpr expr2 ctx
      (lExpr', rExpr') = requireTypeCompatibleExpr lExpr rExpr
      in
        TypedAssiCmd lExpr' rExpr'
  analyzeCmd (CondCmd expr cmd1 cmd2) ctx =
    let
      rExpr = requireRExprType (getRExpr expr ctx) BoolType
      cmd1' = analyzeCmd cmd1 ctx
      cmd2' = analyzeCmd cmd2 ctx
      in
        TypedCondCmd rExpr cmd1' cmd2'
  analyzeCmd (WhileCmd expr cmd) ctx =
    let
      rExpr = requireRExprType (getRExpr expr ctx) BoolType
      cmd' = analyzeCmd cmd ctx
      in
        TypedWhileCmd rExpr cmd'
  analyzeCmd (ProcCallCmd (ident, params)) ctx =
    let
      _ = ctxCheckIsProc ctx ident
      actualRoutineCall = analyzeRoutineCall (ident, params) ctx
      in
        TypedProcCallCmd actualRoutineCall

  analyzeCpsCmd :: [Command] -> Context -> [TypedCommand]
  analyzeCpsCmd [] _ = []
  analyzeCpsCmd (c : cs) ctx =
    let
      c' = analyzeCmd c ctx
      cs' = analyzeCpsCmd cs ctx
      in
        (c' : cs')

  analyzeParams :: [Parameter] -> Context -> (Context, [Param])
  analyzeParams [] ctx = (ctx, [])
  analyzeParams (p : ps) ctx =
    let
      (ctx', p') = analyzeParam p ctx
      (ctx'', ps') = analyzeParams ps ctx'
      in
        (ctx'', p' : ps')

  analyzeParam :: Parameter -> Context -> (Context, Param)
  analyzeParam ((ident, t), mm, cm) ctx = (ctx, Param { parIdent = ident, parType = t, parMechMode = getMechMode mm, parChangeMode = getChangeMode cm })

  -- Expressions
  genLOrRExpr :: Expr -> Context -> Either LExpr RExpr
  genLOrRExpr (StoreExpr ident cm) ctx = Left (getLExpr (StoreExpr ident cm) ctx)
  genLOrRExpr expr ctx = Right (getRExpr expr ctx)

  getLExpr :: Expr -> Context -> LExpr
  getLExpr (StoreExpr ident _) ctx =
    let
      _ = ctxCheckVarIsDefined ctx ident
      t = ctxGetVariableType ctx ident
      in
        (StoreLExpr ident CONST COPY, t)
  getLExpr _ _ = error "Expected lExpr"

  getRExpr :: Expr -> Context -> RExpr
  getRExpr (LiteralExpr val) _ = (LiteralRExpr val, getLiteralType val)
  getRExpr (StoreExpr ident initialize) ctx =
    let
      (lExpr, t) = getLExpr (StoreExpr ident initialize) ctx
      in
        (DerefRExpr (lExpr, t), t)
  getRExpr (MonadicExpr op expr) ctx =
    let
      (rExpr, t) = getRExpr expr ctx
      t' = getMonadicOpType op t
      in
        (MonadicRExpr op (rExpr, t), t')
  getRExpr (DyadicExpr op expr1 expr2) ctx =
    let
      (expr1', t1) = getRExpr expr1 ctx
      (expr2', t2) = getRExpr expr2 ctx
      t' = getDyadicOpType op t1 t2
      in
        (DyadicRExpr op (expr1', t1) (expr2', t2), t')
  getRExpr (FunCallExpr (ident, params)) ctx =
    let
      _ = ctxCheckRoutineIsDefined ctx ident
      (ident', params') = analyzeRoutineCall (ident, params) ctx
      t' = fromMaybe
        (error "Routine without return type in expression, use call instead")
        (ctxGetFunReturnType ctx ident)
      in
        (FunCallRExpr (ident', params'), t')

  getLiteralType :: Value -> Type
  getLiteralType (IntVal _) = IntType
  getLiteralType (BoolVal _) = BoolType
  getLiteralType (RatioVal _) = RatioType

  -- RoutineCalls
  analyzeRoutineCall :: RoutineCall -> Context -> ActualRoutineCall
  analyzeRoutineCall (ident, params) ctx =
    let
      params' = analyzeRcExprs params ctx
      in
        (ident, params')

  analyzeRcExprs :: [Expr] -> Context -> [ActualParameter]
  analyzeRcExprs [] _ = []
  analyzeRcExprs (p : ps) ctx =
    let
      p' = analyzeRcExpr p ctx
      ps' = analyzeRcExprs ps ctx
      in
        (p' : ps')

  analyzeRcExpr :: Expr -> Context -> ActualParameter
  analyzeRcExpr expr ctx = (COPY, genLOrRExpr expr ctx)

  -- Operators
  getMonadicOpType :: Operator -> Type -> Type
  getMonadicOpType op t
    | op `elem` [Denom, Num, Floor, Ceil, Round] && t == RatioType = IntType
    | op == Not && t == BoolType = BoolType
    | op `elem` [Plus, Minus] && t == IntType = IntType
    | otherwise = error $ "Operator " ++ show op ++ " not type compatible with type " ++ show t

  getDyadicOpType :: Operator -> Type -> Type -> Type
  getDyadicOpType op t1 t2 =
    let
      t = getCompatibleType t1 t2
      t' = isDyadicOprCompatible op t
      in
        t'

  isDyadicOprCompatible :: Operator -> Type -> Type
  isDyadicOprCompatible op t
    | op `elem` [Times, Div, Plus, Minus] && t /= BoolType = t
    | op == Mod && t == IntType = t
    | op `elem` [Less, LessEq, GreaterEq, Greater] && t /= BoolType = BoolType
    | op `elem` [Equal, NotEq] && (t == IntType || t == BoolType || t == RatioType) = BoolType -- important: check t for all types to evaluate t in order to trigger errors!
    | op `elem` [CAnd, Cor] && t == BoolType = BoolType
    | otherwise = error $ "Operator " ++ show op ++ " not allowed for type " ++ show t

  getCompatibleType :: Type -> Type -> Type
  getCompatibleType RatioType IntType = RatioType
  getCompatibleType IntType RatioType = RatioType
  getCompatibleType t1 t2
    | t1 == t2 = t1
    | otherwise = error $ "Type " ++ show t1 ++ " not assignment compatible with " ++ show t2

  -- Misc stuff
  requireRExprType :: RExpr -> Type -> RExpr
  requireRExprType (rExpr, t) tReq
    | t == tReq = (rExpr, t)
    | otherwise = error $ "Expected expression of type " ++ show tReq ++ ", but got " ++ show t

  requireTypeCompatibleExpr :: LExpr -> RExpr -> (LExpr, RExpr)
  requireTypeCompatibleExpr (lExpr, lExprT) (rExpr, rExprT) =
    let
      lExprT' = getCompatibleType lExprT rExprT
      in
        ((lExpr, lExprT'), (rExpr, rExprT))

  getChangeMode :: Maybe ChangeMode -> ChangeMode
  getChangeMode Nothing = CONST
  getChangeMode (Just cm) = cm

  getMechMode :: Maybe MechMode -> MechMode
  getMechMode Nothing = COPY
  getMechMode (Just mm) = mm

  getCpsDecl :: Maybe Decl -> [Decl]
  getCpsDecl Nothing = []
  getCpsDecl (Just (CpsDecl decl)) = decl
  getCpsDecl _ = error "Internal error: expected CpsDecl"

  getFunRetType :: Decl -> Type
  getFunRetType (StoDecl (_, t) _) = t
  getFunRetType _ = error "Internal error: expected StoDecl"
