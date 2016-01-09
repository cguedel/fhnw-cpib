module StaticAnalysis where

  import IML
  import AbstractSyntax

  import qualified Data.Map as M
  import Data.Maybe

  -- Types
  type AnalyzedProgram = (Context, TypedCommand, TypedDecl)

  data Variable = Variable {
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
    routVars :: M.Map String Variable
  } deriving (Show)

  data Context = Context {
    ctxLoc :: Int,
    ctxRoutines :: M.Map String Routine,
    ctxVars :: M.Map String Variable
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

  data AccessMode
    = Direct
    | Indirect
    deriving (Show)

  data LExprType
    = StoreLExpr Ident ChangeMode AccessMode
    deriving (Show)

  data RExprType
    = LiteralRExpr Value
    | DerefRExpr LExpr
    | FunCallRExpr ActualRoutineCall
    | MonadicRExpr Operator RExpr
    | DyadicRExpr Operator RExpr RExpr
    | TypeConvRExpr RExpr
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
      cmd = analyzeCpsCmd cs (globalCtx, Nothing)
      in
        (globalCtx, TypedCpsCmd cmd, decls')
  analyze (_, _) = error "Internal error: Expected CpsCmd"

  -- Context related stuff
  ctxAddVariable :: Context -> (String, Type, ChangeMode) -> Context
  ctxAddVariable Context { ctxLoc = l, ctxRoutines = r, ctxVars = v } (ident, t, cm) =
    let
      variable = Variable { varAddr = length v, varType = t, varChangeMode = cm }
      in
        Context { ctxLoc = l, ctxRoutines = r, ctxVars = M.insert ident variable v }

  ctxAddRoutineVariable :: Context -> Ident -> (String, Type, ChangeMode) -> Context
  ctxAddRoutineVariable Context { ctxLoc = l, ctxRoutines = r, ctxVars = v } rout (ident, t, cm) =
    let
      Routine { routAddr = a, routType = rt, routParams = p, routVars = rv } = r M.! rout
      variable =
        if M.member ident v || M.member ident rv then
          error $ "Duplicate variable name " ++ show ident
        else
          Variable { varAddr = length rv, varType = t, varChangeMode = cm }
      routine' = Routine { routAddr = a, routType = rt, routParams = p, routVars = M.insert ident variable rv }
      in
        Context { ctxLoc = l, ctxRoutines = M.insert rout routine' r, ctxVars = v }

  ctxAddRoutine :: Context -> (String, Maybe Type) -> Context
  ctxAddRoutine Context { ctxLoc = l, ctxRoutines = r, ctxVars = v } (ident, t) =
    let
      routine = Routine { routAddr = 0, routType = t, routVars = M.empty, routParams = [] }
      in
        Context { ctxLoc = l, ctxRoutines = M.insert ident routine r, ctxVars = v }

  ctxGetRoutine :: Context -> Ident -> Routine
  ctxGetRoutine Context { ctxRoutines = r } ident
    | M.member ident r = r M.! ident
    | otherwise = error $ "Undefined routine: " ++ ident

  ctxSetRoutine :: Context -> Ident -> Routine -> Context
  ctxSetRoutine Context { ctxLoc = l, ctxVars = v, ctxRoutines = r } ident routine =
    Context { ctxLoc = l, ctxVars = v, ctxRoutines = M.insert ident routine r }

  ctxSetRoutineParams :: Context -> Ident -> [Param] -> Context
  ctxSetRoutineParams ctx ident params =
    let
      Routine { routVars = v, routType = t, routAddr = a } = ctxGetRoutine ctx ident
      in
        ctxSetRoutine ctx ident Routine { routVars = v, routType = t, routAddr = a, routParams = params }

  ctxCheckVarIsDefined :: Context -> Ident -> Bool
  ctxCheckVarIsDefined Context { ctxVars = v } ident
    | M.member ident v = True
    | otherwise = error $ "Undefined variable: " ++ ident

  ctxGetGlobalVar :: Context -> Ident -> Variable
  ctxGetGlobalVar Context { ctxVars = v } ident
    | M.member ident v = v M.! ident
    | otherwise = error $ "Undefined global variable: " ++ ident

  ctxGetVar :: Context -> Maybe Ident -> Ident -> (Variable, Bool)
  ctxGetVar ctx Nothing ident = (ctxGetGlobalVar ctx ident, True)
  ctxGetVar ctx (Just routineIdent) ident =
    let
      Routine { routVars = rv } = ctxGetRoutine ctx routineIdent
      isRoutineVar = M.member ident rv
      in
        if isRoutineVar then
          (rv M.! ident, False)
        else
          ctxGetVar ctx Nothing ident

  ctxGetVariableType :: (Context, Maybe Ident) -> Ident -> Type
  ctxGetVariableType (ctx, routine) ident =
    let
      (Variable { varType = t }, _) = ctxGetVar ctx routine ident
      in
        t

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
  analyzeDecls Nothing = (Context { ctxLoc = 0, ctxRoutines = M.empty, ctxVars = M.empty }, TypedCpsDecl [])
  analyzeDecls (Just (CpsDecl ds)) =
    let
      (emptyCtx, _) = analyzeDecls Nothing
      (ctx, _, decls') = analyzeCpsDecl ds (emptyCtx, Nothing)
      in
        (ctx, TypedCpsDecl decls')
  analyzeDecls (Just _) = error "Internal error: Expected CpsDecl"

  analyzeDecl :: Decl -> (Context, Maybe Ident) -> (Context, Maybe Ident, TypedDecl)
  analyzeDecl (CpsDecl ds) (ctx, routine) =
    let
      (ctx', routine', ds') = analyzeCpsDecl ds (ctx, routine)
      in
        (ctx', routine', TypedCpsDecl ds')

  analyzeDecl (StoDecl (ident, t) cm) (ctx, routine) =
    let
      ctx' =
        case routine of
          Nothing -> ctxAddVariable ctx (ident, t, getChangeMode cm)
          Just r -> ctxAddRoutineVariable ctx r (ident, t, getChangeMode cm)
      in
        (ctx', routine, TypedStoDecl ident)

  analyzeDecl (FunDecl ident params returns locals body) (ctx, _) =
    let
      ctx' = ctxAddRoutine ctx (ident, Just retType)
      (ctx1, params') = analyzeParams ident params ctx'
      retType = getFunRetType returns
      (ctx2, _, returns') = analyzeDecl returns (ctx1, Just ident)
      ctx3 = ctxSetRoutineParams ctx2 ident params'
      (ctx4, _, locals') = analyzeCpsDecl (getCpsDecl locals) (ctx3, Just ident)
      body' = analyzeCmd body (ctx4, Just ident)
      in
        (ctx4, Nothing, TypedFunDecl ident params' returns' (TypedCpsDecl locals') body')

  analyzeDecl (ProcDecl ident params locals body) (ctx, _) =
    let
      ctx' = ctxAddRoutine ctx (ident, Nothing)
      (ctx1, params') = analyzeParams ident params ctx'
      ctx2 = ctxSetRoutineParams ctx1 ident params'
      (ctx3, _, locals') = analyzeCpsDecl (getCpsDecl locals) (ctx2, Just ident)
      body' = analyzeCmd body (ctx3, Just ident)
      in
        (ctx3, Nothing, TypedProcDecl ident params' (TypedCpsDecl locals') body')

  analyzeCpsDecl :: [Decl] -> (Context, Maybe Ident) -> (Context, Maybe Ident, [TypedDecl])
  analyzeCpsDecl [] (ctx, routine) = (ctx, routine, [])
  analyzeCpsDecl (d : ds) (ctx, routine) =
    let
      (ctx', routine', d') = analyzeDecl d (ctx, routine)
      (ctx'', routine'', ds') = analyzeCpsDecl ds (ctx', routine')
      in
        (ctx'', routine'', d' : ds')

  analyzeCmd :: Command -> (Context, Maybe Ident) -> TypedCommand
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
  analyzeCmd (ProcCallCmd (ident, params)) (ctx, routine) =
    let
      _ = ctxCheckIsProc ctx ident
      actualRoutineCall = analyzeRoutineCall (ident, params) (ctx, routine)
      in
        TypedProcCallCmd actualRoutineCall

  analyzeCpsCmd :: [Command] -> (Context, Maybe Ident) -> [TypedCommand]
  analyzeCpsCmd [] _ = []
  analyzeCpsCmd (c : cs) ctx =
    let
      c' = analyzeCmd c ctx
      cs' = analyzeCpsCmd cs ctx
      in
        (c' : cs')

  analyzeParams :: Ident -> [Parameter] -> Context -> (Context, [Param])
  analyzeParams _ [] ctx = (ctx, [])
  analyzeParams ident (p : ps) ctx =
    let
      (ctx', p') = analyzeParam ident p ctx
      (ctx'', ps') = analyzeParams ident ps ctx'
      in
        (ctx'', p' : ps')

  analyzeParam :: Ident -> Parameter -> Context -> (Context, Param)
  analyzeParam routineIdent ((ident, t), mm, cm) ctx =
    let
      cm' = getChangeMode cm
      ctx' = ctxAddRoutineVariable ctx routineIdent (ident, t, cm')
        in
          (ctx', Param { parIdent = ident, parType = t, parMechMode = getMechMode mm, parChangeMode = cm' })

  -- Expressions
  getLOrRExpr :: Expr -> (Context, Maybe Ident) -> Either LExpr RExpr
  getLOrRExpr (StoreExpr ident cm) ctx = Left (getLExpr (StoreExpr ident cm) ctx)
  getLOrRExpr expr ctx = Right (getRExpr expr ctx)

  getLExpr :: Expr -> (Context, Maybe Ident) -> LExpr
  getLExpr (StoreExpr ident _) (ctx, routine) =
    let
      t = ctxGetVariableType (ctx, routine) ident
      in
        (StoreLExpr ident CONST Direct, t)
  getLExpr _ _ = error "Expected lExpr"

  getRExpr :: Expr -> (Context, Maybe Ident) -> RExpr
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
  getRExpr (FunCallExpr (ident, params)) (ctx, routine) =
    let
      (ident', params') = analyzeRoutineCall (ident, params) (ctx, routine)
      t' = fromMaybe
        (error "Routine without return type in expression, use call instead")
        (ctxGetFunReturnType ctx ident)
      in
        (FunCallRExpr (ident', params'), t')
  getRExpr (TypeConvExpr RatioType expr) ctx =
    let
      (expr', t') = getRExpr expr ctx
      t'' = case t' of
        BoolType -> error "Cannot convert bool to Ratio"
        _ -> RatioType
      in
        (TypeConvRExpr (expr', t'), t'')
  getRExpr (TypeConvExpr _ _) _ = error "Internal error, undefined type conversion expr"

  getLiteralType :: Value -> Type
  getLiteralType (IntVal _) = IntType
  getLiteralType (BoolVal _) = BoolType
  getLiteralType (RatioVal _) = RatioType

  -- RoutineCalls
  analyzeRoutineCall :: RoutineCall -> (Context, Maybe Ident) -> ActualRoutineCall
  analyzeRoutineCall (ident, params) (ctx, routine) =
    let
      Routine { routParams = p } = ctxGetRoutine ctx ident
      params' = analyzeRcExprs params (ctx, routine)
      (Just params'') = analyzeRcParams p params'
      in
        (ident, params'')

  analyzeRcParams :: [Param] -> [ActualParameter] -> Maybe [ActualParameter]
  analyzeRcParams [] [] = Just []
  analyzeRcParams (p : ps) (r : rs) =
    let
      (Just actual) = analyzeRcParam p r
      (Just actuals) = analyzeRcParams ps rs
      in
        Just (actual : actuals)
  analyzeRcParams _ _ = error "Parameter count mismatch"

  analyzeRcParam :: Param -> ActualParameter -> Maybe ActualParameter
  analyzeRcParam routParam (mm, actualParamExpr) =
    let
      exprType = getLOrRExprType actualParamExpr
      Param { parType = paramType } = routParam
      in
        if exprType == paramType || (exprType == IntType && paramType == RatioType) then
          Just (mm, actualParamExpr)
        else
          error $ "Parameter type mismatch"

  getLOrRExprType :: Either LExpr RExpr -> Type
  getLOrRExprType (Left (_, t)) = t
  getLOrRExprType (Right (_, t)) = t

  analyzeRcExprs :: [Expr] -> (Context, Maybe Ident) -> [ActualParameter]
  analyzeRcExprs [] _ = []
  analyzeRcExprs (p : ps) ctx =
    let
      p' = analyzeRcExpr p ctx
      ps' = analyzeRcExprs ps ctx
      in
        (p' : ps')

  analyzeRcExpr :: Expr -> (Context, Maybe Ident) -> ActualParameter
  analyzeRcExpr expr ctx = (COPY, getLOrRExpr expr ctx)

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
