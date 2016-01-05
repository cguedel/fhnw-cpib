module CodeGenerator (genCode) where

  import IML
  import StaticAnalysis
  import Instructions
  import AbstractSyntax

  import qualified Data.Map as M

  -- Exported functions
  genCode :: AnalyzedProgram -> (Context, [Instr])
  genCode (ctx, TypedCpsCmd cs, TypedCpsDecl ds) =
    let
      (globalCtx, _, declInstr) = genCpsDecl (ctx, Nothing) ds
      (ctx', _, cmdInstr) = genCpsCmd (globalCtx, Nothing) cs
      in
        (ctx', reverse $ Stop : cmdInstr ++ declInstr)
  genCode (_, _, _) = error "Internal error, expected TypedCpsCmd"

  -- Context
  ctxIncrLoc :: Context -> [Instr] -> Context
  ctxIncrLoc Context { ctxLoc = l, ctxVars = v, ctxRoutines = r } instr =
    Context { ctxLoc = l + length instr, ctxVars = v, ctxRoutines = r }

  ctxGetLoc :: Context -> Int
  ctxGetLoc Context { ctxLoc = l } = l

  ctxGetLocalVar :: Context -> Ident -> Ident -> Variable
  ctxGetLocalVar ctx routine ident
    | M.member ident rv = rv M.! ident
    | otherwise = error $ "Undefined local variable: " ++ ident
    where
      Routine { routVars = rv } = ctxGetRoutine ctx routine

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

  ctxGetVarAddr :: Variable -> Int
  ctxGetVarAddr Variable { varAddr = a } = a

  ctxGetGlobalVar :: Context -> Ident -> Variable
  ctxGetGlobalVar Context { ctxVars = v } ident
    | M.member ident v = v M.! ident
    | otherwise = error $ "Undefined global variable: " ++ ident

  ctxSetRoutineAddr :: Context -> Ident -> Int -> Context
  ctxSetRoutineAddr ctx routine addr =
    let
      Routine { routVars = v, routType = t, routParams = p } = ctxGetRoutine ctx routine
      in
        ctxSetRoutine ctx routine Routine { routVars = v, routType = t, routParams = p, routAddr = addr }

  -- Declarations
  genCpsDecl :: (Context, Maybe Ident) -> [TypedDecl] -> (Context, Maybe Ident, [Instr])
  genCpsDecl (ctx, ident) [] = (ctx, ident, [])
  genCpsDecl (ctx, ident) (d : ds) =
    let
      (ctx', ident', instr') = genDecl (ctx, ident) d
      (ctx'', ident'', instr'') = genCpsDecl (ctx', ident') ds
      in
        (ctx'', ident'', instr'' ++ instr')

  genDecl :: (Context, Maybe Ident) -> TypedDecl -> (Context, Maybe Ident, [Instr])
  genDecl (ctx, Just routine) (TypedStoDecl ident) =
    let
      variable = ctxGetLocalVar ctx routine ident
      instr = genLocalVarDecl variable
      ctx' = ctxIncrLoc ctx instr
      in
        (ctx', Just routine, instr)
  genDecl (ctx, Nothing) (TypedStoDecl ident) =
    let
      variable = ctxGetGlobalVar ctx ident
      instr = genGlobalVarDecl variable
      ctx' = ctxIncrLoc ctx instr
      in
        (ctx', Nothing, instr)
  genDecl (ctx, Nothing) (TypedProcDecl ident params (TypedCpsDecl locals) (TypedCpsCmd body)) =
    let
      addr = ctxGetLoc ctx
      ctx' = ctxSetRoutineAddr ctx ident (addr + 1)
      (routCtx, paramsInstr) = genParams (ctx', ident) params
      (routCtx', _, localsInstr) = genCpsDecl (routCtx, Just ident) locals
      (_, _, bodyInstr) = genCpsCmd (routCtx', Just ident) body
      routInstr = bodyInstr ++ localsInstr ++ paramsInstr
      routLength = length routInstr
      jump = UncondJump (addr + 1 + routLength)
      allInstr = routInstr ++ [jump]
      finalCtx = ctxIncrLoc ctx' allInstr
      in
        (finalCtx, Nothing, allInstr)

  genLocalVarDecl :: Variable -> [Instr]
  genLocalVarDecl Variable { varType = t, varAddr = a } = [Store, genTypeInit t, LoadAddrRel (a + 3), AllocBlock 1]

  genGlobalVarDecl :: Variable -> [Instr]
  genGlobalVarDecl Variable { varType = t, varAddr = a } = [Store, genTypeInit t, LoadImInt a, AllocBlock 1]

  genParams :: (Context, Ident) -> [Param] -> (Context, [Instr])
  genParams (ctx, _) [] = (ctx, [])
  genParams (ctx, ident) (p : ps) =
    let
      (ctx', i) = genParam (ctx, ident) p
      (ctx'', is) = genParams (ctx', ident) ps
      in
        (ctx'', is ++ i)

  genParam :: (Context, Ident) -> Param -> (Context, [Instr])
  genParam (ctx, routine) Param { parIdent = ident } =
    let
      variable = ctxGetLocalVar ctx routine ident
      addr = ctxGetVarAddr variable
      declInstr = genLocalVarDecl variable
      loadInstr = [Store, Deref, LoadAddrRel (addr * (-1) - 4), LoadAddrRel (addr + 3)]
      in
        (ctx, loadInstr ++ declInstr)

  genTypeInit :: Type -> Instr
  genTypeInit RatioType = LoadImRatio 0 1
  genTypeInit _ = LoadImInt 0

  -- Commands
  genCpsCmd :: (Context, Maybe Ident) -> [TypedCommand] -> (Context, Maybe Ident, [Instr])
  genCpsCmd (ctx, ident) [] = (ctx, ident, [])
  genCpsCmd (ctx, ident) (c : cs) =
    let
      (ctx', ident', i) = genCmd (ctx, ident) c
      (ctx'', ident'', is) = genCpsCmd (ctx', ident') cs
      in
        (ctx'', ident'', is ++ i)

  genCmd :: (Context, Maybe Ident) -> TypedCommand -> (Context, Maybe Ident, [Instr])
  genCmd (ctx, ident) TypedSkipCmd = (ctx, ident, [])
  genCmd (ctx, ident) (TypedDebugInCmd lExpr) =
    let
      (_, t) = lExpr
      exprInstr = genLExpr (ctx, ident) lExpr
      code = genDebugInCmd t : exprInstr
      ctx' = ctxIncrLoc ctx code
      in
        (ctx', ident, code)
  genCmd (ctx, ident) (TypedDebugOutCmd rExpr) =
    let
      (_, t) = rExpr
      exprInstr = genRExpr (ctx, ident) rExpr
      code = genDebugOutCmd t : exprInstr
      ctx' = ctxIncrLoc ctx code
      in
        (ctx', ident, code)
  genCmd (ctx, ident) (TypedAssiCmd lExpr rExpr) =
    let
      lExprInstr = genLExpr (ctx, ident) lExpr
      rExprInstr = genRExpr (ctx, ident) rExpr
      code = Store : rExprInstr ++ lExprInstr
      ctx' = ctxIncrLoc ctx code
      in
        (ctx', ident, code)

  genDebugInCmd :: Type -> Instr
  genDebugInCmd BoolType = InputBool ""
  genDebugInCmd IntType = InputInt ""
  genDebugInCmd RatioType = InputRatio ""

  genDebugOutCmd :: Type -> Instr
  genDebugOutCmd BoolType = OutputBool ""
  genDebugOutCmd IntType = OutputInt ""
  genDebugOutCmd RatioType = OutputRatio ""

  -- Expressions
  genLExpr :: (Context, Maybe Ident) -> LExpr -> [Instr]
  genLExpr (ctx, routine) (StoreLExpr ident _ _, _) =
    let
      (variable, isGlobal) = ctxGetVar ctx routine ident
      addr = ctxGetVarAddr variable
      in
        if isGlobal then
          [LoadImInt addr]
        else
          [LoadAddrRel (addr + 3)]

  genRExpr :: (Context, Maybe Ident) -> RExpr -> [Instr]
  genRExpr (ctx, routine) (DerefRExpr lExpr, _) = Deref : genLExpr (ctx, routine) lExpr
  genRExpr _ (LiteralRExpr value, t) = [genLiteralLoad t value]
  genRExpr (ctx, routine) (MonadicRExpr operator rExpr, t) =
    let
      rExprInstr = genRExpr (ctx, routine) rExpr
      operatorInstr = genOp t operator
      code = operatorInstr : rExprInstr
      in
        code
  genRExpr (ctx, routine) (DyadicRExpr operator rExpr1 rExpr2, _) =
    let
      rExpr1Instr = genRExpr (ctx, routine) rExpr1
      rExpr2Instr = genRExpr (ctx, routine) rExpr2
      t = getCompatibleType (getRExprType rExpr1) (getRExprType rExpr2)
      operatorInstr = genOp t operator
      code = operatorInstr : rExpr2Instr ++ rExpr1Instr
      in
        code

  genLiteralLoad :: Type -> Value -> Instr
  genLiteralLoad RatioType (RatioVal (num, denom)) = LoadImRatio num denom
  genLiteralLoad IntType (IntVal int) = LoadImInt int
  genLiteralLoad BoolType (BoolVal bool) = if bool then LoadImInt 1 else LoadImInt 0
  genLiteralLoad _ _ = error "Type mismatch for literal load instruction"

  getRExprType :: RExpr -> Type
  getRExprType (_, t) = t

  genOp :: Type -> Operator -> Instr
  genOp _ Num = NumRatio
  genOp _ Denom = DenomRatio
  genOp _ Round = RoundRatio
  genOp _ Floor = FloorRatio
  genOp _ Ceil = CeilRatio
  genOp RatioType Less = LtRatio
  genOp RatioType LessEq = LeRatio
  genOp RatioType Equal = EqRatio
  genOp RatioType NotEq = NeRatio
  genOp RatioType Greater = GtRatio
  genOp RatioType GreaterEq = GeRatio
  genOp RatioType Plus = AddRatio
  genOp RatioType Minus = SubRatio
  genOp RatioType Times = MultRatio
  genOp RatioType Div = DivTruncRatio
  genOp _ Less = LtInt
  genOp _ LessEq = LeInt
  genOp _ Equal = EqInt
  genOp _ NotEq = NeInt
  genOp _ Greater = GtInt
  genOp _ GreaterEq = GeInt
  genOp IntType Mod = ModTruncInt
  genOp _ Plus = AddInt
  genOp _ Minus = SubInt
  genOp _ Times = MultInt
  genOp _ Div = DivTruncInt
  genOp _ _ = undefined
