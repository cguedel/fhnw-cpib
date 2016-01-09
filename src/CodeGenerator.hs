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
  ctxIncrLoc ctx instr = ctxIncrLocBy ctx (length instr)

  ctxIncrLocBy :: Context -> Int -> Context
  ctxIncrLocBy Context { ctxLoc = l, ctxVars = v, ctxRoutines = r } i =
    Context { ctxLoc = l + i, ctxVars = v, ctxRoutines = r }

  ctxGetLoc :: Context -> Int
  ctxGetLoc Context { ctxLoc = l } = l

  ctxGetLocalVar :: Context -> Ident -> Ident -> Variable
  ctxGetLocalVar ctx routine ident
    | M.member ident rv = rv M.! ident
    | otherwise = error $ "Undefined local variable: " ++ ident
    where
      Routine { routVars = rv } = ctxGetRoutine ctx routine

  ctxGetVarAddr :: Variable -> Int
  ctxGetVarAddr Variable { varAddr = a } = a

  ctxSetRoutineAddr :: Context -> Ident -> Int -> Context
  ctxSetRoutineAddr ctx routine addr =
    let
      Routine { routVars = v, routType = t, routParams = p } = ctxGetRoutine ctx routine
      in
        ctxSetRoutine ctx routine Routine { routVars = v, routType = t, routParams = p, routAddr = addr }

  ctxGetRoutineAddr :: Context -> Ident -> Int
  ctxGetRoutineAddr ctx routine =
    let
      Routine { routAddr = a } = ctxGetRoutine ctx routine
      in
        a

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
      ctx' = ctxIncrLocBy ctx 1 -- Make room for the uncondjump to skip over the routine at program start
      addr = ctxGetLoc ctx'
      ctx'' = ctxSetRoutineAddr ctx' ident addr
      paramsInstr = genParams (ctx'', ident) params
      routCtx' = ctxIncrLoc ctx'' paramsInstr
      (routCtx'', _, localsInstr) = genCpsDecl (routCtx', Just ident) locals
      (_, _, bodyInstr) = genCpsCmd (routCtx'', Just ident) body
      routInstr = Return 0 : bodyInstr ++ localsInstr ++ paramsInstr
      routLength = length routInstr
      jump = UncondJump (addr + routLength)
      allInstr = routInstr
      finalCtx = ctxIncrLoc ctx'' allInstr
      in
        (finalCtx, Nothing, allInstr ++ [jump])
  genDecl (ctx, Nothing) (TypedFunDecl ident params returns (TypedCpsDecl locals) (TypedCpsCmd body)) =
    let
      ctx' = ctxIncrLocBy ctx 1 -- Make room for the uncondjump to skip over the routine at program start
      addr = ctxGetLoc ctx'
      ctx'' = ctxSetRoutineAddr ctx' ident addr
      (routCtx, _, returnInstr) = genDecl (ctx'', Just ident) returns
      paramsInstr = genParams (routCtx, ident) params
      routCtx' = ctxIncrLoc routCtx paramsInstr
      (routCtx'', _, localsInstr) = genCpsDecl (routCtx', Just ident) locals
      (_, _, bodyInstr) = genCpsCmd (routCtx'', Just ident) body
      routInstr = Return 0 : Store: Deref : LoadAddrRel (3 + length params) : LoadAddrRel (-1) : bodyInstr ++ localsInstr ++ paramsInstr ++ returnInstr
      routLength = length routInstr
      jump = UncondJump (addr + routLength)
      allInstr = routInstr
      finalCtx = ctxIncrLoc ctx'' allInstr
      in
        (finalCtx, Nothing, allInstr ++ [jump])
  genDecl _ _ = error "Internal error: Unknown declaration"

  genLocalVarDecl :: Variable -> [Instr]
  genLocalVarDecl Variable { varType = t, varAddr = a } = [Store, genTypeInit t, LoadAddrRel (a + 3), AllocBlock 1]

  genGlobalVarDecl :: Variable -> [Instr]
  genGlobalVarDecl Variable { varType = t, varAddr = a } = [Store, genTypeInit t, LoadImInt a, AllocBlock 1]

  genParams :: (Context, Ident) -> [Param] -> [Instr]
  genParams (_, _) [] = []
  genParams (ctx, ident) (p : ps) =
    let
      i = genParam (ctx, ident) p
      is = genParams (ctx, ident) ps
      in
        is ++ i

  genParam :: (Context, Ident) -> Param -> [Instr]
  genParam (ctx, routine) Param { parIdent = ident } =
    let
      variable = ctxGetLocalVar ctx routine ident
      addr = ctxGetVarAddr variable
      declInstr = genLocalVarDecl variable
      loadInstr = [Store, Deref, LoadAddrRel (-3 + addr), LoadAddrRel (addr + 3)]
      in
        loadInstr ++ declInstr

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
  genCmd (ctx, ident) (TypedCpsCmd cs) = genCpsCmd (ctx, ident) cs
  genCmd (ctx, ident) TypedSkipCmd = (ctx, ident, [])
  genCmd (ctx, ident) (TypedDebugInCmd lExpr) =
    let
      (_, t) = lExpr
      exprInstr = genLExpr (ctx, ident) lExpr
      code = genDebugInCmd "" t : exprInstr
      ctx' = ctxIncrLoc ctx code
      in
        (ctx', ident, code)
  genCmd (ctx, ident) (TypedDebugOutCmd rExpr) =
    let
      (_, t) = rExpr
      exprInstr = genRExpr (ctx, ident) rExpr
      code = genDebugOutCmd "" t : exprInstr
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
  genCmd (ctx, ident) (TypedCondCmd cond cmd1 cmd2) =
    let
      condExprInstr = genRExpr (ctx, ident) cond
      ctx' = ctxIncrLocBy ctx (length condExprInstr + 1)
      (_, _, cmd1Instr) = genCmd (ctx', ident) cmd1
      ctx'' = ctxIncrLocBy ctx' (length cmd1Instr + 1)
      elseAddr = ctxGetLoc ctx''
      (_, _, cmd2Instr) = genCmd (ctx'', ident) cmd2
      endAddr = ctxGetLoc ctx'' + length cmd2Instr
      code = cmd2Instr ++ [UncondJump endAddr] ++ cmd1Instr ++ [CondJump elseAddr] ++ condExprInstr
      finalCtx = ctxIncrLoc ctx code
      in
        (finalCtx, ident, code)
  genCmd (ctx, ident) (TypedWhileCmd cond body) =
    let
      condAddr = ctxGetLoc ctx
      condExprInstr = genRExpr (ctx, ident) cond
      ctx' = ctxIncrLocBy ctx (length condExprInstr + 1)
      (_, _, bodyInstr) = genCmd (ctx', ident) body
      endAddr = ctxGetLoc ctx' + length bodyInstr + 1
      code = [UncondJump condAddr] ++ bodyInstr ++ [CondJump endAddr] ++ condExprInstr
      finalCtx = ctxIncrLoc ctx code
      in
        (finalCtx, ident, code)
  genCmd (ctx, ident) (TypedProcCallCmd (routineIdent, params)) =
    let
      routineAddr = ctxGetRoutineAddr ctx routineIdent
      paramInstr = genParamsLoad (ctx, ident) params
      code = Call routineAddr : paramInstr
      finalCtx = ctxIncrLoc ctx code
      in
        (finalCtx, ident, code)

  genDebugInCmd :: String -> Type -> Instr
  genDebugInCmd s BoolType = InputBool s
  genDebugInCmd s IntType = InputInt s
  genDebugInCmd s RatioType = InputRatio s

  genDebugOutCmd :: String -> Type -> Instr
  genDebugOutCmd s BoolType = OutputBool s
  genDebugOutCmd s IntType = OutputInt s
  genDebugOutCmd s RatioType = OutputRatio s

  genParamsLoad :: (Context, Maybe Ident) -> [ActualParameter] -> [Instr]
  genParamsLoad (_, _) [] = []
  genParamsLoad (ctx, ident) (p : ps) =
    let
      instr = genParamLoad (ctx, ident) p
      instrs = genParamsLoad (ctx, ident) ps
      in
        (instr ++ instrs)

  genParamLoad :: (Context, Maybe Ident) -> ActualParameter -> [Instr]
  genParamLoad (ctx, ident) (_, Left expr) = Deref : genLExpr (ctx, ident) expr
  genParamLoad (ctx, ident) (_, Right expr) = genRExpr (ctx, ident) expr

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
      operatorInstr = genMonadicOp t operator
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
  genRExpr (ctx, routine) (FunCallRExpr (routineIdent, params), _) =
    let
      routineAddr = ctxGetRoutineAddr ctx routineIdent
      paramInstr = genParamsLoad (ctx, routine) (reverse params)
      code = Call routineAddr : AllocBlock 1 : paramInstr -- ++ [AllocBlock 1]
      in
        code

  -- Type conversion to ratio is handled internally in the VM
  genRExpr (ctx, routine) (TypeConvRExpr expr, _) = genRExpr (ctx, routine) expr

  genLiteralLoad :: Type -> Value -> Instr
  genLiteralLoad RatioType (RatioVal (num, denom)) = LoadImRatio num denom
  genLiteralLoad IntType (IntVal int) = LoadImInt int
  genLiteralLoad BoolType (BoolVal bool) = if bool then LoadImInt 1 else LoadImInt 0
  genLiteralLoad _ _ = error "Type mismatch for literal load instruction"

  getRExprType :: RExpr -> Type
  getRExprType (_, t) = t

  genMonadicOp :: Type -> Operator -> Instr
  genMonadicOp _ Num = NumRatio
  genMonadicOp _ Denom = DenomRatio
  genMonadicOp _ Round = RoundRatio
  genMonadicOp _ Floor = FloorRatio
  genMonadicOp _ Ceil = CeilRatio
  genMonadicOp IntType Minus = NegInt
  genMonadicOp RatioType Minus = NegRatio
  genMonadicOp BoolType Not = NegBool
  genMonadicOp t op = error $ "Illegal monadic operator " ++ show op ++ " for type " ++ show t

  genOp :: Type -> Operator -> Instr
  genOp RatioType Less = LtRatio
  genOp RatioType LessEq = LeRatio
  genOp RatioType Equal = EqRatio
  genOp RatioType NotEq = NeRatio
  genOp RatioType Greater = GtRatio
  genOp RatioType GreaterEq = GeRatio
  genOp RatioType Plus = AddRatio
  genOp RatioType Minus = SubRatio
  genOp RatioType Times = MultRatio
  genOp RatioType Div = DivRatio
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
  genOp BoolType CAnd = MultInt
  genOp BoolType Cor = AddInt
  genOp t op = error $ "Illegal operator " ++ show op ++ " for type " ++ show t
