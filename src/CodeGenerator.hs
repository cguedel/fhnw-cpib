module CodeGenerator (genCode) where

  import IML
  import StaticAnalysis
  import Instructions
  import AbstractSyntax

  type ScopedContext = (Context, Maybe Ident)

  -- Exported functions
  genCode :: AnalyzedProgram -> (Context, [Instr])
  genCode (ctx, TypedCpsCmd cs, TypedCpsDecl ds) =
    let
      (globalCtx, declInstr) = genCpsDecl (ctx, Nothing) ds
      (globalCtx', cmdInstr) = genCpsCmd globalCtx cs
      (finalCtx, _) = globalCtx'
      in
        (finalCtx, reverse $ Stop : cmdInstr ++ declInstr)
  genCode (_, _, _) = error "Internal error, expected TypedCpsCmd"

  -- Context related functions
  ctxGetLoc :: ScopedContext -> Int
  ctxGetLoc (Context { ctxLoc = l }, _) = l

  ctxIncrLoc :: ScopedContext -> Int -> ScopedContext
  ctxIncrLoc (Context { ctxLoc = l, ctxVars = v, ctxRoutines = r }, ident) i =
    (Context { ctxLoc = l + i, ctxVars = v, ctxRoutines = r }, ident)

  ctxSetRoutAddr :: ScopedContext -> Ident -> Int -> ScopedContext
  ctxSetRoutAddr (ctx, ident) routIdent i =
    let
      Routine { routVars = rv, routType = t, routParams = p } = ctxGetRoutine ctx routIdent
      routine' = Routine { routAddr = i, routType = t, routParams = p, routVars = rv }
      ctx' = ctxSetRoutine ctx routIdent routine'
      in
        (ctx', ident)

  ctxGetRoutAddr :: ScopedContext -> Ident -> Int
  ctxGetRoutAddr (ctx, _) routIdent =
    let
      Routine { routAddr = a } = ctxGetRoutine ctx routIdent
      in
        a

  ctxGetVarAddr :: ScopedContext -> Ident -> (Int, Bool)
  ctxGetVarAddr (ctx, ident) varIdent =
    let
      (Variable { varAddr = a }, isGlobal) = ctxGetVar ctx ident varIdent
      in
        (a, isGlobal)

  -- Declarations
  genCpsDecl :: ScopedContext -> [TypedDecl] -> (ScopedContext, [Instr])
  genCpsDecl ctx [] = (ctx, [])
  genCpsDecl ctx (d : ds) =
    let
      (ctx', instr) = genDecl ctx d
      (ctx'', instr') = genCpsDecl ctx' ds
      in
        (ctx'', instr' ++ instr)

  genDecl :: ScopedContext -> TypedDecl -> (ScopedContext, [Instr])
  genDecl ctx (TypedProcDecl ident params (TypedCpsDecl locals) (TypedCpsCmd body)) =
    genRoutine ctx (ident, params, Nothing, locals, body)

  genDecl ctx (TypedFunDecl ident params returns (TypedCpsDecl locals) (TypedCpsCmd body)) =
    genRoutine ctx (ident, params, Just returns, locals, body)

  genDecl ctx (TypedStoDecl ident) =
    let
      loadInstr = genVarInit ctx ident
      in
        (ctxIncrLoc ctx (length loadInstr), loadInstr)

  genRoutine :: ScopedContext -> (Ident, [Param], Maybe TypedDecl, [TypedDecl], [TypedCommand]) -> (ScopedContext, [Instr])
  genRoutine ctx (ident, params, returns, locals, body) =
    let
      ctx' = ctxIncrLoc ctx 1 -- Make room for the jump instruction that is not part of the routine
      routineAddress = ctxGetLoc ctx'
      (ctx'', _) = ctxSetRoutAddr ctx' ident routineAddress
      alloc = genVarInits (ctx'', Just ident) $ map (\(TypedStoDecl i) -> i) locals
      retInit = case returns of
        Just (TypedStoDecl x) -> genVarInit (ctx'', Just ident) x
        Nothing -> []
        _ -> error "Internal error"
      (ctx3, _) = ctxIncrLoc (ctx'', Nothing) (length alloc + length retInit)
      (ctx4, body') = genCpsCmd (ctx3, Just ident) body
      code = Return (length params) : body' ++ alloc ++ retInit
      instrs = code ++ [UncondJump (routineAddress + length code)]
      in
        (ctxIncrLoc ctx4 1, instrs)

  genVarInits :: ScopedContext -> [Ident] -> [Instr]
  genVarInits _ [] = []
  genVarInits ctx (t : ts) =
    let
      i = genVarInit ctx t
      is = genVarInits ctx ts
      in
        i ++ is

  genVarInit :: ScopedContext -> Ident -> [Instr]
  genVarInit ctx var =
    let
      t = ctxGetVariableType ctx var
      load = genVariableLoad ctx var
      initialize =
        case t of
          IntType -> LoadImInt 0
          RatioType -> LoadImRatio 0 1
          BoolType -> LoadImInt 0
      in
        [Store, initialize, load, AllocBlock 1]

  -- Commands
  genCpsCmd :: ScopedContext -> [TypedCommand] -> (ScopedContext, [Instr])
  genCpsCmd ctx [] = (ctx, [])
  genCpsCmd ctx (c : cs) =
    let
      (ctx', instr) = genCmd ctx c
      (ctx'', instr') = genCpsCmd ctx' cs
      in
        (ctx'', instr' ++ instr)

  genCmd :: ScopedContext -> TypedCommand -> (ScopedContext, [Instr])
  genCmd ctx (TypedSkipCmd) = (ctx, [])
  genCmd ctx (TypedCpsCmd cs) = genCpsCmd ctx cs
  genCmd ctx (TypedDebugInCmd lExpr) =
    let
      (ctx', exprInstr) = genLExpr ctx lExpr
      cmd = genInputCmd $ getLExprType lExpr
      code = cmd : exprInstr
      in
        (ctxIncrLoc ctx' 1, code)
  genCmd ctx (TypedDebugOutCmd rExpr) =
    let
      (ctx', exprInstr) = genRExpr ctx rExpr
      cmd = genOutputCmd $ getRExprType rExpr
      code = cmd : exprInstr
      in
        (ctxIncrLoc ctx' 1, code)
  genCmd ctx (TypedProcCallCmd routineCall) = genRoutineCall ctx routineCall
  genCmd ctx (TypedAssiCmd lExpr rExpr) =
    let
      (ctx', lExprInstr) = genLExpr ctx lExpr
      (ctx'', rExprInstr) = genRExpr ctx' rExpr
      cmd = Store
      in
        (ctxIncrLoc ctx'' 1, cmd : rExprInstr ++ lExprInstr)
  genCmd ctx (TypedWhileCmd expr (TypedCpsCmd body)) =
    let
      condAddr = ctxGetLoc ctx
      (ctx', exprInstr) = genRExpr ctx expr
      ctx'' = ctxIncrLoc ctx' 1
      (ctx''', body') = genCpsCmd ctx'' body
      endAddr = ctxGetLoc ctx'''
      code = UncondJump condAddr : body' ++ [CondJump (endAddr + 1)] ++ exprInstr
      in
        (ctxIncrLoc ctx''' 1, code)
  genCmd ctx (TypedCondCmd expr (TypedCpsCmd body1) (TypedCpsCmd body2)) =
    let
      (ctx', condInstr) = genRExpr ctx expr
      ctx'' = ctxIncrLoc ctx' 1 -- for the conditional jump
      (ctx3, body1Instr) = genCpsCmd ctx'' body1
      ctx4 = ctxIncrLoc ctx3 1 -- for the unconditional jump
      elseAddr = ctxGetLoc ctx4
      (ctx5, body2Instr) = genCpsCmd ctx4 body2
      endAddr = ctxGetLoc ctx5
      code = body2Instr ++ [UncondJump endAddr] ++ body1Instr ++ [CondJump elseAddr] ++ condInstr
      in
        (ctx5, code)
  genCmd _ _ = error "Internal error, unknown command"

  genOutputCmd :: Type -> Instr
  genOutputCmd RatioType = OutputRatio ""
  genOutputCmd IntType = OutputInt ""
  genOutputCmd BoolType = OutputBool ""

  genInputCmd :: Type -> Instr
  genInputCmd RatioType = InputRatio ""
  genInputCmd IntType = InputInt ""
  genInputCmd BoolType = InputBool ""

  -- Routine calls
  genRoutineCall :: ScopedContext -> ActualRoutineCall -> (ScopedContext, [Instr])
  genRoutineCall ctx (ident, params) =
    let
      routineAddress = ctxGetRoutAddr ctx ident
      (ctx', paramsLoad) = genParamsLoad ctx $ reverse params
      callInstr = Call routineAddress
      in
        (ctxIncrLoc ctx' 1, callInstr : paramsLoad)

  genParamsLoad :: ScopedContext -> [ActualParameter] -> (ScopedContext, [Instr])
  genParamsLoad ctx [] = (ctx, [])
  genParamsLoad ctx (p : ps) =
    let
      (ctx', instr) = genParamLoad ctx p
      (ctx'', instr') = genParamsLoad ctx' ps
      in
        (ctx'', instr' ++ instr)

  genParamLoad :: ScopedContext -> ActualParameter -> (ScopedContext, [Instr])
  genParamLoad ctx (_, Left lExpr) =
    let
      (ctx', loadInstr) = genLExpr ctx lExpr
      in
        (ctxIncrLoc ctx' 1, Deref : loadInstr)
  genParamLoad ctx (_, Right rExpr) = genRExpr ctx rExpr

  genVariableLoad :: ScopedContext -> Ident -> Instr
  genVariableLoad ctx ident =
    let (addr, isGlobal) = ctxGetVarAddr ctx ident
      in
        if isGlobal then
          LoadImInt addr
        else
          LoadAddrRel addr

  -- Expressions
  getRExprType :: RExpr -> Type
  getRExprType (_, t) = t

  getLExprType :: LExpr -> Type
  getLExprType (_, t) = t

  genLExpr :: ScopedContext -> LExpr -> (ScopedContext, [Instr])
  genLExpr ctx (StoreLExpr ident _ _, _) =
    let
      code = [genVariableLoad ctx ident]
      in
        (ctxIncrLoc ctx 1, code)

  genRExpr :: ScopedContext -> RExpr -> (ScopedContext, [Instr])
  genRExpr ctx (DerefRExpr lExpr, _) =
    let
      (ctx', lExprInstr) = genLExpr ctx lExpr
      in
        (ctxIncrLoc ctx' 1, Deref : lExprInstr)
  genRExpr ctx (LiteralRExpr val, _) =
    let
      load = genLiteralLoad val
      in
        (ctxIncrLoc ctx 1, [load])
  genRExpr ctx (FunCallRExpr rc, _) =
    let
      ctx' = ctxIncrLoc ctx 1
      (ctx'', rcInstr) = genRoutineCall ctx' rc
      in
        (ctx'', rcInstr ++ [AllocBlock 1])
  genRExpr ctx (MonadicRExpr operator rExpr, t) =
    let
      (ctx', rExprInstr) = genRExpr ctx rExpr
      operatorInstr = genMonadicOp t operator
      code = operatorInstr : rExprInstr
      in
        (ctxIncrLoc ctx' 1, code)
  genRExpr ctx (DyadicRExpr operator rExpr1 rExpr2, _) =
    let
      (ctx', rExpr1Instr) = genRExpr ctx rExpr1
      (ctx'', rExpr2Instr) = genRExpr ctx' rExpr2
      t = getCompatibleType (getRExprType rExpr1) (getRExprType rExpr2)
      operatorInstr = genOp t operator
      code = operatorInstr : rExpr2Instr ++ rExpr1Instr
      in
        (ctxIncrLoc ctx'' 1, code)
  genRExpr ctx (TypeConvRExpr expr, _) = genRExpr ctx expr

  genLiteralLoad :: Value -> Instr
  genLiteralLoad (RatioVal (num, denom)) = LoadImRatio num denom
  genLiteralLoad (IntVal val) = LoadImInt val
  genLiteralLoad (BoolVal val) = if val then LoadImInt 1 else LoadImInt 0

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
