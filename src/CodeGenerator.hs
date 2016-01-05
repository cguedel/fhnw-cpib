module CodeGenerator (genCode) where

  import IML
  import StaticAnalysis
  import Instructions
  import AbstractSyntax

  import qualified Data.Map as M

  -- Exported functions
  genCode :: AnalyzedProgram -> (Context, [Instr])
  genCode (ctx, TypedCpsCmd cs, decl) =
    let
      (globalCtx, declInstr) = genDecl ctx decl
      (ctx', cmdInstr) = genCpsCmd globalCtx cs
      in
        (ctx', reverse $ Stop : cmdInstr ++ declInstr)
  genCode (_, _, _) = error "Internal error, expected TypedCpsCmd"

  -- Context stuff
  ctxIncrLoc :: Context -> Int -> Context
  ctxIncrLoc Context { ctxLoc = l, ctxRoutines = r, ctxVars = v, ctxIsGlobal = g } incr =
    Context { ctxLoc = l + incr, ctxRoutines = r, ctxVars = v, ctxIsGlobal = g }

  ctxGetLoc :: Context -> Int
  ctxGetLoc Context { ctxLoc = l } = l

  ctxGetVar :: Context -> Ident -> Variable
  ctxGetVar Context { ctxVars = v } ident = v M.! ident

  ctxIsVarGlobal :: Variable -> Bool
  ctxIsVarGlobal Variable { varIsGlobal = g } = g

  ctxGetVarAddr :: Variable -> Int
  ctxGetVarAddr Variable {varAddr = a, varIsGlobal = g }
    | g = a
    | otherwise = a + 3

  ctxSetVarAddr :: Context -> Ident -> Int -> Context
  ctxSetVarAddr ctx ident addr =
    let
      Variable { varIsGlobal = g, varType = t, varChangeMode = cm } = ctxGetVar ctx ident
      variable' = Variable { varIsGlobal = g, varType = t, varChangeMode = cm, varAddr = addr }
      in
        ctxReplaceVar ctx ident variable'

  ctxReplaceVar :: Context -> Ident -> Variable -> Context
  ctxReplaceVar Context { ctxLoc = l, ctxRoutines = r, ctxVars = v, ctxIsGlobal = g } ident var =
    Context { ctxLoc = l, ctxRoutines = r, ctxVars = M.insert ident var v, ctxIsGlobal = g }

  ctxGetRoutineCtx :: Context -> Ident -> Context
  ctxGetRoutineCtx Context { ctxRoutines = r, ctxLoc = l } ident =
    let
      Routine { routCtx = Context { ctxVars = v } } = r M.! ident
      in
        Context { ctxLoc = l, ctxRoutines = r, ctxVars = v, ctxIsGlobal = False }

  ctxSetRoutineCtx :: Context -> Ident -> Context -> Context
  ctxSetRoutineCtx Context { ctxLoc = l, ctxRoutines = r, ctxVars = v, ctxIsGlobal = g } ident ctx =
    let
      Routine { routType = t, routParams = p } = r M.! ident
      routine' = Routine { routAddr = l + 1, routType = t, routParams = p, routCtx = ctx }
      ctx' = Context { ctxLoc = l, ctxRoutines = M.insert ident routine' r, ctxVars = v, ctxIsGlobal = g }
      in
        ctx'

  -- Declarations
  genDecl :: Context -> TypedDecl -> (Context, [Instr])
  genDecl ctx (TypedCpsDecl cs) = genCpsDecl ctx cs
  genDecl ctx (TypedStoDecl ident)
    | isGlobal = genGlobalVar variable ctx
    | otherwise = genLocalVar COPY variable ctx
    where
      variable = ctxGetVar ctx ident
      isGlobal = ctxIsVarGlobal variable
  genDecl ctx (TypedFunDecl ident params returns locals body) = (ctx, [])
  genDecl ctx (TypedProcDecl ident params locals body) =
    let
      ctx' = ctxGetRoutineCtx ctx ident
      (ctx'', localsInstr) = genLocals ctx' locals
      (ctx''', paramsInstr) = genParams ctx'' (zip [0..] params)
      (_, body') = genCmd ctx''' body
      code = body' ++ localsInstr ++ paramsInstr
      jump = UncondJump (ctxGetLoc ctx + length code + 1)
      in
        addInstrs (ctxSetRoutineCtx ctx ident ctx''') [] (code ++ [jump])

  genCpsDecl :: Context -> [TypedDecl] -> (Context, [Instr])
  genCpsDecl ctx [] = (ctx, [])
  genCpsDecl ctx (d : ds) =
    let
      (ctx', instr) = genDecl ctx d
      (ctx'', instr') = genCpsDecl ctx' ds
      in
        (ctx'', instr' ++ instr)

  -- Parameters & locals
  genParams :: Context -> [(Int, Param)] -> (Context, [Instr])
  genParams ctx [] = (ctx, [])
  genParams ctx (p : ps) =
    let
      (idx, p') = p
      (ctx', i) = genParam ctx idx p'
      (ctx'', is) = genParams ctx' ps
      in
        (ctx'', is ++ i)

  genParam :: Context -> Int -> Param -> (Context, [Instr])
  genParam ctx idx Param { parIdent = ident, parType = t, parMechMode = mm, parChangeMode = cm } =
    let
      ctx' = ctxAddVariable ctx (ident, t, cm)
      variable = ctxGetVar ctx' ident
      addr = ctxGetVarAddr variable
      addr' = getParamVarAddr addr idx mm
      ctx'' = ctxSetVarAddr ctx' ident addr'
      variable' = ctxGetVar ctx'' ident
      (ctx''', instr) = genLocalVar mm variable' ctx''
      load = genLoadParam idx mm variable'
      in
        addInstrs ctx''' [] (load ++ instr)

  genLoadParam :: Int -> MechMode -> Variable -> [Instr]
  genLoadParam _ REF _ = []
  genLoadParam idx COPY var =
    let
      argAddr = getParamVarAddr 0 idx REF
      paramAddr = ctxGetVarAddr var
      loadParam = LoadAddrRel paramAddr
      loadArg = LoadAddrRel argAddr
      in
        [Store, Deref, loadArg, loadParam]

  getParamVarAddr :: Int -> Int -> MechMode -> Int
  getParamVarAddr addr _ COPY = addr - 3
  getParamVarAddr _ idx REF = (-4) - idx

  genLocals :: Context -> TypedDecl -> (Context, [Instr])
  genLocals ctx (TypedCpsDecl cs) = genCpsLocal ctx cs
  genLocals ctx (TypedStoDecl ident) =
    let
      variable = ctxGetVar ctx ident
      (ctx', instr) = genLocalVar COPY variable ctx
      in
        addInstrs ctx' [] instr
  genLocals _ decl = error $ "Internal error: Expected TypedStoDecl or TypedCpsDecl, but got " ++ show decl

  genCpsLocal :: Context -> [TypedDecl] -> (Context, [Instr])
  genCpsLocal ctx [] = (ctx, [])
  genCpsLocal ctx (d : ds) =
    let
      (ctx', instr) = genLocals ctx d
      (ctx'', instr') = genCpsLocal ctx' ds
      in
        (ctx'', instr' ++ instr)

  -- Variables
  genGlobalVar :: Variable -> Context -> (Context, [Instr])
  genGlobalVar var ctx =
    let
      initialize = genVarInit var
      instr = [Store, initialize, LoadImInt (ctxGetVarAddr var), AllocBlock 1]
      in
        (ctx, instr)

  genLocalVar :: MechMode -> Variable -> Context -> (Context, [Instr])
  genLocalVar REF _ ctx = (ctx, [])
  genLocalVar _ var ctx =
    let
      initialize = genVarInit var
      instr = [Store, initialize, LoadAddrRel (ctxGetVarAddr var), AllocBlock 1]
      in
        (ctx, instr)

  genVarInit :: Variable -> Instr
  genVarInit Variable { varType = t }
    | t == RatioType = LoadImRatio 0 1
    | otherwise = LoadImInt 0

  -- Instructions
  addInstr :: Context -> [Instr] -> Instr -> (Context, [Instr])
  addInstr ctx instrs instr = addInstrs ctx instrs [instr]

  addInstrs :: Context -> [Instr] -> [Instr] -> (Context, [Instr])
  addInstrs ctx instrs instrs' =
    let
      ctx' = ctxIncrLoc ctx (length instrs')
      in
        (ctx', instrs' ++ instrs)

  -- Commands
  genCmd :: Context -> TypedCommand -> (Context, [Instr])
  genCmd ctx (TypedCpsCmd cs) = genCpsCmd ctx cs
  genCmd ctx TypedSkipCmd = (ctx, [])

  genCpsCmd :: Context -> [TypedCommand] -> (Context, [Instr])
  genCpsCmd ctx [] = (ctx, [])
  genCpsCmd ctx (c : cs) =
    let
      (ctx', instr) = genCmd ctx c
      (ctx'', instr') = genCpsCmd ctx' cs
      in
        (ctx'', instr ++ instr')
