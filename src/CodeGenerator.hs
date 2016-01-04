module CodeGenerator (genCode) where

  import IML
  import StaticAnalysis
  import Instructions
  import AbstractSyntax

  import qualified Data.Map as M

  -- Exported functions
  genCode :: AnalyzedProgram -> [Instr]
  genCode (ctx, TypedCpsCmd cs, decl) =
    let
      (globalCtx, declInstr) = genDecl ctx decl
      (_, cmdInstr) = genCpsCmd globalCtx cs
      in
        reverse $ Stop : cmdInstr ++ declInstr
  genCode (_, _, _) = error "Internal error, expected TypedCpsCmd"

  -- Context stuff
  ctxGetVar :: Context -> Ident -> Variable
  ctxGetVar Context { ctxVars = v } ident = v M.! ident

  ctxIsVarGlobal :: Variable -> Bool
  ctxIsVarGlobal Variable { varIsGlobal = g } = g

  ctxGetVarAddr :: Variable -> Int
  ctxGetVarAddr Variable {varAddr = a } = a

  -- Declarations
  genDecl :: Context -> TypedDecl -> (Context, [Instr])
  genDecl ctx (TypedCpsDecl cs) = genCpsDecl ctx cs
  genDecl ctx (TypedStoDecl ident)
    | isGlobal = genGlobalVar variable ctx
    | otherwise = genLocalVar variable ctx
    where
      variable = ctxGetVar ctx ident
      isGlobal = ctxIsVarGlobal variable
  genDecl ctx (TypedFunDecl ident params returns locals body) = (ctx, [])
  genDecl ctx (TypedProcDecl ident params locals body) = (ctx, [])

  genCpsDecl :: Context -> [TypedDecl] -> (Context, [Instr])
  genCpsDecl ctx [] = (ctx, [])
  genCpsDecl ctx (d : ds) =
    let
      (ctx', instr) = genDecl ctx d
      (ctx'', instr') = genCpsDecl ctx ds
      in
        (ctx'', instr' ++ instr)

  -- Variables
  genGlobalVar :: Variable -> Context -> (Context, [Instr])
  genGlobalVar var ctx =
    let
      initialize = genVarInit var
      instr = [Store, initialize, LoadImInt (ctxGetVarAddr var)]
      in (ctx, instr)

  genLocalVar :: Variable -> Context -> (Context, [Instr])
  genLocalVar = undefined

  genVarInit :: Variable -> Instr
  genVarInit Variable { varType = t }
    | t == RatioType = LoadImRatio 0 1
    | otherwise = LoadImInt 0

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
