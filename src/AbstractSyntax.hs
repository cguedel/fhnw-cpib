module AbstractSyntax where

  import IML

  type Ident = String
  type TypedIdent = (Ident, Type)
  type Ratio = (Int, Int)

  type Program = (Command, Maybe Decl)
  type Parameter = (TypedIdent, Maybe MechMode, Maybe ChangeMode)

  data Command
    = SkipCmd
    | CpsCmd [Command]
    | AssiCmd Ident ArithExpr
    | CondCmd BoolExpr Command Command
    | WhileCmd BoolExpr Command
    | CallCmd Ident
    | DebugInCmd ArithExpr
    | DebugOutCmd ArithExpr
    deriving (Show, Eq)

  data Decl
    = CpsDecl [Decl]
    | StoDecl TypedIdent (Maybe ChangeMode)
    | FunDecl Ident [Parameter] Decl (Maybe Decl) Command
    | ProcDecl Ident [Parameter] (Maybe Decl) Command
    deriving (Show, Eq)

  data ArithExpr
    = LitAExpr Int
    | IdAExpr Ident
    | DyaAExpr ArithOperator ArithExpr ArithExpr
    deriving (Show, Eq)

  data BoolExpr
    = LitBExpr Bool
    | RelBExpr RelOperator ArithExpr ArithExpr
    | NegBExpr BoolExpr
    | DyaBExpr BoolOperator BoolExpr BoolExpr
    deriving (Eq, Show)
