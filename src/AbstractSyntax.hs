module AbstractSyntax where

  import IML

  type Ident = String
  type TypedIdent = (Ident, Type)
  type Ratio = (Int, Int)

  type Program = (Command, Maybe Decl)
  type Parameter = (TypedIdent, Maybe MechMode, Maybe ChangeMode)

  type RoutineCall = (Ident, [Expr])

  data Value
    = BoolVal Bool
    | IntVal Int
    | RatioVal Ratio
    deriving (Show, Eq)

  data IsInitialization
    = Initialization
    | NoInitialization
    deriving (Show, Eq)

  data Operator
    = Not
    | Times
    | Div
    | Mod
    | Plus
    | Minus
    | Less
    | GreaterEq
    | Equal
    | NotEq
    | Greater
    | LessEq
    | CAnd
    | Cor
    | Denom
    | Num
    | Floor
    | Ceil
    | Round
    deriving (Show, Eq)

  data Command
    = SkipCmd
    | CpsCmd [Command]
    | AssiCmd Expr Expr
    | CondCmd Expr Command Command
    | WhileCmd Expr Command
    | ProcCallCmd RoutineCall
    | DebugInCmd Expr
    | DebugOutCmd Expr
    deriving (Show, Eq)

  data Decl
    = CpsDecl [Decl]
    | StoDecl TypedIdent (Maybe ChangeMode)
    | FunDecl Ident [Parameter] Decl (Maybe Decl) Command
    | ProcDecl Ident [Parameter] (Maybe Decl) Command
    deriving (Show, Eq)

  data Expr
    = LiteralExpr Value
    | StoreExpr Ident IsInitialization
    | FunCallExpr RoutineCall
    | MonadicExpr Operator Expr
    | DyadicExpr Operator Expr Expr
    deriving (Show, Eq)