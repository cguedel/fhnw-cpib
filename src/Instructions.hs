module Instructions where

  type CommentedInstr = (Instr, Maybe String)

  data Instr
    = AddInt
    | AddRatio
    | AllocBlock Int
    | AllocStack Int
    | Call Int
    | CeilRatio
    | CondJump Int
    | DenomRatio
    | Deref
    | DivTruncInt
    | DivRatio
    | Dup
    | EqInt
    | EqRatio
    | FloorRatio
    | GeInt
    | GeRatio
    | GtInt
    | GtRatio
    | InputBool String
    | InputInt String
    | InputRatio String
    | LeInt
    | LeRatio
    | LoadAddrRel Int
    | LoadImInt Int
    | LoadImRatio Int Int
    | LtInt
    | LtRatio
    | ModTruncInt
    | MultInt
    | MultRatio
    | NegBool
    | NegInt
    | NegRatio
    | NeInt
    | NeRatio
    | NumRatio
    | OutputBool String
    | OutputInt String
    | OutputRatio String
    | Return Int
    | RoundRatio
    | Stop
    | Store
    | SubInt
    | SubRatio
    | UncondJump Int

  instance Show Instr where
    show AddInt = "AddInt"
    show AddRatio = "AddRatio"
    show (AllocBlock i) = "AllocBlock(" ++ show i ++ ")"
    show (AllocStack i) = "AllocStack(" ++ show i ++ ")"
    show (Call i) = "Call(" ++ show i ++ ")"
    show CeilRatio = "CeilRatio"
    show (CondJump i) = "CondJump(" ++ show i ++ ")"
    show DenomRatio = "DenomRatio"
    show Deref = "Deref"
    show DivTruncInt = "DivTruncInt"
    show DivRatio = "DivRatio"
    show Dup = "Dup"
    show EqInt = "EqInt"
    show EqRatio = "EqRatio"
    show FloorRatio = "FloorRatio"
    show GeInt = "GeInt"
    show GeRatio = "GeRatio"
    show GtInt = "GtInt"
    show GtRatio = "GtRatio"
    show (InputBool s) = "InputBool(" ++ s ++ ")"
    show (InputInt s) = "InputInt(" ++ s ++ ")"
    show (InputRatio s) = "InputRatio(" ++ s ++ ")"
    show LeInt = "LeInt"
    show LeRatio = "LeRatio"
    show (LoadAddrRel i) = "LoadAddrRel(" ++ show i ++ ")"
    show (LoadImInt i) = "LoadImInt(" ++ show i ++ ")"
    show (LoadImRatio i j) = "LoadImRatio(" ++ show i ++ "/" ++ show j ++ ")"
    show LtInt = "LtInt"
    show LtRatio = "LtRatio"
    show ModTruncInt = "ModTruncInt"
    show MultInt = "MultInt"
    show MultRatio = "MultRatio"
    show NegBool = "NegBool"
    show NegInt = "NegInt"
    show NegRatio = "NegRatio"
    show NeInt = "NeInt"
    show NeRatio = "NeRatio"
    show NumRatio = "NumRatio"
    show (OutputBool s) = "OutputBool(" ++ s ++ ")"
    show (OutputInt s) = "OutputInt(" ++ s ++ ")"
    show (OutputRatio s) = "OutputRatio(" ++ s ++ ")"
    show (Return i) = "Return(" ++ show i ++ ")"
    show RoundRatio = "RoundRatio"
    show Stop = "Stop"
    show Store = "Store"
    show SubInt = "SubInt"
    show SubRatio = "SubRatio"
    show (UncondJump i) = "UncondJump(" ++ show i ++ ")"
