module IML.Value where

  data Value = BoolVal Bool | Int32Val Int | RationalVal Int Int
    deriving (Show, Eq)
