module Model where

data Expr
        = Program [Expr]
        | Word String
        | Literal Integer
        | Block [Expr]
        | Definition String Expr
        deriving Show
