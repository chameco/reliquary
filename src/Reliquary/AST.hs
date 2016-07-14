module Reliquary.AST where

data Term = Word String
          | Literal Integer
          | Block Term
          | Compose Term Term
          deriving Show
