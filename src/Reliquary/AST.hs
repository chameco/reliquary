module Reliquary.AST where

data Term = Word String
          | Literal Int
          | Block [Term]
          | ListType [Term]
          deriving Show
