module Reliquary.AST where

data Term = Word String
          | Literal Integer
          | Block [Term]
          | Quasi [Term]
          deriving Show

data Definition = Definition String Term
                deriving Show
