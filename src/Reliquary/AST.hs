module Reliquary.AST where

data Term = Star
          | UnitType
          | Unit
          | Definition String Term Term Term
          | Word String
          | Literal Int
          | Block [Term]
          | ListType [Term]
          deriving Show
