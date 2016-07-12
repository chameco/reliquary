module Reliquary.Core.AST where

data Name = Integer

data Term = TermStar
          | TermVar Int
          | TermApply Term Term
          | TermLambda Term Term
          | TermPi Term Term
          deriving (Show, Eq)

type Env = [(Term, Int)]

data TypeError = Mismatch Term Term
               | NotInScope
               | NotFunction Term
               | NotType Term 
               deriving Show
