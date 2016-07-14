module Reliquary.Core.AST where

data Name = Integer

data CoreTerm = CStar
              | CVar Int
              | CApply CoreTerm CoreTerm
              | CLambda CoreTerm CoreTerm
              | CPi CoreTerm CoreTerm
              deriving (Show, Eq)

type CoreEnv = [(CoreTerm, Int)]

data CoreError = Mismatch CoreTerm CoreTerm
               | NotInScope
               | NotFunction CoreTerm
               | NotType CoreTerm 
               deriving Show
