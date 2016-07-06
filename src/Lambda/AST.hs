module Lambda.AST where

data Term = TermTrue
          | TermFalse
          | TermIf Term Term Term
          | TermZero
          | TermSucc Term
          | TermPred Term
          | TermIsZero Term
          | TermVar String
          | TermApply Term Term
          | TermLambda String Term
          deriving Show
