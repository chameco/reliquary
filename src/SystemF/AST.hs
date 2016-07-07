module SystemF.AST where

import SystemF.Type

data Term = TermTrue
          | TermFalse
          | TermIf Term Term Term
          | TermZero
          | TermSucc Term
          | TermPred Term
          | TermIsZero Term
          | TermVar String
          | TermApply Term Term
          | TermLambda String Type Term
          | TermTypeApply Term Type
          | TermTypeLambda String Kind Term
          deriving Show
