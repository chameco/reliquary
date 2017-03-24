module Reliquary.AST where

import Reliquary.Core.AST

data Term = Word String
          | Literal Int
          | Block [Term]
          deriving Show

type Type = ([CoreTerm], [CoreTerm])
