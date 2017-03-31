module Reliquary.AST where

data Term = Word String
          | Block [Term]
          deriving (Eq, Show)
