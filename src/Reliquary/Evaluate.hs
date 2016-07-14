module Reliquary.Evaluate where

import Data.Maybe (fromMaybe)

import Reliquary.Core.AST
import Reliquary.AST

translate :: Term -> Maybe CoreTerm
translate (Word s) = undefined
translate (Literal n) = Just CStar
translate (Block t) = Just CStar
translate (Compose f g) = undefined
