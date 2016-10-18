module Reliquary.Utils.Error where

import Text.Parsec.Error

import Reliquary.Core.AST

data GenError = Mismatch CoreTerm CoreTerm
              | NotInScope
              | NotType CoreTerm 
              | NotFunction CoreTerm
              | NotPair CoreTerm 
              | NameNotInScope String
              | Redefinition String
              | SyntaxError ParseError
              deriving Show

displayError :: GenError -> String
displayError (Mismatch t t') = "Type mismatch: " ++ displayTerm t' ++ " does not have expected type " ++ displayTerm t
displayError NotInScope = "Type checker failure: de Bruijn index not in scope"
displayError (NotType t) = displayTerm t ++ " is not a type"
displayError (NotFunction t) = displayTerm t ++ " is not a function"
displayError (NotPair t) = displayTerm t ++ " is not a pair"
displayError (NameNotInScope n) = "Name " ++ n ++ " is not in scope"
displayError (Redefinition n) = "Attempt to redefine name " ++ n
displayError (SyntaxError parseError) = "Syntax error: " ++ show parseError
