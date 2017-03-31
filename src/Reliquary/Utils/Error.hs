module Reliquary.Utils.Error where

import Text.Parsec.Error

import Reliquary.Core.AST

data GenError = Mismatch CoreTerm CoreTerm
              | NotInScope
              | NotType CoreTerm 
              | NotType2 CoreTerm CoreTerm 
              | NotType3 CoreTerm CoreTerm
              | NotFunction CoreTerm
              | NotPair CoreTerm 
              | NameNotInScope String
              | Redefinition String
              | SyntaxError ParseError

displayError :: GenError -> String
displayError (Mismatch t t') = "Type mismatch: " ++ displayTerm t' ++ " is not expected type " ++ displayTerm t
displayError NotInScope = "Type checker failure: de Bruijn index not in scope"
displayError (NotType t) = displayTerm t ++ " is not a type"
displayError (NotType2 t t') = displayTerm t ++ " (" ++ displayTerm t' ++ ") is super not a type"
displayError (NotType3 t t') = displayTerm t ++ " (" ++ displayTerm t' ++ ") is super duper not a type"
displayError (NotFunction t) = displayTerm t ++ " is not a function"
displayError (NotPair t) = displayTerm t ++ " is not a pair"
displayError (NameNotInScope n) = "Name " ++ n ++ " is not in scope"
displayError (Redefinition n) = "Attempt to redefine name " ++ n
displayError (SyntaxError parseError) = "Syntax error: " ++ show parseError
