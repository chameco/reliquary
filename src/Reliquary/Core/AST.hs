module Reliquary.Core.AST where

import Text.Parsec

import Reliquary.AST

data Name = Integer

data CoreTerm = CStar
              | CUnitType
              | CUnit
              | CRelTermType 
              | CRelTerm Term
              | CVar Int
              | CApply CoreTerm CoreTerm
              | CLambda CoreTerm CoreTerm
              | CCons CoreTerm CoreTerm
              | CFst CoreTerm
              | CSnd CoreTerm
              | CPi CoreTerm CoreTerm
              | CSigma CoreTerm CoreTerm

type CoreEnv = [(CoreTerm, Int)]

data GenError = Mismatch CoreTerm CoreTerm
              | NotInScope
              | NotType CoreTerm 
              | NotFunction CoreTerm
              | NotPair CoreTerm 
              | NameNotInScope String
              | Redefinition String
              | SyntaxError ParseError
              | EmptyStack
              | InternalError CoreTerm

isType :: CoreTerm -> Bool
isType CStar = True
isType _ = False

matchTerm :: CoreTerm -> CoreTerm -> Bool
matchTerm CStar CStar = True
matchTerm CUnitType CUnitType = True
matchTerm CUnit CUnit = True
matchTerm CRelTermType CRelTermType = True
matchTerm (CRelTerm t) (CRelTerm t') = t == t'
matchTerm (CVar i) (CVar j) = i == j
matchTerm (CApply f t) (CApply f' t') = matchTerm f f' && matchTerm t t'
matchTerm (CLambda ty t) (CLambda ty' t') = matchTerm ty ty' && matchTerm t t'
matchTerm (CCons t1 t2) (CCons t1' t2') = matchTerm t1 t1' && matchTerm t2 t2'
matchTerm (CFst t) (CFst t') = matchTerm t t'
matchTerm (CSnd t) (CSnd t') = matchTerm t t'
matchTerm (CPi ty t) (CPi ty' t') = matchTerm ty ty' && matchTerm t t'
matchTerm (CSigma ty t) (CSigma ty' t') = matchTerm ty ty' && matchTerm t t'
matchTerm _ _ = False

displayTerm :: CoreTerm -> String
displayTerm CStar = "*"
displayTerm CUnitType = "Unit"
displayTerm CUnit = "()"
displayTerm CRelTermType = "RelTerm"
displayTerm (CRelTerm t) = show t
displayTerm (CVar i) = show i
displayTerm (CApply f t) = "(" ++ displayTerm f ++ " " ++ displayTerm t ++ ")"
displayTerm (CLambda ty t) = "λ" ++ displayTerm ty ++ "." ++ displayTerm t
displayTerm (CCons t t') = "[" ++ displayTerm t ++ "," ++ displayTerm t' ++ "]"
displayTerm (CFst t) = "fst " ++ displayTerm t
displayTerm (CSnd t) = "snd " ++ displayTerm t
displayTerm (CPi ty t) = "Π" ++ displayTerm ty ++ "." ++ displayTerm t
displayTerm (CSigma ty t) = "Σ" ++ displayTerm ty ++ "." ++ displayTerm t

displayError :: GenError -> String
displayError (Mismatch t t') = "Type mismatch: " ++ displayTerm t' ++ " is not expected type " ++ displayTerm t
displayError NotInScope = "Type checker failure: de Bruijn index not in scope"
displayError (NotType t) = displayTerm t ++ " is not a type"
displayError (NotFunction t) = displayTerm t ++ " is not a function"
displayError (NotPair t) = displayTerm t ++ " is not a pair"
displayError (NameNotInScope n) = "Name " ++ n ++ " is not in scope"
displayError (Redefinition n) = "Attempt to redefine name " ++ n
displayError (SyntaxError parseError) = "Syntax error: " ++ show parseError
displayError EmptyStack = "Empty stack"
displayError (InternalError t) = "Internal error: " ++ displayTerm t
