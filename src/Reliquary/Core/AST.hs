module Reliquary.Core.AST where

data Name = Integer

data CoreTerm = CStar
              | CUnitType
              | CVar Int
              | CApply CoreTerm CoreTerm
              | CLambda CoreTerm CoreTerm
              | CCons CoreTerm CoreTerm
              | CUnit
              | CFst CoreTerm
              | CSnd CoreTerm
              | CPi CoreTerm CoreTerm
              | CSigma CoreTerm CoreTerm
              deriving (Eq, Show)

type CoreEnv = [(CoreTerm, Int)]

displayTerm :: CoreTerm -> String
displayTerm CStar = "*"
displayTerm CUnitType = "Unit"
displayTerm (CVar i) = show i
displayTerm (CApply f t) = "(" ++ displayTerm f ++ " " ++ displayTerm t ++ ")"
displayTerm (CLambda ty t) = "λ" ++ displayTerm ty ++ "." ++ displayTerm t
displayTerm (CCons t t') = "[" ++ displayTerm t ++ "," ++ displayTerm t' ++ "]"
displayTerm CUnit = "()"
displayTerm (CFst t) = "fst " ++ displayTerm t
displayTerm (CSnd t) = "snd " ++ displayTerm t
displayTerm (CPi ty t) = "Π" ++ displayTerm ty ++ "." ++ displayTerm t
displayTerm (CSigma ty t) = "Σ" ++ displayTerm ty ++ "." ++ displayTerm t
