module Reliquary.Core.AST where

import Reliquary.AST

data Name = Integer

data CoreTerm = CStar
              | CUnitType
              | CUnit
              | CBlockType
              | CBlock [Term]
              | CVar Int
              | CApply CoreTerm CoreTerm
              | CLambda CoreTerm CoreTerm
              | CUnsafe CoreTerm (CoreTerm -> CoreTerm) (CoreTerm -> CoreTerm)
              | CCons CoreTerm CoreTerm
              | CFst CoreTerm
              | CSnd CoreTerm
              | CPi CoreTerm CoreTerm
              | CSigma CoreTerm CoreTerm

type CoreEnv = [(CoreTerm, Int)]

isType :: CoreTerm -> Bool
isType CStar = True
isType _ = False

matchTerm :: CoreTerm -> CoreTerm -> Bool
matchTerm CStar CStar = True
matchTerm CUnitType CUnitType = True
matchTerm CUnit CUnit = True
matchTerm CBlockType CBlockType = True
matchTerm (CBlock ts) (CBlock ts') = ts == ts'
matchTerm (CVar i) (CVar j) = i == j
matchTerm (CApply f t) (CApply f' t') = matchTerm f f' && matchTerm t t'
matchTerm (CLambda ty t) (CLambda ty' t') = matchTerm ty ty' && matchTerm t t'
matchTerm CUnsafe{} CUnsafe{} = False
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
displayTerm CBlockType = "Block"
displayTerm (CBlock ts) = show ts
displayTerm (CVar i) = show i
displayTerm (CApply f t) = "(" ++ displayTerm f ++ " " ++ displayTerm t ++ ")"
displayTerm (CLambda ty t) = "λ" ++ displayTerm ty ++ "." ++ displayTerm t
displayTerm (CUnsafe i _ _) = "{Unsafe " ++ displayTerm i ++ "}"
displayTerm (CCons t t') = "[" ++ displayTerm t ++ "," ++ displayTerm t' ++ "]"
displayTerm (CFst t) = "fst " ++ displayTerm t
displayTerm (CSnd t) = "snd " ++ displayTerm t
displayTerm (CPi ty t) = "Π" ++ displayTerm ty ++ "." ++ displayTerm t
displayTerm (CSigma ty t) = "Σ" ++ displayTerm ty ++ "." ++ displayTerm t
