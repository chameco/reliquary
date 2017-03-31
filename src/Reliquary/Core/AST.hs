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

matchType :: CoreTerm -> CoreTerm -> Bool
matchType CStar CStar = True
matchType CUnitType CUnitType = True
matchType CUnit CUnit = True
matchType CBlockType CBlockType = True
matchType (CBlock ts) (CBlock ts') = ts == ts'
matchType (CVar i) (CVar j) = i == j
matchType (CApply f t) (CApply f' t') = matchType f f' && matchType t t'
matchType (CLambda ty t) (CLambda ty' t') = matchType ty ty' && matchType t t'
matchType CUnsafe{} CUnsafe{} = False
matchType (CCons t1 t2) (CCons t1' t2') = matchType t1 t1' && matchType t2 t2'
matchType (CFst t) (CFst t') = matchType t t'
matchType (CSnd t) (CSnd t') = matchType t t'
matchType (CPi ty t) (CPi ty' t') = matchType ty ty' && matchType t t'
matchType (CSigma ty t) (CSigma ty' t') = matchType ty ty' && matchType t t'
matchType _ _ = False

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
