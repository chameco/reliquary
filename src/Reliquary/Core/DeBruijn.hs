module Reliquary.Core.DeBruijn where

import Reliquary.Core.AST

shift :: Int -> CoreTerm -> CoreTerm
shift n = shiftInt 0 where
    shiftInt _ CStar = CStar
    shiftInt _ CUnitType = CUnitType
    shiftInt _ CUnit = CUnit
    shiftInt _ CBlockType = CBlockType
    shiftInt _ b@(CBlock _) = b
    shiftInt l (CVar n') = CVar $ if n' >= l then n + n' else n'
    shiftInt l (CApply f t) = CApply (shiftInt l f) (shiftInt l t)
    shiftInt l (CLambda tt t) = CLambda (shiftInt l tt) (shiftInt (l + 1) t)
    shiftInt l (CCons t t') = CCons (shiftInt l t) (shiftInt l t')
    shiftInt l (CFst t) = CFst (shiftInt l t)
    shiftInt l (CSnd t) = CSnd (shiftInt l t)
    shiftInt l (CPi tt t) = CPi (shiftInt l tt) (shiftInt (l + 1) t)
    shiftInt l (CSigma t t') = CSigma (shiftInt l t) (shiftInt (l + 1) t')

subst :: Int -> CoreTerm -> CoreTerm -> CoreTerm
subst _ _ CStar = CStar
subst _ _ CUnitType = CUnitType
subst _ _ CUnit = CUnit
subst _ _ CBlockType = CBlockType
subst _ _ b@(CBlock _) = b
subst n new old@(CVar i) = if i == n then new else old
subst n new (CApply f t) = CApply (subst n new f) (subst n new t)
subst n new (CLambda tt t) = CLambda (subst n new tt) (subst (n + 1) (shift 1 new) t)
subst n new (CCons tt t) = CCons (subst n new tt) (subst n new t)
subst n new (CFst t) = CFst (subst n new t)
subst n new (CSnd t) = CSnd (subst n new t)
subst n new (CPi tt t) = CPi (subst n new tt) (subst (n + 1) (shift 1 new) t)
subst n new (CSigma tt t) = CSigma (subst n new tt) (subst (n + 1) (shift 1 new) t)
