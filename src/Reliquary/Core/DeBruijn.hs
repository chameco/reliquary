module Reliquary.Core.DeBruijn where

import Reliquary.Core.AST

shift :: Int -> CoreTerm -> CoreTerm
shift n = shiftInt 0 where
    shiftInt _ CStar = CStar
    shiftInt l (CVar n') = CVar $ if n' >= l then n + n' else n'
    shiftInt l (CApply f t) = CApply (shiftInt l f) (shiftInt l t)
    shiftInt l (CLambda tt t) = CLambda (shiftInt l tt) (shiftInt (l + 1) t)
    shiftInt l (CPi tt t) = CPi (shiftInt l tt) (shiftInt (l + 1) t)

subst :: Int -> CoreTerm -> CoreTerm -> CoreTerm
subst _ _ CStar = CStar
subst n new old@(CVar i) = if i == n then new else old
subst n new (CApply f t) = CApply (subst n new f) (subst n new t)
subst n new (CLambda tt t) = CLambda (subst n new tt) (subst (n + 1) (shift 1 new) t)
subst n new (CPi tt t) = CPi (subst n new tt) (subst (n + 1) (shift 1 new) t)
