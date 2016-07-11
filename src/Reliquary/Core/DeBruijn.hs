module Reliquary.Core.DeBruijn where

import Reliquary.Core.AST

shift :: Int -> Term -> Term
shift n = shiftInt 0 where
    shiftInt _ TermStar = TermStar
    shiftInt l (TermVar n') = TermVar $ if n' >= l then n + n' else n'
    shiftInt l (TermApply f t) = TermApply (shiftInt l f) (shiftInt l t)
    shiftInt l (TermLambda tt t) = TermLambda (shiftInt l tt) (shiftInt (l + 1) t)
    shiftInt l (TermPi tt t) = TermPi (shiftInt l tt) (shiftInt (l + 1) t)

subst :: Int -> Term -> Term -> Term
subst _ _ TermStar = TermStar
subst n new old@(TermVar i) = if i == n then new else old
subst n new (TermApply f t) = TermApply (subst n new f) (subst n new t)
subst n new (TermLambda tt t) = TermLambda (subst (n + 1) (shift 1 new) tt) (subst (n + 1) (shift 1 new) t)
subst n new (TermPi tt t) = TermPi (subst (n + 1) (shift 1 new) tt) (subst (n + 1) (shift 1 new) t)
