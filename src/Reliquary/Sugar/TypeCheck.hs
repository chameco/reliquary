module Reliquary.Sugar.TypeCheck where

import Reliquary.Core.AST
import Reliquary.Core.TypeCheck
import Reliquary.Core.DeBruijn

import Reliquary.Sugar.AST
import Reliquary.Sugar.Evaluate

resugar :: CoreTerm -> SugarTerm
resugar term = go (depth term - 1) term where
    depth (CApply f t) = max (depth f) (depth t)
    depth (CLambda ty t) = max (depth ty) (depth t + 1)
    depth (CPi ty t) = max (depth ty) (depth t + 1)
    depth (CSigma t t') = max (depth t) (depth t')
    depth (CCons t t') = max (depth t) (depth t')
    depth (CFst t) = depth t
    depth (CSnd t) = depth t
    depth _ = 0

    go n CStar = SStar
    go n (CVar name) = SVar $ show name
    go n (CApply f t) = SApply (go n f) (go n t)
    go n (CLambda ty t) = SLambda (show n) (go n $ shift 1 ty) (go (n - 1) t)
    go n (CPi ty t) = SPi (Just $ show n) (go n $ shift 1 ty) (go (n - 1) t)
    go n (CSigma t t') = SSigma (go n t) (go n t')
    go n (CCons t t') = SCons (go n t) (go n t')
    go n (CFst t) = SFst $ go n t
    go n (CSnd t) = SSnd $ go n t

sugarType :: SugarEnv -> SugarTerm -> Either CoreError SugarTerm
sugarType env term = undefined
