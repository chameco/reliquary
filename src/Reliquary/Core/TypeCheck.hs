module Reliquary.Core.TypeCheck where

import Control.Monad.Except

import Reliquary.Utils.Error

import Reliquary.Core.AST
import Reliquary.Core.DeBruijn
import Reliquary.Core.Evaluate

envLookup :: Int -> CoreEnv -> Maybe (CoreTerm, Int)
envLookup n env = if n >= l then Nothing else Just $ env !! n where
    l = length env

check :: CoreEnv -> CoreTerm -> Either GenError CoreTerm
check _ CStar = pure CStar
check _ CUnitType = pure CStar
check env CUnit = pure CUnitType
check _ CBlockType = pure CStar
check _ (CBlock _) = pure CBlockType
check env (CVar n) = case envLookup n env of
                            Just (t, d) -> pure $ shift (length env - d - 1) t
                            Nothing -> throwError NotInScope
check env (CApply e e') = do
        te <- check env e
        te' <- check env e'
        case te of
            CPi t t' -> if matchTerm te' t
                            then pure $ normalize $ subst 0 e' t'
                            else throwError $ Mismatch t te'
            _ -> throwError $ NotFunction te
check env (CLambda p p') = do
        tp <- check env p
        if isType tp
            then do
                tp' <- check ((p, length env):env) p'
                pure $ CPi p tp'
            else throwError $ NotType p
check env (CUnsafe i f _) = pure $ CPi i (CApply (CUnsafe CStar id f) (CVar 0))
check env (CCons p p') = CSigma <$> check env p <*> check env p'
check env (CFst p) = do
        tp <- check env p
        case tp of CSigma t _ -> pure t
                   _  -> throwError $ NotPair tp
check env (CSnd p) = do
        tp <- check env p
        case tp of CSigma t' t -> pure t
                   _ -> throwError $ NotPair tp
check env (CPi p p') = do
        tp <- check env p
        if isType tp
            then do
                tp' <- check ((normalize p, length env):env) p'
                if isType tp'
                    then pure CStar
                    else throwError $ NotType p'
            else throwError $ NotType p
check env (CSigma p p') = do
        tp <- check env p
        if isType tp
            then do
                tp' <- check ((normalize p, length env):env) p'
                if isType tp'
                    then pure CStar
                    else throwError $ NotType p'
            else throwError $ NotType p
