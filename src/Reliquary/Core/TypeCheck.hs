module Reliquary.Core.TypeCheck where

import Control.Monad.Except

import Reliquary.Core.AST
import Reliquary.Core.DeBruijn
import Reliquary.Core.Evaluate
import Reliquary.Core.Utils

import Debug.Trace

envLookup :: Int -> CoreEnv -> Maybe (CoreTerm, Int)
envLookup n env = if n >= l then Nothing else Just $ env !! n where
    l = length env

-- Given an environment of type associations and a term, return the type of
-- that term (or throw a type error)
check :: CoreEnv -> CoreTerm -> Either GenError CoreTerm
check _ CStar = return CStar
check _ CUnitType = return CStar
check env CUnit = return CUnitType
check _ CRelTermType = return CStar
check _ (CRelTerm _) = return CRelTermType
check env (CVar n) = case envLookup n env of
                            Just (t, d) -> return $ shift (length env - d) t
                            Nothing -> throwError NotInScope
check env (CApply e e') = do
        te <- check env e
        te' <- check env e'
        case te of
            CPi t t' -> if matchTerm te' t
                            then return $ normalize $ subst 0 e' t'
                            else throwError $ Mismatch t te'
            _ -> throwError $ NotFunction te
check env (CLambda p p') = do
        tp <- check env p
        if isType tp
            then do
                tp' <- check ((p, length env):env) p'
                return $ CPi p tp'
            else throwError $ NotType p
check env (CCons p p') = CSigma <$> check env p <*> check env p'
check env (CFst p) = do
        tp <- check env p
        case tp of CSigma t _ -> return t
                   _  -> throwError $ NotPair tp
check env (CSnd p) = do
        tp <- check env p
        case tp of CSigma _ t -> return t
                   _ -> throwError $ NotPair tp
check env (CPi p p') = do
        tp <- check env p
        if isType tp
            then do
                tp' <- check ((normalize p, length env):env) p'
                if isType tp'
                    then return CStar
                    else throwError $ NotType p'
            else throwError $ NotType p
check env (CSigma p p') = do
        tp <- check env p
        if isType tp
            then do
                tp' <- check ((normalize p, length env):env) p'
                if isType tp'
                    then return CStar
                    else throwError $ NotType tp'
            else throwError $ NotType p
