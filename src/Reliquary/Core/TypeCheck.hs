module Reliquary.Core.TypeCheck where

import Control.Monad.Except

import Reliquary.Utils.Monad

import Reliquary.Core.AST
import Reliquary.Core.DeBruijn
import Reliquary.Core.Evaluate

import Debug.Trace

envLookup :: Int -> CoreEnv -> Maybe (CoreTerm, Int)
envLookup n env = if n >= l then Nothing else Just $ env !! n where
    l = length env

checkType :: CoreEnv -> CoreTerm -> Compiler CoreTerm
checkType env CStar = return CStar
checkType env CUnitType = return CStar
checkType env (CVar n) = case envLookup n env of
                            Just (t, d) -> return $ shift (length env - d) t
                            Nothing -> throwError NotInScope
checkType env (CApply e e') = do
        te <- checkType env e
        te' <- checkType env e'
        case te of CPi t t' -> if te' /= t
                       then throwError $ Mismatch t te'
                       else return $ subst 0 e' t'
                   _ -> throwError $ NotFunction te
checkType env (CLambda p p') = do
        tp <- checkType env p
        if tp == CStar
            then do
                tp' <- checkType ((p, length env):env) p'
                return $ CPi p tp'
            else throwError $ NotType tp
checkType env (CCons p p') = CSigma <$> checkType env p <*> checkType env p'
checkType env CUnit = return CUnitType
checkType env (CFst p) = do
        tp <- checkType env p
        case tp of CSigma t _ -> return t
                   _  -> throwError $ NotPair tp
checkType env (CSnd p) = do
        tp <- checkType env p
        case tp of CSigma t' t -> return t
                   _ -> throwError $ NotPair tp
checkType env (CPi p p') = do
        tp <- checkType env p
        if tp == CStar
            then do
                normalized <- normalize p
                tp' <- checkType ((normalized, length env):env) p'
                if tp' == CStar
                    then return CStar
                    else throwError $ NotType tp'
            else throwError $ NotType tp
checkType env (CSigma p p') = do
        tp <- checkType env p
        if tp == CStar
            then do
                tp' <- checkType env p'
                if tp' == CStar
                    then return CStar
                    else throwError $ NotType tp'
            else throwError $ NotType tp
