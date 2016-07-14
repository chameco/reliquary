module Reliquary.Core.TypeCheck where

import Data.Set as Set

import Reliquary.Core.AST
import Reliquary.Core.DeBruijn
import Reliquary.Core.Evaluate

envLookup :: Int -> CoreEnv -> Maybe (CoreTerm, Int)
envLookup n env = if n >= l then Nothing else Just $ env !! n where
    l = length env

checkType :: CoreEnv -> CoreTerm -> Either CoreError CoreTerm
checkType env CStar = Right CStar
checkType env (CVar n) = case envLookup n env of
                            Just (t, d) -> Right $ shift (length env - d) t
                            Nothing -> Left NotInScope
checkType env (CApply e e') = do
        te <- checkType env e
        te' <- checkType env e'
        case te of CPi t t' -> if te' /= t
                       then Left $ Mismatch t te'
                       else Right $ normalize $ subst 0 e' t'
                   _ -> Left $ NotFunction te
checkType env (CLambda p p') = do
        tp <- checkType env p
        if tp == CStar
            then do
                tp' <- checkType ((p, length env):env) p'
                Right $ CPi p tp'
            else Left $ NotType tp
checkType env (CPi p p') = do
        tp <- checkType env p
        if tp == CStar
            then do
                tp' <- checkType ((normalize p, length env):env) p'
                if tp' == CStar
                    then Right CStar
                    else Left $ NotType tp'
            else Left $ NotType tp
