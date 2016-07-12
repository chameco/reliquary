module Reliquary.Core.TypeCheck where

import Data.Set as Set

import Reliquary.Core.AST
import Reliquary.Core.DeBruijn
import Reliquary.Core.Evaluate

envLookup :: Int -> Env -> Maybe (Term, Int)
envLookup n env = if n >= l then Nothing else Just $ env !! n where
    l = length env

checkType :: Env -> Term -> Either TypeError Term
checkType env TermStar = Right TermStar
checkType env (TermVar n) = case envLookup n env of
                                   Just (t, d) -> Right $ shift (length env - d) t
                                   Nothing -> Left NotInScope
checkType env (TermApply e e') = do
        te <- checkType env e
        te' <- checkType env e'
        case te of TermPi t t' -> if te' /= t
                       then Left $ Mismatch t te'
                       else Right $ normalize $ subst 0 e' t'
                   _ -> Left $ NotFunction te
checkType env (TermLambda p p') = do
        tp <- checkType env p
        if tp == TermStar
            then do
                tp' <- checkType ((p, length env):env) p'
                Right $ TermPi p tp'
            else Left $ NotType tp

checkType env (TermPi p p') = do
        tp <- checkType env p
        if tp == TermStar
            then do
                tp' <- checkType ((normalize p, length env):env) p'
                if tp' == TermStar
                    then Right TermStar
                    else Left $ NotType tp'
            else Left $ NotType tp
