module Reliquary.Core.TypeCheck where

import Data.Set as Set

import Debug.Trace

import Reliquary.Core.AST
import Reliquary.Core.DeBruijn
import Reliquary.Core.Evaluate

envLookup :: Int -> Env -> Maybe Term
envLookup n env = if n >= l then Nothing else Just $ env !! n where
    l = length env

checkType :: Env -> Term -> Either TypeError Term
checkType env TermStar = Right TermStar
checkType env (TermVar n) = case envLookup n env of
                                   Just t -> Right t
                                   Nothing -> Left NotInScope
checkType env (TermApply e e') = do
        te <- checkType env e
        te' <- checkType env e'
        case te of TermPi t t' -> if te' /= t
                   then Left $ Mismatch t te'
                   else Right $ normalize $ subst 0 e' t'
                   _ -> Left $ NotFunction te
checkType env all@(TermLambda ntype t) = do
        tt <- checkType (ntype:env) t
        Right $ TermPi ntype tt

checkType env (TermPi p p') = do
        tp <- checkType env p
        tp' <- checkType (normalize p:env) p'
        if tp == TermStar && tp' == TermStar
            then Right TermStar
            else Left $ NotKindStar tp tp' 
