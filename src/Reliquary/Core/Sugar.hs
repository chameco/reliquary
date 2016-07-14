module Reliquary.Core.Sugar where

import Data.List (elemIndex)

import Reliquary.Core.AST

data SugarTerm = SStar
               | SVar String
               | SApply SugarTerm SugarTerm
               | SLambda String SugarTerm SugarTerm
               | SPi String SugarTerm SugarTerm
               deriving (Show, Eq)

type SugarEnv = [String]

data SugarError = NameNotInScope
                | Redefinition
                deriving Show

desugar :: SugarEnv -> SugarTerm -> Either SugarError CoreTerm
desugar _ SStar = Right CStar
desugar env (SVar n) = case elemIndex n env of
                           Just i -> Right $ CVar i
                           Nothing -> Left NameNotInScope
desugar env (SApply f t) = Right CApply
                           <*> desugar env f
                           <*> desugar env t
desugar env (SLambda n ty t) =
        if n `elem` env
            then Left Redefinition
            else Right CLambda
                 <*> desugar env ty
                 <*> desugar (n:env) t
desugar env (SPi n ty t) = 
        if n `elem` env
            then Left Redefinition
            else Right CPi
                 <*> desugar env ty
                 <*> desugar (n:env) t
