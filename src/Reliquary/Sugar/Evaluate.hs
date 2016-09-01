module Reliquary.Sugar.Evaluate where

import Data.List (elemIndex)

import Reliquary.Core.AST

import Reliquary.Sugar.AST

desugar :: SugarEnv -> SugarTerm -> Either SugarError CoreTerm
desugar _ SStar = Right CStar
desugar env (SVar n) = case elemIndex (Just n) env of
                           Just i -> Right $ CVar i
                           Nothing -> Left $ NameNotInScope n
desugar env (SApply f t) = CApply <$> desugar env f
                                  <*> desugar env t
desugar env (SLambda n ty t) =
        if Just n `elem` env
            then Left $ Redefinition n
            else CLambda <$> desugar env ty
                         <*> desugar (Just n:env) t
desugar env (SPi n@(Just name) ty t) = 
        if n `elem` env
            then Left $ Redefinition name
            else CPi <$> desugar env ty
                     <*> desugar (n:env) t
desugar env (SPi Nothing ty t) = CPi <$> desugar env ty
                                     <*> desugar (Nothing:env) t
desugar env (SSigma t t') = CSigma <$> desugar env t
                                   <*> desugar env t'
desugar env (SCons t t') = CCons <$> desugar env t
                                 <*> desugar env t'
desugar env (SFst t) = CFst <$> desugar env t
desugar env (SSnd t) = CSnd <$> desugar env t
