module Reliquary.Evaluate where

import Reliquary.Sugar.AST
import Reliquary.Sugar.Scope

import Reliquary.AST
import Reliquary.Dictionary

translate :: Term -> Maybe SugarTerm
translate = build 0 where
    build _ (Word s) = Just $ SVar s
    build _ (Literal n) = Just $ SLambda "a" SStar $ SLambda "f" (SPi Nothing (SVar "a") (SVar "a")) $ SLambda "x" (SVar "a") $ iterate (SApply (SVar "f")) (SVar "x") !! n
    build d (Block ts) = undefined

compile :: Dictionary -> Maybe Scope
compile (Entry n tt t:xs) = do
        stt <- translate tt
        st <- translate t
        Just ((n, stt, st):) <*> compile xs
compile []  = Just []
