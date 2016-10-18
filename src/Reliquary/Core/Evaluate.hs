module Reliquary.Core.Evaluate where

import Data.Maybe

import Reliquary.Utils.Monad

import Reliquary.Core.AST
import Reliquary.Core.DeBruijn

normalize :: CoreTerm -> Compiler CoreTerm
normalize t = rec $ normalize1 t where
    rec (Just t') = normalize t'
    rec Nothing = return t

    both :: (a -> a -> a) -> (a -> Maybe a) -> a -> a -> Maybe a
    both f g x y = go (g x) (g y) where
        go (Just a) (Just b) = Just $ f a b
        go Nothing (Just b) = Just $ f x b
        go (Just a) Nothing = Just $ f a y
        go Nothing Nothing = Nothing

    normalize1 :: CoreTerm -> Maybe CoreTerm
    normalize1 (CApply (CLambda ntype body) t) = Just $ shift (-1) $ subst 0 t body
    normalize1 (CApply f t) = CApply <$> normalize1 f <*> pure t
    normalize1 (CFst (CCons t _)) = Just t
    normalize1 (CSnd (CCons _ t)) = Just t
    normalize1 (CFst p) = normalize1 p >>= Just . CFst
    normalize1 (CSnd p) = normalize1 p >>= Just . CSnd
    normalize1 (CCons p p') = both CCons normalize1 p p' 
    normalize1 (CPi p p') = both CPi normalize1 p p'
    normalize1 (CSigma p p') = both CSigma normalize1 p p'
    normalize1 _ = Nothing
