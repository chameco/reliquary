module Reliquary.Core.Evaluate where

import Data.Maybe

import Reliquary.Core.AST
import Reliquary.Core.DeBruijn

normalize :: CoreTerm -> CoreTerm
normalize t = rec $ go t where
    rec (Just t') = normalize t'
    rec Nothing = t

    both :: (a -> a -> a) -> (a -> Maybe a) -> a -> a -> Maybe a
    both f g x y = go (g x) (g y) where
        go (Just a) (Just b) = Just $ f a b
        go Nothing (Just b) = Just $ f x b
        go (Just a) Nothing = Just $ f a y
        go Nothing Nothing = Nothing

    go :: CoreTerm -> Maybe CoreTerm
    go (CApply (CLambda ntype body) t) = Just $ shift (-1) $ subst 0 t body
    go (CApply (CUnsafe i _ f) t)  = case f t of Right t -> Just t; Left e -> error $ displayError e
    go (CApply f t) = CApply <$> go f <*> return t
    go (CFst (CCons t _)) = Just t
    go (CSnd (CCons _ t)) = Just t
    go (CFst p) = go p >>= Just . CFst
    go (CSnd p) = go p >>= Just . CSnd
    go (CCons p p') = both CCons go p p' 
    go (CPi p p') = both CPi go p p'
    go (CSigma p p') = both CSigma go p p'
    go _ = Nothing
