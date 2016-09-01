module Reliquary.Core.Evaluate where

import Reliquary.Core.AST
import Reliquary.Core.DeBruijn

normalize1 :: CoreTerm -> Maybe CoreTerm
normalize1 (CApply (CLambda ntype body) t) = Just $ shift (-1) $ subst 0 t body
normalize1 (CApply f t) = case normalize1 f of (Just f') -> Just $ CApply f' t
                                               Nothing -> Nothing
normalize1 (CFst (CCons t _)) = Just t
normalize1 (CSnd (CCons _ t)) = Just t
normalize1 (CFst p) = normalize1 p >>= Just . CFst
normalize1 (CSnd p) = normalize1 p >>= Just . CSnd
normalize1 (CPi p p') = case normalize1 p of
                            (Just t) -> case normalize1 p' of
                                            (Just t') -> Just $ CPi p' t'
                                            Nothing -> Nothing
                            Nothing -> Nothing
normalize1 (CSigma p p') = case normalize1 p of
                              (Just t) -> case normalize1 p' of
                                              (Just t') -> Just $ CSigma p' t'
                                              Nothing -> Nothing
                              Nothing -> Nothing
normalize1 _ = Nothing

normalize :: CoreTerm -> CoreTerm
normalize t = rec $ normalize1 t where
    rec (Just t') = normalize t'
    rec Nothing = t
