module Reliquary.Core.Evaluate where

import Reliquary.Core.AST
import Reliquary.Core.DeBruijn

normalize1 :: Term -> Maybe Term
normalize1 (TermApply (TermLambda ntype body) t) = Just $ shift (-1) $ subst 0 t body
normalize1 (TermApply f t) = case normalize1 f of (Just f') -> Just $ TermApply f' t
                                                  Nothing -> Nothing
normalize1 (TermPi p p') = case normalize1 p of
                               (Just t) ->  case normalize1 p' of
                                                (Just t') -> Just $ TermPi p' t'
                                                Nothing -> Nothing
                               Nothing -> Nothing
                                           
normalize1 _ = Nothing

normalize :: Term -> Term
normalize t = rec $ normalize1 t where
    rec (Just t') = normalize t'
    rec Nothing = t
