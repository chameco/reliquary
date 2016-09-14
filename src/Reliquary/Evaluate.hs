module Reliquary.Evaluate where

import Data.Maybe (fromMaybe)
import Data.List (elemIndex)

import Control.Monad
import Control.Monad.Except

import Reliquary.Utils.Monad

import Reliquary.Core.AST
import Reliquary.Core.TypeCheck

import Reliquary.AST
import Reliquary.Dictionary

interleaveCoerce :: [CoreTerm] -> Compiler [CoreTerm]
interleaveCoerce (t1:t2:ts) = do
        t1ty <- checkType [] t1
        t2ty <- checkType [] t1
        case t1ty of 
            (CPi _ t1out) -> case t2ty of
                (CPi t2in _) -> do
                    coercion <- coerce t1out t2in
                    ([t1, coercion] ++) <$> interleaveCoerce (t2:ts)
                _ -> (t1:) <$> interleaveCoerce (t2:ts)
            _ -> (t1:) <$> interleaveCoerce (t2:ts)
interleaveCoerce [t] = return [t]
interleaveCoerce [] = return []

coerce :: CoreTerm -> CoreTerm -> Compiler CoreTerm
coerce i o = CLambda i
             <$> (takeSigma
                  <$> join (return needed <*> checkType [] i <*> checkType [] o)
                  <*> pure (CVar 0)) where
    needed :: CoreTerm -> CoreTerm -> Compiler Int
    needed (CSigma t ts) (CSigma c cs) = if t == c then (+1) <$> needed ts cs else throwError $ Mismatch t c
    needed _ CUnitType = return 0
    needed x@(CSigma _ _) _ = throwError $ NotPair x
    needed _ x@(CSigma _ _) = throwError $ NotPair x
    needed x _ = throwError $ NotPair x

    takeSigma :: Int -> CoreTerm -> CoreTerm
    takeSigma 0 _ = CUnit
    takeSigma n t = CCons (CFst t) $ takeSigma (n - 1) (CSnd t)

compose :: CoreTerm -> CoreTerm -> Compiler CoreTerm
compose f g = do -- computes g . f
        fty <- checkType [] f
        case fty of
            (CPi fin _) -> return $ CLambda fin $ CApply f (CApply g $ CVar 0)
            x -> throwError $ NotFunction x

staticId :: CoreTerm -> CoreTerm
staticId t = CLambda t $ CVar 0

thunk :: CoreTerm -> CoreTerm
thunk = CLambda CUnitType

churchNum :: Int -> CoreTerm
churchNum n = CLambda CStar $ CLambda (CPi (CVar 0) (CVar 0)) $ CLambda (CVar 1) $ iterate (CApply (CVar 1)) (CVar 0) !! n

translate :: Term -> Compiler CoreTerm
translate = go [] where
    go :: [String] -> Term -> Compiler CoreTerm
    go _ Star = return CStar
    go _ UnitType = return CUnitType
    go _ Unit = return CUnit
    go env (Definition s ty t b) = CApply <$> (CLambda <$> go env ty <*> go (s:env) b) <*> go env t
    go env (Word s) = case elemIndex s env of
                          Just i -> return $ CVar i
                          Nothing -> throwError $ NameNotInScope s
    go _ (Literal v) = return $ churchNum v
    go env (Block terms) = composeHelp terms where
        composeHelp (t1:t2:ts) = do
            t1c <- go env t1
            rest <- composeHelp (t2:ts)
            coercion <- coerce t1c rest
            join $ compose t1c <$> compose coercion rest
        composeHelp [t] = thunk <$> go env t
        composeHelp [] = return $ staticId CUnitType
    go env (ListType terms) = listHelp terms where
        listHelp = foldr (\t -> (<*>) (CSigma <$> go env t)) $ return CUnit
