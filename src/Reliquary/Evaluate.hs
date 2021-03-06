module Reliquary.Evaluate where

import Data.Maybe (fromMaybe)
import Data.List (elemIndex)

import Control.Monad
import Control.Monad.Except

import Reliquary.Core.AST
import Reliquary.Core.Evaluate
import Reliquary.Core.TypeCheck

import Reliquary.AST
import Reliquary.Dictionary

import Debug.Trace

translate1 :: Dictionary -> [String] -> Term -> Either GenError Typed
translate1 d env (Word s) = case do i <- elemIndex s env
                                    ty <- snd <$> dictLookup d s
                                    Just (CVar i, ty)
                                of Nothing -> throwError $ NameNotInScope s
                                   Just ret -> return ret
translate1 _ _ (Block terms) = return $ merge (CLambda CUnitType . flip CCons CUnit) (CPi CUnitType . flip CSigma CUnitType) $ go terms where
    go [] = (CUnit, CUnitType)
    go (t:ts) = merge (CCons $ CRelTerm t) (CSigma CRelTermType) $ go ts
    merge :: (CoreTerm -> CoreTerm) -> (CoreTerm -> CoreTerm) -> Typed -> Typed
    merge f f' (t, t') = (f t, f' t')

translateAll :: Dictionary -> [Term] -> Either GenError Typed
translateAll d terms = mapM (translate1 d env) terms >>= composeAll d
    where env = reverse $ fst <$> d

eval :: Dictionary -> CoreTerm -> Typed -> Either GenError Typed
eval d base (t, _) = do
        let wrapped = dictWrap d $ CApply t base
        ty <- check [] wrapped
        return (normalize wrapped, ty)

raw :: Dictionary -> Typed -> Either GenError Typed
raw d (t, _) = do
        ty <- check [] $ dictWrap d t
        return (t, ty)

wrapped :: Dictionary -> Typed -> Either GenError Typed
wrapped d (t, _) = do
        ty <- check [] $ dictWrap d t
        return (dictWrap d t, ty)
