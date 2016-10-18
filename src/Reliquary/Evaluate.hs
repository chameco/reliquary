module Reliquary.Evaluate where

import Data.Maybe (fromMaybe)
import Data.List (elemIndex)

import Control.Monad
import Control.Monad.Except

import Reliquary.Utils.Monad
import Reliquary.Utils.Error

import Reliquary.Core.AST
import Reliquary.Core.DeBruijn
import Reliquary.Core.Evaluate
import Reliquary.Core.TypeCheck

import Reliquary.AST
import Reliquary.Dictionary

import Debug.Trace

needed :: CoreTerm -> CoreTerm -> Compiler Int
needed (CSigma t ts) (CSigma c cs) = if t == c then (+1) <$> needed ts cs else throwError $ Mismatch c t
needed _ CUnitType = return 0
needed (CSigma _ _) x = throwError $ NotPair x
needed x (CSigma _ _) = throwError $ NotPair x
needed x _ = throwError $ NotPair x

appendSigma :: CoreTerm -> CoreTerm -> CoreTerm -> Compiler CoreTerm
appendSigma st s t = go s st where
    go :: CoreTerm -> CoreTerm -> Compiler CoreTerm
    go _ CUnitType = return t
    go cs (CSigma x xs) = CCons (CFst cs) <$> go (CSnd cs) xs
    go _ t = throwError $ NotPair t

takeSigma :: Int -> CoreTerm -> CoreTerm
takeSigma 0 _ = CUnit
takeSigma n t = CCons (CFst t) $ takeSigma (n - 1) (CSnd t)

dropSigma :: Int -> CoreTerm -> CoreTerm
dropSigma n t = iterate CSnd t !! n

wrap :: CoreEnv -> CoreTerm -> CoreTerm -> Compiler CoreTerm
wrap env i o = do
        ity <- checkType env i
        oty <- checkType env o
        case ity of 
            (CPi _ iout) -> case oty of
                (CPi oin oout) -> do
                    n <- needed iout oin
                    CLambda iout <$> appendSigma oout (CApply (shift 1 o) (takeSigma n $ CVar 0)) (dropSigma n $ CVar 0)
                _ -> throwError $ NotFunction oty
            _ -> throwError $ NotFunction ity

compose :: CoreEnv -> CoreTerm -> CoreTerm -> Compiler CoreTerm
compose env f g = do
        fty <- checkType env f
        case fty of
            (CPi fin _) -> return $ CLambda fin $ CApply (shift 1 g) $ CApply (shift 1 f) $ CVar 0
            x -> throwError $ NotFunction x

composeList :: CoreEnv -> [CoreTerm] -> Compiler CoreTerm
composeList env = foldM (composeWrap env) (CLambda CUnitType CUnit) where
    composeWrap env f g = wrap env f g >>= compose env f

thunk :: CoreTerm -> CoreTerm
thunk = CLambda CUnitType . flip CCons CUnit . shift 1

churchNum :: Int -> CoreTerm
churchNum n = CLambda CStar $ CLambda (CPi (CVar 0) (CVar 1)) $ CLambda (CVar 1) $ iterate (CApply (CVar 1)) (CVar 0) !! n

translate :: CoreEnv -> [String] -> Term -> Compiler CoreTerm
translate _ env (Word s) = case elemIndex s env of
                               Just i -> return $ CVar i
                               Nothing -> throwError $ NameNotInScope s
translate _ _ (Literal v) = return $ thunk $ churchNum v
translate tenv env (Block terms) = do
        translated <- mapM (translate tenv env) terms
        composeList tenv translated
translate tenv env (ListType terms) = thunk <$> (translate tenv env (Block terms) >>= pure . flip CApply CUnit >>= normalize) where
    raiseCons CUnit = CUnitType
    raiseCons (CCons t ts) = CSigma t $ raiseCons ts
    raiseCons x = x

translateWith :: Dictionary -> Term -> Compiler CoreTerm
translateWith d = go d d where
    tenv = toCoreEnv d
    go (e:es) d t = do
        body <- go es d t
        return $ CApply (CLambda (entryType e) body) (entryTerm e)
    go [] d t = translate tenv (reverse $ entryName <$> d) t

translateDict :: Dictionary -> SourceDictionary -> Compiler Dictionary
translateDict env (SourceEntry n t:es) = do
        ct <- translateWith env t
        cty <- checkType (toCoreEnv env) ct
        translateDict (Entry n ct cty:env) es
translateDict env [] = return env
