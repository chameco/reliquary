module Reliquary.Evaluate where

import Data.Maybe (fromMaybe)
import Data.List (elemIndex)

import Control.Monad
import Control.Monad.Except

import Reliquary.Core.AST
import Reliquary.Core.Evaluate

import Reliquary.AST
import Reliquary.Dictionary

import Debug.Trace

translate :: Dictionary -> Term -> Either GenError Typed
translate d (Word s) = fromMaybe (throwError $ NameNotInScope s) (return <$> dictLookup d s)
translate d (Block terms) = return $ merge (CLambda CUnitType . flip CCons CUnit) (CPi CUnitType . flip CSigma CUnitType) $ go terms where
    go [] = (CUnit, CUnitType)
    go (t:ts) = merge (CCons $ CRelTerm t) (CSigma CRelTermType) $ go ts
    merge :: (CoreTerm -> CoreTerm) -> (CoreTerm -> CoreTerm) -> Typed -> Typed
    merge f f' (t, t') = (f t, f' t')

translateAll :: Dictionary -> [Term] -> Either GenError Typed
translateAll d terms = mapM (translate d) terms >>= composeAll

force :: Typed -> Typed
force (t, ty) = (normalize $ CApply t CUnit, ty)
