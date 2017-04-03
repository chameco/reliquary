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

translate :: Dictionary -> Term -> Either GenError TypedFunction
translate d (Word s) = fromMaybe (throwError $ NameNotInScope s) (return <$> dictLookup d s)
translate d (Block terms) = return (CLambda CUnitType $ CBlock terms, ([], [CBlockType]))

translateAll :: Dictionary -> [Term] -> Either GenError TypedFunction
translateAll d terms = mapM (translate d) terms >>= composeAll

force :: TypedFunction -> TypedFunction
force (t, ty) = (normalize $ CApply t CUnit, ty)
