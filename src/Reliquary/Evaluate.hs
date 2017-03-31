module Reliquary.Evaluate where

import Data.Maybe (fromMaybe)
import Data.List (elemIndex)

import Control.Monad
import Control.Monad.Except

import Reliquary.Utils.Error

import Reliquary.Core.AST
import Reliquary.Core.DeBruijn
import Reliquary.Core.Evaluate
import Reliquary.Core.TypeCheck
import Reliquary.Core.Utils

import Reliquary.AST
import Reliquary.Dictionary

import Debug.Trace

translate1 :: Dictionary -> Term -> Either GenError CoreTerm
translate1 d (Word s) =  case dictLookup d s of Just e -> return $ entryTerm e
                                                Nothing -> throwError $ NameNotInScope s
translate1 _ (Literal v) = return $ thunk $ church v
translate1 d (Block terms) = return $ CBlock terms
