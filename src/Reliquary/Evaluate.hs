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

translate :: Dictionary -> Term -> Either GenError CoreTerm
translate d (Word s) = fromMaybe (throwError $ NameNotInScope s) (pure . fst <$> dictLookup d s)
translate d (Block terms) = return $ CBlock terms
