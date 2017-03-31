module Reliquary.Core.Utils where

import Control.Monad.Except

import Reliquary.Utils.Error

import Reliquary.Core.AST
import Reliquary.Core.DeBruijn

thunk :: CoreTerm -> CoreTerm
thunk = CLambda CUnitType . flip CCons CUnit . shift 1

church :: Int -> CoreTerm
church n = CLambda CStar $ CLambda (CPi (CVar 0) (CVar 1)) $ CLambda (CVar 1) $ iterate (CApply (CVar 1)) (CVar 0) !! n

displayCore :: Either GenError CoreTerm -> String
displayCore (Left e) = displayError e
displayCore (Right t) = displayTerm t
