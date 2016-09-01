module Reliquary.Sugar.Scope where

import Reliquary.Core.Evaluate
import Reliquary.Core.TypeCheck

import Reliquary.Sugar.AST
import Reliquary.Sugar.Evaluate

type Scope = [(String, SugarTerm, SugarTerm)]

withinScope :: Scope -> SugarTerm -> SugarTerm
withinScope ((n, ty, val):ss) t = SApply (SLambda n ty $ withinScope ss t) val
withinScope [] t = t
