module Reliquary.Core.Prim where

import Reliquary.Core.Sugar
import Reliquary.Core.Scope

boolScope :: Scope
boolScope = [("Bool", SPi "a" SStar $ SPi "_" (SVar "a") $ SPi "__" (SVar "a") (SVar "a"))
            ,("True", SLambda "a" SStar $ SLambda "x" (SVar "a") $ SLambda "y" (SVar "a") (SVar "x"))
            ,("False", SLambda "a" SStar $ SLambda "x" (SVar "a") $ SLambda "y" (SVar "a") (SVar "y"))
            ]
