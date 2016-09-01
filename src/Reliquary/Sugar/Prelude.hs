module Reliquary.Sugar.Prelude where

import Reliquary.Sugar.AST
import Reliquary.Sugar.Scope

boolScope :: Scope
boolScope = [("Bool", SStar
                    , SPi (Just "a") SStar $ SPi Nothing (SVar "a") $ SPi Nothing (SVar "a") (SVar "a"))
            ,("true", SVar "Bool"
                    , SLambda "a" SStar $ SLambda "x" (SVar "a") $ SLambda "y" (SVar "a") (SVar "x"))
            ,("false", SVar "Bool"
                     , SLambda "a" SStar $ SLambda "x" (SVar "a") $ SLambda "y" (SVar "a") (SVar "y"))
            ,("and", SPi Nothing (SVar "Bool") $ SPi Nothing (SVar "Bool") $ SVar "Bool"
                   , SLambda "p" (SVar "Bool") $ SLambda "q" (SVar "Bool") $ SApply (SApply (SApply (SVar "p") (SVar "Bool")) (SVar "q")) (SVar "p"))
            ,("or", SPi Nothing (SVar "Bool") $ SPi Nothing (SVar "Bool") $ SVar "Bool"
                  , SLambda "p" (SVar "Bool") $ SLambda "q" (SVar "Bool") $ SApply (SApply (SApply (SVar "p") (SVar "Bool")) (SVar "p")) (SVar "q"))
            ,("not", SPi Nothing (SVar "Bool") $ SVar "Bool"
                   , SLambda "p" (SVar "Bool") $ SApply (SApply (SApply (SVar "p") (SVar "Bool")) (SVar "false")) (SVar "true"))
            ]

natScope :: Scope
natScope = [("Nat", SStar
                  , SPi (Just "a") SStar $ SPi Nothing (SPi Nothing (SVar "a") (SVar "a")) (SPi Nothing (SVar "a") (SVar "a")))
           ]

funcScope :: Scope
funcScope = [("compose", SPi (Just "a") SStar $ SPi (Just "b") SStar $ SPi (Just "c") SStar $ SPi Nothing (SPi Nothing (SVar "b") (SVar "c")) $ SPi Nothing (SPi Nothing (SVar "a") (SVar "b")) $ SPi Nothing (SVar "a") (SVar "c")
                       , SLambda "a" SStar $ SLambda "b" SStar $ SLambda "c" SStar $ SLambda "f" (SPi Nothing (SVar "b") (SVar "c")) $ SLambda "g" (SPi Nothing (SVar "a") (SVar "b")) $ SLambda "x" (SVar "a") (SApply (SVar "f") (SApply (SVar "g") (SVar "x"))))
            ]
