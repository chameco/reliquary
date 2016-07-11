module Reliquary.Evaluate where

import Data.Maybe (fromMaybe)

import Reliquary.AST

import qualified Reliquary.Core.AST as SF

--translate :: Term -> SF.Term
--translate (Word s) = SF.TermVar s
--translate (Literal n) = SF.TermLambda "##stack" SF.TypeNat $ foldr succ (SF.TermVar "##stack") [1..n] where
    --succ _ = SF.TermSucc
--translate (Block exprs) = SF.TermLambda "##stack" SF.TypeNat $ translate $ Quasi exprs
--translate (Quasi exprs) = foldr (SF.TermApply . translate) SF.TermZero $ reverse exprs
