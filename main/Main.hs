module Main where

import System.Environment

import System.Console.Haskeline

import Control.Monad.Except

import Reliquary.Utils.Error

import Reliquary.Core.AST
import Reliquary.Core.TypeCheck
import Reliquary.Core.Evaluate

import Reliquary.Parser
import Reliquary.Evaluate
import Reliquary.Dictionary

defaultDict :: Dictionary
defaultDict = [Entry "*" (CLambda CUnitType (CCons CStar CUnit)) (CPi CUnitType (CSigma CStar CUnitType))
              ,Entry "()" (CLambda CUnitType (CCons CUnit CUnit)) (CPi CUnitType (CSigma CUnitType CUnitType))
              ,Entry "popStar" (CLambda (CSigma CStar CUnitType) CUnit) (CPi (CSigma CStar CUnitType) CUnitType)
              ,Entry "pass" (CLambda CUnitType CUnit) (CPi CUnitType CUnitType)
              ]

main :: IO ()
main = undefined
