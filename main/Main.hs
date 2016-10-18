module Main where

import System.Environment

import System.Console.Haskeline

import Control.Monad.Except

import Rainbow

import Reliquary.Utils.Monad
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

repl :: String -> IO ()
repl line = do
        result <- runExceptT $ runCompiler (processRepl line >>= \(t, ty) -> liftIO (putStrLn (displayTerm t ++ " : " ++ displayTerm ty)))
        case result of Left e -> putStrLn ("!!! " ++ displayError e)
                       _ -> return ()
    where
        processRepl line = do
            ct <- parseRepl line >>= translateWith defaultDict
            let wrapped = CApply ct CUnit in do
                cty <- checkType [] wrapped
                result <- normalize wrapped
                return (result, cty)

main :: IO ()
main = runInputT defaultSettings loop where
    loop = do
        input <- getInputLine "rlq> "
        case input of Nothing -> outputStrLn "Goodbye."
                      Just input -> liftIO (repl input) >> loop
