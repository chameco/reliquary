module Main where

import System.Environment

import System.Console.Haskeline

import Control.Monad.Except

import Reliquary.Core.AST
import Reliquary.Core.TypeCheck
import Reliquary.Core.Evaluate

import Reliquary.Parser
import Reliquary.Evaluate
import Reliquary.Dictionary

defaultDict :: Dictionary
defaultDict = [ ("*",       (CLambda CUnitType (CCons CStar CUnit), ([], [CStar])))
              , ("()",      (CLambda CUnitType (CCons CUnit CUnit), ([], [CUnitType])))
              , ("popStar", (CLambda (CSigma CStar CUnitType) CUnit, ([CStar], [])))
              , ("popUnit", (CLambda (CSigma CUnitType CUnitType) CUnit, ([CUnitType], [])))
              , ("pass",    (CLambda CUnitType CUnit, ([], [])))
              ]

repl :: String -> IO ()
repl line = case displayFunc . force <$> processRepl line of
                Left e -> putStrLn ("!!! " ++ displayError e)
                Right e -> putStrLn e
    where processRepl line = parseRepl line >>= translateAll defaultDict

main :: IO ()
main = runInputT defaultSettings loop where
    loop = do
        input <- getInputLine "rlq> "
        case input of Nothing -> outputStrLn "Goodbye."
                      Just i -> liftIO (repl i) >> loop
