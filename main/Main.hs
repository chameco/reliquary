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
defaultDict = [ ("*",       (CLambda CUnitType (CCons CStar CUnit), ([], [CStar])))
              , ("()",      (CLambda CUnitType (CCons CUnit CUnit), ([], [CUnitType])))
              , ("popStar", (CLambda (CSigma CStar CUnitType) CUnit, ([CStar], [])))
              , ("pass",    (CLambda CUnitType CUnit, ([], [])))
              ]

repl :: String -> IO ()
repl line = case processRepl line >>= \(t, ty) -> return $ displayTerm t ++ " : " ++ displayTerm ty of
                Left e -> putStrLn ("!!! " ++ displayError e)
                Right e -> putStrLn e
    where
        processRepl line = do
           t <- normalize <$> (parseRepl line >>= translate defaultDict)
           ty <- check [] t
           return (t, ty)

main :: IO ()
main = runInputT defaultSettings loop where
    loop = do
        input <- getInputLine "rlq> "
        case input of Nothing -> outputStrLn "Goodbye."
                      Just i -> liftIO (repl i) >> loop
