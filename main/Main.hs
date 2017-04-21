module Main where

import System.Environment

import System.Console.Haskeline

import Control.Monad.Except

import Reliquary.Core.AST
import Reliquary.Core.TypeCheck
import Reliquary.Core.Evaluate
import Reliquary.Core.Utils

import Reliquary.Parser
import Reliquary.Evaluate
import Reliquary.Dictionary

star = (CLambda CUnitType $ CCons CStar CUnit, CPi CUnitType $ CSigma CStar CUnitType)
unit = (CLambda CUnitType $ CCons CUnit CUnit, CPi CUnitType $ CSigma CUnitType CUnitType)
starstar = (CLambda CUnitType $ CCons CStar $ CCons CStar CUnit, CPi CUnitType $ CSigma CStar $ CSigma CStar CUnitType)
popStar = (CLambda CStar $ CLambda CUnitType CUnit, CPi CStar $ CPi CUnitType CUnitType)
idf = (CLambda CStar $ CLambda (CVar 0) $ CLambda CUnitType $ CCons (CVar 1) CUnit, CPi CStar $ CPi (CVar 0) $ CPi CUnitType $ CSigma (CVar 2) CUnitType)
dp = putStrLn . displayTerm
dc = putStrLn . displayCore . check []

defaultDict :: Dictionary
defaultDict = [ ("*",       (CLambda CUnitType $ CCons CStar CUnit, CPi CUnitType $ CSigma CStar CUnitType))
              , ("**",      (CLambda CUnitType $ CCons CStar $ CCons CStar CUnit, CPi CUnitType $ CSigma CStar $ CSigma CStar CUnitType))
              , ("popStar", (CLambda CStar $ CLambda CUnitType CUnit, CPi CStar $ CPi CUnitType CUnitType))
              , ("pushT",   (CLambda CUnitType $ CCons (CLambda CStar CStar) CUnit, CPi CUnitType $ CSigma (CPi CStar CStar) CUnitType))
              , ("popT",    (CLambda (CPi CStar CStar) $ CLambda CUnitType CUnit, CPi (CPi CStar CStar) $ CPi CUnitType CUnitType))
              , ("pushTy",  (CLambda CUnitType $ CCons (CPi CStar CStar) CUnit, CPi CUnitType $ CSigma CStar CUnitType))
              , ("id",      (CLambda CStar $ CLambda (CVar 0) $ CLambda CUnitType $ CCons (CVar 1) CUnit
                            ,CPi CStar $ CPi (CVar 0) $ CPi CUnitType $ CSigma (CVar 2) CUnitType
                            ))
              , ("pass",    (CLambda CUnitType CUnit, CPi CUnitType CUnitType))
              ]

repl :: String -> IO ()
repl line = case displayTyped <$> (processRepl line >>= eval defaultDict CUnit) of
                Left e -> putStrLn ("!!! " ++ displayError e)
                Right e -> putStrLn e
    where processRepl line = parseRepl line >>= translateAll defaultDict

main :: IO ()
main = runInputT defaultSettings loop where
    loop = do
        input <- getInputLine "rlq> "
        case input of Nothing -> outputStrLn "Goodbye."
                      Just i -> liftIO (repl i) >> loop
