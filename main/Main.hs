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

blockFunc :: CoreTerm -> Either GenError CoreTerm
blockFunc (CCons (CBlock terms) CUnit) = fst <$> translateAll defaultDict terms
blockFunc t = throwError $ InternalError $ CFst t

blockType :: CoreTerm -> Either GenError CoreTerm
blockType (CCons (CBlock terms) CUnit) = fst <$> translateAll defaultDict terms >>= check []
blockType t = throwError $ InternalError $ CSnd t

defaultDict :: Dictionary
defaultDict = [ ("*",       (CLambda CUnitType (CCons CStar CUnit), ([], [CStar])))
              , ("()",      (CLambda CUnitType (CCons CUnit CUnit), ([], [CUnitType])))
              , ("popStar", (CLambda (CSigma CStar CUnitType) CUnit, ([CStar], [])))
              , ("popUnit", (CLambda (CSigma CUnitType CUnitType) CUnit, ([CUnitType], [])))
              , ("pass",    (CLambda CUnitType CUnit, ([], [])))
              , ("@",       (CUnsafe (CSigma CBlockType CUnitType) blockType blockFunc, ([CBlockType], [CPi CUnitType (CApply (CUnsafe CBlockType undefined undefined) (CVar 0))])))
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
