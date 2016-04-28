module Main where

import System.Environment
import Text.Parsec.Prim
import Control.Monad.State

import Parser
import CodeGen

main :: IO ()
main = do
    [f] <- getArgs
    s <- readFile f
    let prog = runParser parseProgram () f s
    case prog of
        Left err -> print err
        Right tree -> putStr . buildOutput $ runState (generate tree) ("", "", "", "")
