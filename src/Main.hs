module Main where

import System.Environment

import qualified Data.Map as Map

import Control.Monad.State

import Text.Parsec.Prim

import Parser
import Evaluate

main :: IO ()
main = do
        [f] <- getArgs
        s <- readFile f
        let prog = runParser parseProgram () f s in
            case prog of
                Left err -> print err
                Right tree -> print tree
