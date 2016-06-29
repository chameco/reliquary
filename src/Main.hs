module Main where

import System.Environment

import qualified Data.Map as Map

import Control.Monad.State

import Text.Parsec.Prim

import Parser
import Evaluate

defaultContext :: Context Integer
defaultContext = ([],
                 Map.fromList [
                              ("call", stackCall),
                              ("compose", stackCompose),
                              ("drop", stackDrop),
                              ("swap", stackSwap),
                              ("plus", churchAdd)
                              ])

main :: IO ()
main = do
        [f] <- getArgs
        s <- readFile f
        let prog = runParser parseProgram () f s in
            case prog of
                Left err -> print err
                Right tree -> let res = call (compileExpr tree) defaultContext in
                    case numValue (head $ fst res) res of
                        Just n -> print n
                        Nothing -> print "Error"
