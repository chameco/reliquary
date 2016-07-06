module Main where

import System.Environment

import qualified Data.Map as Map

import Control.Monad.State

import Text.Parsec.Prim

import Reliquary.Parser
import Reliquary.Evaluate

defaultSymTab :: SymbolTable a
defaultSymTab = Map.fromList [
                             ("call", stackCall),
                             ("compose", stackCompose),
                             ("drop", stackDrop),
                             ("swap", stackSwap),
                             ("succ", churchSucc),
                             ("+", churchAdd)
                             ]

main :: IO ()
main = do
        [f] <- getArgs
        s <- readFile f
        let prog = runParser parseProgram () f s in
            case prog of
                Left err -> print err
                Right definitions -> let symtab = compileProgram defaultSymTab definitions
                                         f = Map.lookup "main" symtab in
                                             case f of
                                                 Just f -> case numValue $ head $ call f [] of
                                                               Just n -> print n
                                                               Nothing -> print "Error"
                                                 Nothing -> print "error"
