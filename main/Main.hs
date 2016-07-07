module Main where

import System.Environment

import qualified Data.Map as Map

import Control.Monad.State

import Text.Parsec.Prim

import qualified Reliquary.Parser as Rel
import qualified Reliquary.AST as Rel
import qualified Reliquary.Evaluate as Rel

import qualified SystemF.AST as SF
import qualified SystemF.Type as SF
import qualified SystemF.TypeCheck as SF
import qualified SystemF.Evaluate as SF

main :: IO ()
main = undefined
