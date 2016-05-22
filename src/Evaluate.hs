module Evaluate where

import qualified Data.Map as Map

import Control.Arrow (second)

import Model

data StackFunction
        = SF (([StackFunction], Map.Map String StackFunction) -> ([StackFunction], Map.Map String StackFunction))
        | Abstract Expr

call :: StackFunction -> ([StackFunction], Map.Map String StackFunction) -> ([StackFunction], Map.Map String StackFunction)
call (SF f) = f

compose :: StackFunction -> StackFunction -> StackFunction
compose (SF f) (SF g) = SF (f . g)

stackCall :: StackFunction
stackCall = SF $ \(f:stack, symtab) -> call f (stack, symtab)

stackCompose :: StackFunction
stackCompose = SF $ \(f:g:stack, symtab) -> (compose f g:stack, symtab)

churchZero :: StackFunction
churchZero = SF $ \(f:stack, symtab) -> (stack, symtab)

churchOne :: StackFunction
churchOne = stackCall

churchPlus :: StackFunction
churchPlus = compose stackCall stackCompose

compileExpr :: Expr -> StackFunction
compileExpr (Program exprs) = SF $ foldr (flip (.) . call . compileExpr) id exprs
compileExpr (Word s) = SF $ \(stack, symtab) -> case Map.lookup s symtab of Just g -> call g (stack, symtab)
                                                                            Nothing -> (stack, symtab)
compileExpr (Literal n) = foldr (\_ -> compose churchOne) (SF id) [1..n]
compileExpr (Block exprs) = Abstract $ Block exprs
compileExpr (Definition s expr) = SF $ second $ Map.insert s (compileExpr expr)
