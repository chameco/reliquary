module Evaluate where

import qualified Data.Map as Map

import Control.Arrow (first, second)

import Model

data StackFunction = Pure (([StackFunction], Map.Map String StackFunction) -> ([StackFunction], Map.Map String StackFunction))
                   | Impure Integer

-- Helper Functions
call :: StackFunction -> ([StackFunction], Map.Map String StackFunction) -> ([StackFunction], Map.Map String StackFunction)
call (Pure f) = f
call (Impure n) = call $ churchNum n

compose :: StackFunction -> StackFunction -> StackFunction
compose f g = Pure (call f . call g)

numValue :: StackFunction -> Maybe Integer
numValue (Pure f) = case f ([Pure realAdd, Impure 0], Map.empty) of (Impure res:stack, symtab) -> Just res
                                                                    _ -> Nothing
                                                                    where realAdd (Impure x:stack, symtab) = (Impure (x + 1):stack, symtab)
                                                                          realAdd x = x
numValue (Impure r) = Just r

printStackFunction :: StackFunction -> IO ()
printStackFunction (Pure _) = putStr "Pure"
printStackFunction (Impure n) = putStr $ show n

printStack :: [StackFunction] -> IO ()
printStack stack = foldl combine (putStr "[") stack >> putStrLn "]" where
    combine left right = left >>= \_ -> printStackFunction right >>= \_ -> putStr ", "

-- Basic Operations
stackCall :: StackFunction
stackCall = Pure $ \(f:stack, symtab) -> call f (stack, symtab)

stackCompose :: StackFunction
stackCompose = Pure $ \(f:g:stack, symtab) -> (compose f g:stack, symtab)

stackDrop :: StackFunction
stackDrop = Pure $ \(x:stack, symtab) -> (stack, symtab)

stackSwap :: StackFunction
stackSwap = Pure $ \(x:y:stack, symtab) -> (y:x:stack, symtab)

-- Church Encoding
churchZero :: StackFunction
churchZero = stackDrop

churchSucc :: StackFunction
churchSucc = Pure $ \(Pure n:s, st) -> ((Pure $ \(Pure f:stack, symtab) -> case n (Pure f:stack, symtab) of
                                                                               (stack', symtab') -> f (stack', symtab')):s, st)

churchTrue :: StackFunction
churchTrue = compose stackDrop stackSwap

churchFalse :: StackFunction
churchFalse = stackDrop

churchAdd :: StackFunction
churchAdd = Pure $ \(Pure n:s, st) -> n (churchSucc:s, st)

churchNum :: Integer -> StackFunction
churchNum n = head $ fst $ call (foldr (\_ -> compose churchSucc) (Pure id) [1..n]) ([churchZero], Map.empty)

compileExpr :: Expr -> StackFunction
compileExpr (Program exprs) = foldr (flip compose . compileExpr) (Pure id) exprs
compileExpr (Word s) = Pure $ \(stack, symtab) -> case Map.lookup s symtab of Just g -> call g (stack, symtab)
                                                                              Nothing -> (stack, symtab)
compileExpr (Literal n) = churchNum n
compileExpr (Block exprs) = Pure $ first $ (:) (foldr (flip compose . compileExpr) (Pure id) exprs)
compileExpr (Definition s expr) = Pure $ second $ Map.insert s (compileExpr expr)
