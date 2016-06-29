{-# LANGUAGE GADTs #-}

module Evaluate where

import qualified Data.Map as Map

import Control.Arrow (first, second)

import Model

type Context a = ([StackFunction a], Map.Map String (StackFunction a))

class Purifiable a where
        purify :: a -> StackFunction a

data StackFunction a where 
        Pure :: (Context a -> Context a) -> StackFunction a
        Impure :: Purifiable a => a -> StackFunction a

instance Show (StackFunction a) where
        show (Pure _) = "Pure"
        show (Impure _) = "Impure"

instance Purifiable Integer where
        purify = churchNum

-- Helper Functions
call :: StackFunction a -> Context a -> Context a
call (Pure f) = f
call (Impure n) = call $ purify n

compose :: StackFunction a -> StackFunction a -> StackFunction a
compose f g = Pure (call f . call g)

numValue :: StackFunction Integer -> Context Integer -> Maybe Integer
numValue (Pure f) c = case f $ first ([Pure realAdd, Impure 0] ++) c  of (Impure res:stack, symtab) -> Just res
                                                                         _ -> Nothing
                                                                         where realAdd (Impure x:stack, symtab) = (Impure (x + 1):stack, symtab)
                                                                               realAdd x = x
numValue (Impure r) c = Just r

-- Basic Operations
stackCall :: StackFunction a
stackCall = Pure $ \(f:stack, symtab) -> call f (stack, symtab)

stackCompose :: StackFunction a
stackCompose = Pure $ \(f:g:stack, symtab) -> (compose f g:stack, symtab)

stackDrop :: StackFunction a
stackDrop = Pure $ \(x:stack, symtab) -> (stack, symtab)

stackSwap :: StackFunction a
stackSwap = Pure $ \(x:y:stack, symtab) -> (y:x:stack, symtab)

-- Church Encoding
churchZero :: StackFunction a
churchZero = stackDrop

churchSucc :: StackFunction a
churchSucc = Pure $ \(Pure n:s, st) -> ((Pure $ \(Pure f:stack, symtab) -> case n (Pure f:stack, symtab) of
                                                                               (stack', symtab') -> f (stack', symtab')):s, st)

churchTrue :: StackFunction a
churchTrue = compose stackDrop stackSwap

churchFalse :: StackFunction a
churchFalse = stackDrop

churchAdd :: StackFunction a
churchAdd = Pure $ \(Pure n:s, st) -> n (churchSucc:s, st)

churchNum :: Integer -> StackFunction a
churchNum n = head $ fst $ call (foldr (\_ -> compose churchSucc) (Pure id) [1..n]) ([churchZero], Map.empty)

compileExpr :: Expr -> StackFunction a
compileExpr (Program exprs) = foldr (flip compose . compileExpr) (Pure id) exprs
compileExpr (Word s) = Pure $ \(stack, symtab) -> case Map.lookup s symtab of Just g -> call g (stack, symtab)
                                                                              Nothing -> (stack, symtab)
compileExpr (Literal n) = Pure $ first $ (:) $ churchNum n
compileExpr (Block exprs) = Pure $ first $ (:) (foldr (flip compose . compileExpr) (Pure id) exprs)
compileExpr (Definition s expr) = Pure $ second $ Map.insert s (compileExpr expr)
