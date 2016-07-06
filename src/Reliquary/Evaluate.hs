{-# LANGUAGE GADTs #-}

module Reliquary.Evaluate where

import qualified Data.Map as Map

import Data.Maybe (fromMaybe)

import Reliquary.AST

type SymbolTable a = Map.Map String (StackFunction a)

type Context a = [StackFunction a]

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

defer :: StackFunction a -> StackFunction a
defer f = Pure $ (:) f

numValue :: StackFunction Integer -> Maybe Integer
numValue (Pure f) = case f [Pure realAdd, Impure 0] of Impure res:stack -> Just res
                                                       _ -> Nothing
                                                       where realAdd (Impure x:stack) = Impure (x + 1):stack
                                                             realAdd stack = stack
numValue (Impure r) = Just r

-- Basic Operations
stackCall :: StackFunction a
stackCall = Pure $ \(f:stack) -> call f stack

stackCompose :: StackFunction a
stackCompose = Pure $ \(f:g:stack) -> compose f g:stack

stackDrop :: StackFunction a
stackDrop = Pure $ \(x:stack) -> stack

stackSwap :: StackFunction a
stackSwap = Pure $ \(x:y:stack) -> y:x:stack

-- Quasiquote Binding
stackBind :: StackFunction a
stackBind = Pure $ \(x:stack) -> Pure (\stack' -> x:stack'):stack

-- Church Encoding
churchZero :: StackFunction a
churchZero = stackDrop

churchSucc :: StackFunction a
churchSucc = Pure $ \(Pure n:s) -> Pure (\(Pure f:stack) -> case n (Pure f:stack) of
                                                                stack' -> f stack'):s

churchTrue :: StackFunction a
churchTrue = compose stackDrop stackSwap

churchFalse :: StackFunction a
churchFalse = stackDrop

churchAdd :: StackFunction a
churchAdd = Pure $ \(Pure n:s) -> n (churchSucc:s)

churchNum :: Integer -> StackFunction a
churchNum n = head $ call (foldr (\_ -> compose churchSucc) (Pure id) [1..n]) [churchZero]

compileProgram :: SymbolTable a -> [Definition] -> SymbolTable a
compileProgram symtab (Definition name e:defs) = compileProgram (Map.insert name (compileExpr symtab e) symtab) defs
compileProgram symtab [] = symtab

compileExpr :: SymbolTable a -> Term -> StackFunction a
compileExpr symtab (Word s) = fromMaybe (Pure id) (Map.lookup s symtab)
compileExpr symtab (Literal n) = defer $ churchNum n
compileExpr symtab (Block exprs) = compose (defer normal) quasi where
    isQuasi (Quasi _) = True
    isQuasi _ = False
    quasi = compileExpr symtab $ Quasi $ filter isQuasi exprs
    normal = compileExpr symtab $ Quasi $ filter (not . isQuasi) exprs
compileExpr symtab (Quasi exprs) = foldr (flip compose . compileExpr symtab) (Pure id) exprs
