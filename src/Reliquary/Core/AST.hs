module Reliquary.Core.AST where

data Name = Integer

data CoreTerm = CStar
              | CVar Int
              | CApply CoreTerm CoreTerm
              | CLambda CoreTerm CoreTerm
              | CCons CoreTerm CoreTerm
              | CFst CoreTerm
              | CSnd CoreTerm
              | CPi CoreTerm CoreTerm
              | CSigma CoreTerm CoreTerm
              deriving Eq

type CoreEnv = [(CoreTerm, Int)]

data CoreError = Mismatch CoreTerm CoreTerm
               | NotInScope
               | NotType CoreTerm 
               | NotFunction CoreTerm
               | NotPair CoreTerm 
               deriving Show

instance Show CoreTerm where
        show CStar = "*"
        show (CVar i) = show i
        show (CApply f t) = "(" ++ show f ++ " " ++ show t ++ ")"
        show (CLambda ty t) = "λ" ++ show ty ++ "." ++ show t
        show (CCons t t') = "[" ++ show t ++ "," ++ show t' ++ "]"
        show (CFst t) = "fst " ++ show t
        show (CSnd t) = "snd " ++ show t
        show (CPi ty t) = "Π" ++ show ty ++ "." ++ show t
        show (CSigma ty t) = "Σ" ++ show ty ++ "." ++ show t
