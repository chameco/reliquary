module Reliquary.Sugar.AST where

import Reliquary.Core.AST

type SugarName = Maybe String
type SugarEnv = [SugarName]

data SugarTerm = SStar
               | SVar String
               | SApply SugarTerm SugarTerm
               | SLambda String SugarTerm SugarTerm
               | SPi SugarName SugarTerm SugarTerm
               | SSigma SugarTerm SugarTerm
               | SCons SugarTerm SugarTerm
               | SFst SugarTerm
               | SSnd SugarTerm

data SugarError = NameNotInScope String
                | Redefinition String
                deriving Show

instance Show SugarTerm where
        show SStar = "*"
        show (SVar n) = n
        show (SApply f t) = "(" ++ show f ++ " " ++ show t ++ ")"
        show (SLambda n ty t) = "λ" ++ n ++ ":" ++ show ty ++ "." ++ show t
        show (SPi (Just n) ty t) = "Π" ++ n ++ ":" ++ show ty ++ "." ++ show t
        show (SPi Nothing ty t) = "(" ++ show ty ++ " → " ++ show t ++ ")"
        show (SSigma ty ty') = show ty ++ " × " ++ show ty'
        show (SCons t t') = show t ++ ":" ++ show t'
        show (SFst t) = "fst " ++ show t
        show (SSnd t) = "snd " ++ show t
