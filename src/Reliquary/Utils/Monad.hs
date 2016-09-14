{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Reliquary.Utils.Monad where

import Control.Monad.Except

import Reliquary.Core.AST

data GenError = Mismatch CoreTerm CoreTerm
              | NotInScope
              | NotType CoreTerm 
              | NotFunction CoreTerm
              | NotPair CoreTerm 
              | NameNotInScope String
              | Redefinition String
              deriving Show

type CompilerMonad = ExceptT GenError IO

newtype Compiler a = Compiler { runCompiler :: CompilerMonad a }
                     deriving
                     ( Functor
                     , Applicative
                     , Monad
                     , MonadIO
                     , MonadError GenError )
