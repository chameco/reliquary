{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Reliquary.Utils.Monad where

import Control.Monad.Except

import Reliquary.Utils.Error

type CompilerMonad = ExceptT GenError IO

newtype Compiler a = Compiler { runCompiler :: CompilerMonad a }
                     deriving
                     ( Functor
                     , Applicative
                     , Monad
                     , MonadIO
                     , MonadError GenError )
