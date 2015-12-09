{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Wyas.Eval where

import Data.Map.Strict (Map)
-- import Data.Maybe
import qualified Data.Map.Strict as Map
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import Data.Functor.Identity

import Wyas.LispVal

newtype LispEvalT m a
  = LispEvalT
  { unLispEvalT :: ExceptT LispError (StateT EvalState (ReaderT Environment m)) a
  } deriving ( Functor, Applicative, Monad, MonadReader Environment
             , MonadState EvalState, MonadError LispError)

type Environment = Map String LispVal
type EvalState = ()
type ApplyLisp = [LispVal] -> LispVal
type LispError = ()

type LispEvalM a = LispEvalT Identity a

type LispEval m = 
  ( MonadState EvalState m
  , MonadReader Environment m
  , MonadError LispError m
  )

-- | eval is a plain ol' function that evaluates a Lisp expression.
-- It doesn't expect or anticipate any environment or state.
eval :: LispEval m => LispVal -> m LispVal
eval = return

getSymbol :: String -> Environment -> Maybe LispVal
getSymbol = Map.lookup
