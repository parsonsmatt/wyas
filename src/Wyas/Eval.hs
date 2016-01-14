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

bareEval :: LispEvalM LispVal -> Either LispError LispVal
bareEval = runIdentity
  . flip runReaderT mempty
  . flip evalStateT () 
  . runExceptT 
  . unLispEvalT

evalD :: LispVal -> LispEvalM LispVal
evalD val@(String _) = return val
evalD val@(Number _) = return val
evalD val@(Bool _) = return val
evalD val@(Character _) = return val
evalD val@(Float _) = return val
evalD (List [Atom "quote", val]) = return val
evalD a = throwError ()
