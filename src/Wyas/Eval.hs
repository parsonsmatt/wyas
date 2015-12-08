{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Wyas.Eval where

import Control.Monad
import Data.Map.Strict (Map)
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

type Environment = Map String ApplyLisp
type EvalState = ()
type ApplyLisp = [LispVal] -> LispVal
type LispError = ()

type LispEvalM a = LispEvalT Identity a

class ( MonadState EvalState m
      , MonadReader Environment m
      , MonadError LispError m
      ) => LispEval m

-- | eval is a plain ol' function that evaluates a Lisp expression.
-- It doesn't expect or anticipate any environment or state.
eval :: LispEval m => LispVal -> m LispVal
eval (List (Atom x : xs)) = apply (Atom x) xs
eval a = return a

apply :: LispEval m => LispVal -> [LispVal] -> m LispVal
apply (Atom a) xs = do
  f <- asks (getSymbol a)
  maybe (throwError ()) (\f' -> return (f' xs)) f
apply _ _ = throwError ()

getSymbol :: String -> Environment -> Maybe ApplyLisp
getSymbol = Map.lookup

coreFunctions :: Environment
coreFunctions = Map.fromList
  [ ( "and", Bool . all truthy)
  , ( "or",  Bool . any truthy)
  ]
