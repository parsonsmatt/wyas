{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Wyas.Eval where

import Data.List
import Data.Map.Strict (Map)
import Data.Maybe
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

data LispError
  = TypeError LispVal LispVal
  | AtomUndefined String
  | IncorrectEval LispVal
  deriving Show

type LispEvalM a = LispEvalT Identity a

bareEval :: LispEvalM LispVal -> Either LispError LispVal
bareEval = runIdentity
  . flip runReaderT coreEnv
  . flip evalStateT () 
  . runExceptT 
  . unLispEvalT

evalD :: LispVal -> LispEvalM LispVal
evalD val@(String _) = return val
evalD val@(Number _) = return val
evalD val@(Bool _) = return val
evalD val@(Float _) = return val
evalD val@(Character _) = return val
evalD (List [Atom "quote", val]) = return val
evalD (List [Atom "let", Atom name, value, body]) = local (Map.insert name value) (evalD body)
evalD (List (Atom fn : args)) = do
  args' <- mapM evalD args
  apply fn args'
evalD (Atom name) = do
  val <- asks (Map.lookup name)
  case val of
       Just a -> evalD a
       Nothing -> throwError $ AtomUndefined name
evalD a = throwError $ IncorrectEval a

apply :: String -> [LispVal] -> LispEvalM LispVal
apply fn args = do
  f <- asks (Map.lookup fn)
  case f of
       Just (Fn g) -> return (g args)
       _ -> throwError $ AtomUndefined fn

coreEnv :: Environment
coreEnv = Map.fromList
  [ ( "+", lispAdd )
  , ( "+f", lispFPAdd )
  , ( "-", lispSub )
  ]

lispAdd :: LispVal
lispAdd = Fn (Number . sum . map unNumber)

unNumber :: LispVal -> Integer
unNumber (Number x) = x

unFloat :: LispVal -> Double
unFloat (Float x) = x
unFloat (Number x) = fromInteger x

lispFPAdd :: LispVal
lispFPAdd = Fn (Float . sum . map unFloat)

lispSub :: LispVal
lispSub = Fn (Number . foldl1' (-) . map unNumber)
