{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Wyas.Eval where

import Data.List
import Data.Map.Strict (Map)
import Text.ParserCombinators.Parsec hiding (string)
import Data.Maybe
import qualified Data.Map.Strict as Map
import Control.Monad.Except
import Control.Monad.Reader
import Data.Functor.Identity

import Wyas.LispVal
import Wyas.Parser


parseAndEval :: String -> Either LispError LispVal
parseAndEval s =
  case parseLisp lispExpr s of
       Left e -> throwError (Parser e)
       Right a -> bareEval (evalD a)

newtype LispEvalT m a
  = LispEvalT
  { unLispEvalT :: ExceptT LispError (ReaderT Environment m) a
  } deriving ( Functor, Applicative, Monad, MonadReader Environment
             , MonadError LispError)

type Environment = Map String LispVal
type ApplyLisp = [LispVal] -> LispVal

data LispError
  = TypeError LispVal LispVal
  | AtomUndefined String
  | Parser ParseError
  | IncorrectEval LispVal
  deriving (Show, Eq)

type LispEvalM a = LispEvalT Identity a

bareEval :: LispEvalM LispVal -> Either LispError LispVal
bareEval = runIdentity
  . flip runReaderT coreEnv
  . runExceptT 
  . unLispEvalT

evalD :: LispVal -> LispEvalM LispVal
evalD val@(String _) = return val
evalD val@(Number _) = return val
evalD val@(Bool _) = return val
evalD val@(Float _) = return val
evalD val@(Character _) = return val
evalD (List [Atom "quote", val]) = return val
evalD (List [Atom "let", Atom name, value, body]) = 
  local (Map.insert name value) (evalD body)
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
       Just (List [Atom "lambda", List params, body]) -> do
         newBody <- substituteVariables args (map unAtom params) body
         evalD newBody
       _ -> throwError $ AtomUndefined fn

unAtom :: LispVal -> String
unAtom (Atom s) = s

substituteVariables :: [LispVal] -> [String] -> LispVal -> LispEvalM LispVal
substituteVariables args params body =
  local (Map.union (Map.fromList (zip params args))) (evalD body)

coreEnv :: Environment
coreEnv = Map.fromList
  [ ( "+", Fn lispAdd )
  , ( "+f", Fn lispFPAdd )
  , ( "-", Fn lispSub )
  , ( "*", Fn lispMul )
  ]

type LispFunction = [LispVal] -> LispVal

lispAdd :: LispFunction
lispAdd = Number . sum . map unNumber

lispMul :: LispFunction
lispMul = Number . product . map unNumber

unNumber :: LispVal -> Integer
unNumber (Number x) = x

unFloat :: LispVal -> Double
unFloat (Float x) = x
unFloat (Number x) = fromInteger x

lispFPAdd :: LispFunction
lispFPAdd = Float . sum . map unFloat

lispSub :: LispFunction
lispSub = Number . foldl1' (-) . map unNumber
