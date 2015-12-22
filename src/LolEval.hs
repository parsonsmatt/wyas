module LolEval where

import Control.Monad.Error
import Text.ParserCombinators.Parsec
import Data.IORef


import Wyas.LispVal
import Wyas.Parser


eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval val@(Character _) = val
eval val@(Float _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = 
  [ ( "+", numericBinop (+))
  , ( "-", numericBinop (-))
  , ( "*", numericBinop (*))
  , ( "/", numericBinop div)
  , ( "mod", numericBinop mod)
  , ( "quotient", numericBinop quot)
  , ( "remainder", numericBinop rem)
  ]


numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in 
                           if null parsed 
                              then 0
                              else fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0

-- with errors

data LispError
  = NumArgs Integer [LispVal]
  | TypeMismatch String LispVal
  | Parser ParseError
  | BadSpecialForm String LispVal
  | NotFunction String String
  | UnboundVar String String
  | Default String
  deriving (Eq, Show)

instance Error LispError where
  noMsg = Default "An error has occurred"
  strMsg = Default

trapError action = catchError action (return . show)
extractValue (Right a) = a

readExpr :: String -> Either LispError LispVal
readExpr i = case runParser lispExpr () "lisp" i of
                  Left e -> throwError $ Parser e
                  Right val -> return val

eval' :: LispVal -> Either LispError LispVal
eval' val@(String _) = return val
eval' val@(Number _) = return val
eval' val@(Bool _) = return val
eval' (List [Atom "quote", val]) = return val
eval' (List (Atom func : args)) = mapM eval' args >>= apply' func
eval' (List [Atom "if", pred, conseq, alt]) = do
  result <- eval' pred
  case result of
       Bool False -> eval' alt
       otherwise  -> eval' conseq 
eval' badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply' :: String -> [LispVal] -> Either LispError LispVal
apply' func args =
  maybe (throwError $ NotFunction "Unrecognized primitive function" func)
        ($ args)
        (lookup func primitives')

type ThrowsError = Either LispError

primitives' :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives' = 
  [ ( "+", numericBinop' (+))
  , ( "-", numericBinop' (-))
  , ( "*", numericBinop' (*))
  , ( "/", numericBinop' div)
  , ( "mod", numericBinop' mod)
  , ( "quotient", numericBinop' quot)
  , ( "remainder", numericBinop' rem)
  ]



numericBinop' :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop' op           []  = throwError $ NumArgs 2 []
numericBinop' op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop' op params        = mapM unpackNum' params >>= return . Number . foldl1 op

unpackNum' :: LispVal -> Either LispError Integer
unpackNum' (Number n) = return n
unpackNum' a = throwError $ TypeMismatch "number" a

-- now with iorefs!

type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

type IOErr = ErrorT LispError IO


