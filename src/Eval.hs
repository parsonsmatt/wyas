module Eval where

import Control.Monad.Except

import LispError
import LispVal

type LispEval = Either LispError

eval :: LispVal -> LispEval LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval val@(Atom _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom f : args)) = apply f =<< mapM eval args

apply :: String -> [LispVal] -> LispEval LispVal
apply func args =
  maybe (throwError (NotFunction "Unrecognized primitive function" func))
        ($ args)
        (lookup func primitives)

primitives :: [(String, [LispVal] -> LispEval LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispEval LispVal
numericBinop _ [] = throwError (NumArgs 2 [])
numericBinop _ singleVal@[_] = throwError (NumArgs 2 singleVal)
numericBinop op params =
  Number . foldl1 op <$> mapM unpackNum params

unpackNum :: LispVal -> LispEval Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in 
                           if null parsed 
                              then return 0
                              else return (fst $ head parsed)
unpackNum (List [n]) = unpackNum n
unpackNum _ = return 0
