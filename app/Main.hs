module Main where

import System.IO
import Control.Monad
import Wyas
import Wyas.Pretty

-- | lawl
-- >>> 10
-- 10
main :: IO ()
main = do
  putStrLn "~~~"
  putStrLn "Write Yourself a Scheme!"
  putStrLn "~~~"
  putStrLn "I guess this is a REPL, huh?"
  forever repl

repl :: IO ()
repl = do
  putStr "::| "
  hFlush stdout
  str <- getLine
  let e = parseLisp lispExpr str
  print e
  case e of
       Right p -> print (pretty p)
       _ -> return  ()
