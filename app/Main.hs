module Main where

import System.IO
import Control.Monad
import Wyas
import Wyas.Eval
import Wyas.Pretty
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  unless (null args) $ do
    loaded <- readFile (head args)
    print loaded
    print $ parseAndEval loaded
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
       Right p -> do
         print . pretty $ p
         case bareEval $ evalD p of
              Right e -> print . pretty $ e
              Left e -> putStrLn $ "eval failure: " ++ show e
       _ -> return  ()
