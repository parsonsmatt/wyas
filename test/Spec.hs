module Main where

import Test.Hspec
import ParserSpec

main :: IO ()
main = hspec $ do
    parserSpec
