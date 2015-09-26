module ParserSpec where

import Test.QuickCheck
import Test.Hspec
import Text.ParserCombinators.Parsec

import Parser

parserSpec = do
    describe "Parser" $ do
        describe "parseString" $ do
            it "fails on bare words" $ do
                pending
