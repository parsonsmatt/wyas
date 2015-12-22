module LolEvalSpec where

import Control.Exception (evaluate)
import Test.QuickCheck
import Test.Hspec

import Wyas
import LolEval

spec :: Spec
spec = do
  describe "Basic Values" $ do
    it "String returns itself" $
      eval (String "asdf") `shouldBe` String "asdf"
    it "Number returns itself" $
      eval (Number 10) `shouldBe` Number 10
    it "Bool returns itself" $
      eval (Bool True) `shouldBe` Bool True
  describe "Quoted values" $ do
    it "Returns the quoted item" $
      eval (List [ Atom "quote", String "asdf" ]) `shouldBe` String "asdf"
  describe "Numeric Functions" $ do
    describe "+" $ do
      it "Should add two numbers" $ property $ \x y ->
        eval (List [ Atom "+", Number y, Number x ]) `shouldBe` Number (x + y)
      it "Should accept a list of numbers" $ property $ \(NonEmpty xs) ->
        eval (List (Atom "+" : map Number xs)) `shouldBe` Number (sum xs)
    describe "-" $ do
      it "Should subtract two numbers" $ property $ \x y ->
        eval (List [ Atom "-", Number x, Number y ]) `shouldBe` Number (x - y)
