module ParserSpec where

import Test.QuickCheck
import Test.Hspec
import Text.ParserCombinators.Parsec
import Numeric
import Data.Either

import Parser


testParser :: Parser a -> String -> Either ParseError a
testParser p = runParser p () ""

mkFloatStr :: (Show a, Num a) => a -> String
mkFloatStr i = show i ++ "." ++ show i

spec :: Spec
spec = do
  describe "parseString" $ do
    it "fails on bare words" $
      testParser parseString "asdf" `shouldSatisfy` isLeft

    it "requires quotes" $
      testParser parseString "\"asdf\"" `shouldBe` Right (String "asdf")

    it "is ok with special chars" $
      testParser parseString "\"asdf\t\t\"" `shouldBe` Right (String "asdf\t\t")

  describe "parseCharacter" $ do
    it "fails if missing hash" $
      testParser parseCharacter "a" `shouldSatisfy` isLeft

    it "fails if missing a backslash" $
      testParser parseCharacter "#a" `shouldSatisfy` isLeft

    it "succeeds if given a single character with hash and backslash" $ property $ \c ->
      testParser parseCharacter ("#\\" ++ [c]) `shouldBe` Right (Character c)

    it "succeeds on space" $
      testParser parseCharacter "#\\space" `shouldBe` Right (Character ' ')

    it "succeeds on newline" $
      testParser parseCharacter "#\\newline" `shouldBe` Right (Character '\n')

  describe "parseFloat" $ do
    it "does not read an int" $ property $ \x -> 
      testParser parseFloat (show (x :: Int)) `shouldSatisfy` isLeft

    it "parses floats" $ property $ \x ->
      testParser parseFloat (showFFloat Nothing x "")
        `shouldBe`
          Right (Float x)

    it "handles a +" $ property $ \(Positive x) -> 
      testParser parseFloat ('+' : mkFloatStr x) 
        `shouldBe` 
          Right (Float (fromInteger x + read ("0." ++ show x)))

    it "handles a -" $ property $ \(Positive x) ->
      testParser parseFloat ('-' : show (x :: Integer) ++ "." ++ show x) 
        `shouldBe` 
          Right (Float . negate $ (fromInteger x + read ("0." ++ show x)))
  
  describe "parseList" $ do
    it "reads a list of chars ok" $
      testParser parseList "a b c d" `shouldBe` Right (List (map (Atom . pure) "abcd"))

    it "reads a list of different types" $
      testParser parseList "3 3.2 a \"asdf\" #\\a"
        `shouldBe`
          Right (List [Number 3, Float 3.2, Atom "a", String "asdf", Character 'a'])

    it "handles parens like a champ" $
      testParser parseList "1 (2 (3 (4)))"
        `shouldBe`
          Right (List [Number 1, List [Number 2, List [Number 3, List [Number 4]]]])

  describe "parseInt" $ do
    it "parses ints just fine" $ property $ \x -> 
      testParser parseInt (show x) `shouldBe` Right (Number x)

    it "parses binary format" $ do
      testParser parseInt "#b0001" `shouldBe` Right (Number 1)
      testParser parseInt "#b0010" `shouldBe` Right (Number 2)
      testParser parseInt "#b0011" `shouldBe` Right (Number 3)

    it "parses hex format" $ property $ \(NonNegative x) ->
      testParser parseInt ("#x" ++ showHex x "") `shouldBe` Right (Number x)

    it "parses octal format" $ property $ \(NonNegative x) ->
      testParser parseInt ("#o" ++ showOct x "") `shouldBe` Right (Number x)

    it "parses regular also" $ property $ \(NonNegative x) ->
      testParser parseInt ("#d" ++ show x) `shouldBe` Right (Number x)
