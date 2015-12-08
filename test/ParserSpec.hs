module ParserSpec where

import Test.QuickCheck
import Test.Hspec
import Text.ParserCombinators.Parsec hiding (string)
import Numeric
import Data.Either

import Wyas

testParser :: Parser a -> String -> Either ParseError a
testParser p = runParser p () ""

mkFloatStr :: (Show a, Num a) => a -> String
mkFloatStr i = show i ++ "." ++ show i

spec :: Spec
spec = do
  describe "string" $ do
    it "fails on bare words" $
      testParser string "asdf" `shouldSatisfy` isLeft

    it "requires quotes" $
      testParser string "\"asdf\"" `shouldBe` Right (String "asdf")

    it "is ok with special chars" $
      testParser string "\"asdf\t\t\"" `shouldBe` Right (String "asdf\t\t")

  describe "character" $ do
    it "fails if missing hash" $
      testParser character "a" `shouldSatisfy` isLeft

    it "fails if missing a backslash" $
      testParser character "#a" `shouldSatisfy` isLeft

    it "does not work on #t or #f" $ do
      testParser character "#t" `shouldSatisfy` isLeft
      testParser character "#f" `shouldSatisfy` isLeft

    it "succeeds if given a single character with hash and backslash" $ property $ \c ->
      testParser character ("#\\" ++ [c]) `shouldBe` Right (Character c)

    it "succeeds on space" $
      testParser character "#\\space" `shouldBe` Right (Character ' ')

    it "succeeds on newline" $
      testParser character "#\\newline" `shouldBe` Right (Character '\n')

  describe "float" $ do
    it "does not read an int" $ property $ \x -> 
      testParser float (show (x :: Int)) `shouldSatisfy` isLeft

    it "parses floats" $ property $ \x ->
      testParser float (showFFloat Nothing x "")
        `shouldBe`
          Right (Float x)

    it "handles a +" $ property $ \(Positive x) -> 
      testParser float ('+' : mkFloatStr x) 
        `shouldBe` 
          Right (Float (fromInteger x + read ("0." ++ show x)))

    it "handles a -" $ property $ \(Positive x) ->
      testParser float ('-' : show (x :: Integer) ++ "." ++ show x) 
        `shouldBe` 
          Right (Float . negate $ (fromInteger x + read ("0." ++ show x)))
  
  describe "atom" $ do
    it "should handle #t" pending
    it "should handle #f" pending
    it "should handle other symbols" pending

  describe "list" $ do
    it "reads a list of chars ok" $
      testParser list "a b c d" `shouldBe` Right (List (map (Atom . pure) "abcd"))

    it "reads a list of different types" $
      testParser list "3 3.2 a \"asdf\" #\\a"
        `shouldBe`
          Right (List [Number 3, Float 3.2, Atom "a", String "asdf", Character 'a'])

    it "handles char and true/false" $
      testParser list "#t #\\a #f #\\newline"
        `shouldBe`
          Right (List [Bool True, Character 'a', Bool False, Character '\n'])

    it "handles parens like a champ" $
      testParser list "1 (2 (3 (4)))"
        `shouldBe`
          Right (List [Number 1, List [Number 2, List [Number 3, List [Number 4]]]])

  describe "int" $ do
    it "parses ints just fine" $ property $ \x -> 
      testParser int (show x) `shouldBe` Right (Number x)

    it "parses binary format" $ do
      testParser int "#b0001" `shouldBe` Right (Number 1)
      testParser int "#b0010" `shouldBe` Right (Number 2)
      testParser int "#b0011" `shouldBe` Right (Number 3)

    it "parses hex format" $ property $ \(NonNegative x) ->
      testParser int ("#x" ++ showHex x "") `shouldBe` Right (Number x)

    it "parses octal format" $ property $ \(NonNegative x) ->
      testParser int ("#o" ++ showOct x "") `shouldBe` Right (Number x)

    it "parses regular also" $ property $ \(NonNegative x) ->
      testParser int ("#d" ++ show x) `shouldBe` Right (Number x)
