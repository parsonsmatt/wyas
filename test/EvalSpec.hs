module EvalSpec where

import Test.QuickCheck
import Test.Hspec

import Wyas
import Wyas.Eval
import Wyas.Parser (parseFile)

evalFile f = do
    e <- parseFile f
    case e of
         Left a -> fail (show a)
         Right a -> return (bareEval (evalD a))

spec :: Spec
spec = do
    describe "parseAndEval" $ do
        it "basic values return themselves" $ do
            parseAndEval "10" `shouldBe` Right (Number 10)
            parseAndEval "#t" `shouldBe` Right (Bool True)
            parseAndEval "\"asdfasdf\"" `shouldBe` Right (String "asdfasdf")
        it "unquotes a quoted value" $
            parseAndEval "'(a b c)" `shouldBe` Right (List [Atom "a", Atom "b", Atom "c"])
        it "handles let variable assignmnet" $
            parseAndEval "(let x 5 (+ x x))" `shouldBe` Right (Number 10)
        it "handles lambdas" $
            parseAndEval "(let sq (lambda (x) (* x x)) (sq 4))" `shouldBe` Right (Number 16)
    describe "file examples" $ do
        it "map.wyas" $ do
            pending
            r <- evalFile "test/Examples/map.wyas" 
            r `shouldBe` Right (Bool True)
