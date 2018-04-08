module ParserSpec
    ( parser
    ) where

import Control.Exception
import Test.Hspec

import Parser
import Lexer

parser = describe "parser" $ do
    expression
    statement
    program

expression = describe "Expression_" $ do
    describe "integer" $ do
        it "should return an integer expression" $ do
            let res = parseExpression . scanTokens $
                    "123"
            res `shouldBe` SappExpLitInteger 123

    describe "boolean" $ do
        it "should return a True boolean expression" $ do
            let res = parseExpression . scanTokens $
                    "true"
            res `shouldBe` SappExpLitBoolean True

        it "should return a False boolean expression" $ do
            let res = parseExpression . scanTokens $
                    "false"
            res `shouldBe` SappExpLitBoolean False

statement = describe "Statement_" $ do
    describe "begin ... end" $ do
        it "" $
            pending

    describe "write" $ do
        it "should accept a string" $ do
            let res = parseStatement . scanTokens $
                    "write \"Hello world\""
            res `shouldBe` SappStmtWrite [Left "Hello world"]
        it "should accept multiple strings" $ do
            let res = parseStatement . scanTokens $
                    "write \"Hello world\\n\", \"Hello again!\""
            res `shouldBe` SappStmtWrite [Left "Hello world\n", Left "Hello again!"]

        it "should accept an expression" $ do
            let res = parseStatement . scanTokens $
                    "write 4"
            res `shouldBe` SappStmtWrite [Right (SappExpLitInteger 4)]

        it "should accept multiple expressions" $ do
            let res = parseStatement . scanTokens $
                    "write 4, true, 5"
            res `shouldBe` SappStmtWrite [Right (SappExpLitInteger 4), Right (SappExpLitBoolean True), Right (SappExpLitInteger 5)]

        it "should accept mixed expressions and strings" $ do
            let res = parseStatement . scanTokens $
                    "write \"A number:\", 6"
            res `shouldBe` SappStmtWrite [Left "A number:", Right (SappExpLitInteger 6)]


program = describe "Program_" $ do
    it "should parse an empty program" $ do
        let res = parseProgram . scanTokens $
                "main end"
        res `shouldBe` SappStmtBlock []

    it "should parse a program with statemets" $ do
        let res = parseProgram . scanTokens $
                "main write \"Hello\"; write \"world\"; end"
        res `shouldSatisfy` (\res -> case res of
                SappStmtBlock stms -> length stms == 2
                _ -> False
            )
