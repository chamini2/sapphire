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
    describe "block" $ do
        it "should parse an empty block" $ do
            let res = parseStatement . scanTokens $
                    "begin end"
            res `shouldBe` SappStmtBlock []

        it "should parse nested blocks" $ do
            let res = parseStatement . scanTokens $
                    "begin\n" ++
                    "  begin\n" ++
                    "    begin end;" ++
                    "  end;\n" ++
                    "end"
            res `shouldBe` SappStmtBlock [ SappStmtBlock [ SappStmtBlock [] ] ]

        it "should parse multiple statements" $ do
            let res = parseStatement . scanTokens $
                    "begin\n" ++
                    "  write \"Hello world\";\n" ++
                    "  write \"Hello again!\";\n" ++
                    "end"
            res `shouldSatisfy` (\res -> case res of
                    SappStmtBlock stms -> length stms == 2
                    _ -> False
                )

        it "should reject statements without an ending ';'" $ do
            let res = parseStatement . scanTokens $
                    "begin\n" ++
                    "  write \"Hello world\"\n" ++
                    "end"
            evaluate res `shouldThrow` anyErrorCall

    describe "read" $ do
        it "should parse a read for an identifier" $ do
            let res = parseStatement . scanTokens $
                    "read num"
            res `shouldBe` SappStmtRead (SappVar "num")

        it "should reject a read for a literal" $ do
            let res = parseStatement . scanTokens $
                    "read 3"
            evaluate res `shouldThrow` anyErrorCall

        it "should reject a read for multiple identifiers" $ do
            let res = parseStatement . scanTokens $
                    "read num, nam"
            evaluate res `shouldThrow` anyErrorCall

    describe "write" $ do
        it "should accept a string" $ do
            let res = parseStatement . scanTokens $
                    "write \"Hello world\""
            res `shouldBe` SappStmtWrite [Left "Hello world"]
        it "should accept multiple strings" $ do
            let res = parseStatement . scanTokens $
                    "write \"Hello world\", \"Hello again!\""
            res `shouldBe` SappStmtWrite [Left "Hello world", Left "Hello again!"]

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
                "main\n" ++
                "  write \"Hello world\";\n" ++
                "  write \"Hello again!\";\n" ++
                "end"
        res `shouldSatisfy` (\res -> case res of
                SappStmtBlock stms -> length stms == 2
                _ -> False
            )
