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
    describe "literal integer" $ do
        it "should return an integer expression" $ do
            let res = parseExpression . scanTokens $
                    "123"
            res `shouldBe` SappExpLitInteger 123

    describe "literal boolean" $ do
        it "should return a True boolean expression" $ do
            let res = parseExpression . scanTokens $
                    "true"
            res `shouldBe` SappExpLitBoolean True

        it "should return a False boolean expression" $ do
            let res = parseExpression . scanTokens $
                    "false"
            res `shouldBe` SappExpLitBoolean False

    describe "variable" $ do
        it "should parse a variable as an expression" $ do
            let res = parseExpression . scanTokens $
                    "flag"
            res `shouldBe` SappExpVariable (SappVar "flag")

    describe "operators" $ do
        describe "addition" $ do
            it "should parse an addition as an expression" $ do
                let res = parseExpression . scanTokens $
                        "+ left right"
                res `shouldBe` SappExpAddition (SappExpVariable (SappVar "left")) (SappExpVariable (SappVar "right"))

        describe "substraction" $ do
            it "should parse a substraction as an expression" $ do
                let res = parseExpression . scanTokens $
                        "- left right"
                res `shouldBe` SappExpSubstraction (SappExpVariable (SappVar "left")) (SappExpVariable (SappVar "right"))

        describe "multiplication" $ do
            it "should parse a multiplication as an expression" $ do
                let res = parseExpression . scanTokens $
                        "* left right"
                res `shouldBe` SappExpMultiplication (SappExpVariable (SappVar "left")) (SappExpVariable (SappVar "right"))

        describe "division" $ do
            it "should parse a division as an expression" $ do
                let res = parseExpression . scanTokens $
                        "/ left right"
                res `shouldBe` SappExpDivision (SappExpVariable (SappVar "left")) (SappExpVariable (SappVar "right"))

        describe "modulo" $ do
            it "should parse a modulo as an expression" $ do
                let res = parseExpression . scanTokens $
                        "% left right"
                res `shouldBe` SappExpModulo (SappExpVariable (SappVar "left")) (SappExpVariable (SappVar "right"))

        describe "exponentiation" $ do
            it "should parse an exponentiation as an expression" $ do
                let res = parseExpression . scanTokens $
                        "^ left right"
                res `shouldBe` SappExpExponentiation (SappExpVariable (SappVar "left")) (SappExpVariable (SappVar "right"))

        describe "conjuction" $ do
            it "should parse a conjuction as an expression" $ do
                let res = parseExpression . scanTokens $
                        "or left right"
                res `shouldBe` SappExpConjuction (SappExpVariable (SappVar "left")) (SappExpVariable (SappVar "right"))

        describe "disjunction" $ do
            it "should parse a disjunction as an expression" $ do
                let res = parseExpression . scanTokens $
                        "and left right"
                res `shouldBe` SappExpDisjunction (SappExpVariable (SappVar "left")) (SappExpVariable (SappVar "right"))

        describe "negation" $ do
            it "should parse a negation as an expression" $ do
                let res = parseExpression . scanTokens $
                        "not val"
                res `shouldBe` SappExpNegation (SappExpVariable (SappVar "val"))


        describe "equals to" $ do
            it "should parse an equals to as an expression" $ do
                let res = parseExpression . scanTokens $
                        "= left right"
                res `shouldBe` SappExpEqualsTo (SappExpVariable (SappVar "left")) (SappExpVariable (SappVar "right"))

        describe "different from" $ do
            it "should parse an different from as an expression" $ do
                let res = parseExpression . scanTokens $
                        "/= left right"
                res `shouldBe` SappExpDifferentFrom (SappExpVariable (SappVar "left")) (SappExpVariable (SappVar "right"))

        describe "greater than" $ do
            it "should parse an greater than as an expression" $ do
                let res = parseExpression . scanTokens $
                        "> left right"
                res `shouldBe` SappExpGreaterThan (SappExpVariable (SappVar "left")) (SappExpVariable (SappVar "right"))

        describe "greater than or equal to" $ do
            it "should parse an greater than or equal to as an expression" $ do
                let res = parseExpression . scanTokens $
                        ">= left right"
                res `shouldBe` SappExpGreaterThanOrEqualTo (SappExpVariable (SappVar "left")) (SappExpVariable (SappVar "right"))

        describe "less than" $ do
            it "should parse an less than as an expression" $ do
                let res = parseExpression . scanTokens $
                        "< left right"
                res `shouldBe` SappExpLessThan (SappExpVariable (SappVar "left")) (SappExpVariable (SappVar "right"))

        describe "less than or equal to" $ do
            it "should parse an less than or equal to as an expression" $ do
                let res = parseExpression . scanTokens $
                        "<= left right"
                res `shouldBe` SappExpLessThanOrEqualto (SappExpVariable (SappVar "left")) (SappExpVariable (SappVar "right"))



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

    describe "variable declaration" $ do
        it "should parse an integer variable declaration" $ do
            let res = parseStatement . scanTokens $
                    "integer num"
            res `shouldBe` SappStmtVariableDeclaration SappDTInteger "num"

        it "should parse a boolean variable declaration" $ do
            let res = parseStatement . scanTokens $
                    "boolean flag"
            res `shouldBe` SappStmtVariableDeclaration SappDTBoolean "flag"

        it "should reject a variable declaration with multiple variables" $ do
            let res = parseStatement . scanTokens $
                    "integer year, height, width"
            evaluate res `shouldThrow` anyErrorCall

    describe "assignment" $ do
        it "should parse an assignment" $ do
            let res = parseStatement . scanTokens $
                    "num := 42"
            res `shouldBe` SappStmtAssignment (SappVar "num") (SappExpLitInteger 42)

        it "should parse a boolean variable declaration" $ do
            let res = parseStatement . scanTokens $
                    "boolean flag"
            res `shouldBe` SappStmtVariableDeclaration SappDTBoolean "flag"

        it "should reject a variable declaration with multiple variables" $ do
            let res = parseStatement . scanTokens $
                    "integer year, height, width"
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

    describe "if" $ do
        it "should accept an if-then statement" $ do
            let res = parseStatement . scanTokens $
                    "if true\n" ++
                    "then write \"Hello world\""
            res `shouldSatisfy` (\res -> case res of
                    SappStmtIf expr tStmt Nothing -> True
                    _ -> False
                )

        it "should accept an if-then-else statement" $ do
            let res = parseStatement . scanTokens $
                    "if true\n" ++
                    "then write \"Hello world\"\n" ++
                    "else write \"Goodbye world\""
            res `shouldSatisfy` (\res -> case res of
                    SappStmtIf expr tStmt (Just eStmt) -> True
                    _ -> False
                )

        it "should parse if-then-if-then-else case as if-then(if-then-else)" $ do
            let res = parseStatement . scanTokens $
                    "if true\n" ++
                    "then if false\n" ++
                        "then write \"Hello world\"\n" ++
                        "else write \"Goodbye world\""
            res `shouldSatisfy` (\res -> case res of
                    SappStmtIf oExpr (SappStmtIf iExpr tStmt (Just eStmt)) Nothing -> True
                    _ -> False
                )


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
