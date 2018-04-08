module LexerSpec
    ( lexer
    ) where

import Control.Exception
import Test.Hspec

import Lexer

lexer = describe "lexer" $ do
    skippedChars
    comments
    identifiers
    expressionLiterals

skippedChars = describe "skipped chars" $ do
    it "should skip spaces ' '" $ do
        let res = scanTokens $
                "    "
        res `shouldBe` []

    it "should skip tabs '\\t'" $ do
        let res = scanTokens $
                "\t\t\t\t"
        res `shouldBe` []

    it "should skip newlines '\\n'" $ do
        let res = scanTokens $
                "\n\n\n\n"
        res `shouldBe` []

comments = describe "end-of-line comments" $ do
    it "should ignore text after the symbol" $ do
        let res = scanTokens $
                "stuff # comment"
        length res `shouldBe` 1 -- the 'stuff' token

    it "should not ignore text on the next line" $ do
        let res = scanTokens $
                "stuff # comment\n" ++
                "more"
        length res `shouldBe` 2 -- the 'stuff' and 'more' tokens

identifiers = describe "identifiers" $ do
    it "should consume an all-letters identifier" $ do
        let res = scanTokens $
                "iden"
        res `shouldSatisfy` (\res -> case res of
                [TkIdentifier val _] -> val == "iden"
                _ -> False
            )

    it "should consume an underscore-starting identifier" $ do
        let res = scanTokens $
                "_iden"
        res `shouldSatisfy` (\res -> case res of
                [TkIdentifier val _] -> val == "_iden"
                _ -> False
            )

    it "should consume an identifier with numbers" $ do
        let res = scanTokens $
                "iden2"
        res `shouldSatisfy` (\res -> case res of
                [TkIdentifier val _] -> val == "iden2"
                _ -> False
            )

    it "should not consume a number-starting identifier" $ do
        let res = scanTokens $
                "1iden"
        res `shouldSatisfy` (\res -> case res of
                [_, TkIdentifier val _] -> val == "iden"
                _ -> False
            )

expressionLiterals = describe "expression literals" $ do
    charStrings
    integers
    booleans
  where
    charStrings = describe "character strings" $ do
        it "should consume the empty string" $ do
            let res = scanTokens $
                    "\"\""
            res `shouldSatisfy` (\res -> case res of
                    [TkLitCharString val _] -> val == ""
                    _ -> False
                )

        it "should consume a string with backspaced parts" $ do
            let res = scanTokens $
                    "\" \\\" \\t \\n \\\\ \""
            res `shouldSatisfy` (\res -> case res of
                    [TkLitCharString val _] -> val == " \" \t \n \\ "
                    _ -> False
                )

        it "should fail for non-closed strings" $ do
            let res = scanTokens $
                    "\"Hello world!"
            evaluate res `shouldThrow` anyErrorCall

        it "should fail for multiple-line strings" $ do
            let res = scanTokens $
                    "\"This is my\n" ++
                    "Haiku.\n" ++
                    "This is how they work,\n" ++
                    "right?\""
            evaluate res `shouldThrow` anyErrorCall

    integers = describe "integers" $ do
        it "should consume integer numbers" $ do
            let res = scanTokens $
                    "123"
            res `shouldSatisfy` (\res -> case res of
                    [TkLitInteger val _] -> val == 123
                    _ -> False
                )

        it "should accept integers that start with 0" $ do
            let res = scanTokens $
                    "0123"
            res `shouldSatisfy` (\res -> case res of
                    [TkLitInteger val _] -> val == 123
                    _ -> False
                )

    booleans = describe "booleans" $ do
        it "should consume 'true'" $ do
            let res = scanTokens $
                    "true"
            res `shouldSatisfy` (\res -> case res of
                    [TkLitBoolean val _] -> val == True
                    _ -> False
                )

        it "should consume 'false'" $ do
            let res = scanTokens $
                    "false"
            res `shouldSatisfy` (\res -> case res of
                    [TkLitBoolean val _] -> val == False
                    _ -> False
                )
