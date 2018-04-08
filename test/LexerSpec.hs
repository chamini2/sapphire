module LexerSpec
    ( lexer
    ) where

import Control.Exception
import Test.Hspec

import Lexer

lexer = describe "lexer" $ do
    skippedChars

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
