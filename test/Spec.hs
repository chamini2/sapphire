import Test.Hspec

import LexerSpec
import ParserSpec

main :: IO ()
main = hspec $ do
    lexer
    parser
