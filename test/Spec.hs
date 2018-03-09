import Lib
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "someFunc" $ do
        it "return  ()" $ do
            res <- someFunc
            shouldBe res ()
