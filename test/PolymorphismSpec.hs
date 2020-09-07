module PolymorphismSpec where

import Test.Hspec
import Polymorphism

spec :: Spec
spec = do
    describe "elemInt" $ do
        it "Empty array" $ elemInt 0 [] `shouldBe` False
        it "With error in empty" $ elemInt (error "Nothing") [] `shouldBe` False
        it "Not found" $ elemInt 3 [10, 20, 30] `shouldBe` False
        it "Should found" $ elemInt 3 [1, 2, 3] `shouldBe` True

