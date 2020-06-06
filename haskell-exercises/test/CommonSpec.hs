module CommonSpec (spec) where

import           Common.Fibonacci
import           Test.Hspec

spec :: Spec
spec =
  describe "Common Library Functions" $
    describe "Fibonacci" $ do
    it "can generate the first 12 terms of the fibonacci sequence" $ do
      let expected = [0 , 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89]
          inputs = [0 .. 11]
      fmap fibFast inputs `shouldBe` expected
      fmap fibSlow inputs `shouldBe` expected
    it "returns any given negative number" $ do
      fibFast (-100) `shouldBe` (-100)
      fibSlow (-666) `shouldBe` (-666)
