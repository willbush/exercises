module CommonSpec (spec) where

import           Common.Fibonacci
import           Test.Hspec

spec :: Spec
spec =
  describe "Common Library Functions" $
    describe "Fibonacci" $ do
    it "can generate the first 12 terms of the fibonacci sequence" $ do
      fmap fibFast [0 .. 11] `shouldBe` [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89]
      fmap fibSlow [0 .. 11] `shouldBe` [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89]
    it "returns any given negative number" $ do
      fibFast (-100) `shouldBe` (-100)
      fibSlow (-666) `shouldBe` (-666)
