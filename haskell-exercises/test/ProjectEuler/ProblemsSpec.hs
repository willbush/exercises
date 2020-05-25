{-# LANGUAGE OverloadedStrings #-}

module ProjectEuler.ProblemsSpec (spec) where

import qualified ProjectEuler.Problems         as P
import           Test.Hspec

spec :: Spec
spec =
  describe "Project Euler Problems" $
    describe "Problem 1" $ do

    it "sums natural numbers below n that are multiples of 3 or 5 " $ do
      P.sol1 10 `shouldBe` 23
      P.sol1 1000 `shouldBe` 233168
    it "returns for 0 for negative inputs" $ do
      P.sol1 (-1) `shouldBe` 0
      P.sol1 (-9) `shouldBe` 0

    describe "Problem 2" $
      it "should be the expected answer" $
      P.sol2 `shouldBe` 4613732
