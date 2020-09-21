{-# LANGUAGE OverloadedStrings #-}

module ProjectEuler.ProblemsSpec (spec) where

import qualified ProjectEuler.Problems         as P
import           Test.Hspec

spec :: Spec
spec =
  describe "Project Euler Problems" $ do

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

    describe "Problem 3" $
      it "should be the expected answer" $
      P.sol3 `shouldBe` Just 6857

    describe "Problem 4" $
      it "should be the expected answer" $
      P.sol4 `shouldBe` Just 906609

    describe "Problem 5" $
      it "should be the expected answer" $
      -- not including the actual answer here because it's slow to compute with
      -- my brute force approach.
      P.sol5 (10 :: Int) `shouldBe` Just 2520

    describe "Problem 6" $
      it "should be the expected answer" $ do
      P.sol6 (10 :: Int) `shouldBe` 2640
      P.sol6 (100 :: Int) `shouldBe` 25164150

    describe "Problem 7" $
      it "should be the expected answer" $ do
      P.sol7 (6 :: Int) `shouldBe` Just 13
      P.sol7 (10001 :: Int) `shouldBe` Just 104743
