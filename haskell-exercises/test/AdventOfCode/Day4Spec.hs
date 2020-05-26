{-# LANGUAGE OverloadedStrings #-}

module AdventOfCode.Day4Spec (spec) where

import           AdventOfCode.Day4              ( meetsCriteria
                                                , meetsCriteria'
                                                , toDigits
                                                , part1Solution
                                                , part2Solution
                                                , readRange
                                                )
import           Test.Hspec

spec :: Spec
spec = describe "AOC Day 4" $ do

  describe "Part 1 toDigits function" $
    it "turns a number into a list of digits." $ do
      toDigits 1 `shouldBe` [1]
      toDigits 123 `shouldBe` [1, 2, 3]

  describe "Part 1 meetsCriteria function" $ do
    it "returns False for numbers that do not meet criteria." $ do
      meetsCriteria 1 `shouldBe` False -- does not have an adjacent same pair
      meetsCriteria 110 `shouldBe` False -- 0 is decreasing
      meetsCriteria 1213 `shouldBe` False -- does not have an adjacent same pair
    it "returns true if the given number meets the criteria." $ do
      meetsCriteria 11 `shouldBe` True
      meetsCriteria 22 `shouldBe` True
      meetsCriteria 112 `shouldBe` True
      meetsCriteria 11233 `shouldBe` True
      meetsCriteria 12344 `shouldBe` True

  describe "Part 2 meetsCriteria' function" $ do
    it "returns False for numbers that do not meet criteria." $ do
      meetsCriteria' 1 `shouldBe` False -- does not have an adjacent same pair
      meetsCriteria' 110 `shouldBe` False -- 0 is decreasing
      meetsCriteria' 1213 `shouldBe` False -- does not have an adjacent same pair
      meetsCriteria' 123444 `shouldBe` False -- pair is not alone
      meetsCriteria' 444555 `shouldBe` False -- pair is not alone
      meetsCriteria' 4444 `shouldBe` False -- pair is not alone
    it "returns true if the given number meets the criteria." $ do
      meetsCriteria' 11 `shouldBe` True
      meetsCriteria' 22 `shouldBe` True
      meetsCriteria' 112 `shouldBe` True
      meetsCriteria' 11233 `shouldBe` True
      meetsCriteria' 12344 `shouldBe` True
      meetsCriteria' 112233 `shouldBe` True
      meetsCriteria' 111122 `shouldBe` True
      meetsCriteria' 223333 `shouldBe` True

  describe "Can get solutions" $
    it "Can get solution for part 1 and 2" $ do
      range <- readRange
      part1Solution range `shouldBe` 2090
      part2Solution range `shouldBe` 1419
