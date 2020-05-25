{-# LANGUAGE OverloadedStrings #-}

module AdventOfCode.Day1Spec (spec) where

import           Test.Hspec
import           AdventOfCode.Day1              ( calcFuel
                                                , calcModuleFuel
                                                , getModuleMasses
                                                , part1Solution
                                                , part2Solution
                                                )

spec :: Spec
spec =
  describe "AOC Day 1" $ do

    describe "Part 1 Calc Fuel function" $
      it "should do basic calculations" $ do
        calcFuel 12 `shouldBe` 2
        calcFuel 14 `shouldBe` 2
        calcFuel 1969 `shouldBe` 654
        calcFuel 100756 `shouldBe` 33583

    describe "Part 2 Calc Fuel function" $
      it "should do recursive calculations for the fuel of the fuel" $ do
        calcModuleFuel 12 `shouldBe` 2
        calcModuleFuel 14 `shouldBe` 2
        calcModuleFuel 1969 `shouldBe` 966
        calcModuleFuel 100756 `shouldBe` 50346

    describe "Solution" $ do
      it "can solve part 1 and 2" $ do
        moduleMasses <- getModuleMasses
        part1Solution moduleMasses `shouldBe` 3380880
        part2Solution moduleMasses `shouldBe` 5068454
