{-# LANGUAGE OverloadedStrings #-}

module AdventOfCode.Day2Spec (spec) where

import           AdventOfCode.Day2              ( runProgram
                                                , part1Solution
                                                , part2Solution
                                                , readProgram
                                                )
import           Test.Hspec

spec :: Spec
spec =
  describe "AOC Day 2" $ do
    describe "Part 1 runProgram function" $ do

      it "should run simple programs" $ do
        runProgram [1, 0, 0, 0, 99] `shouldBe` [2, 0, 0, 0, 99]
        runProgram [2, 3, 0, 3, 99] `shouldBe` [2, 3, 0, 6, 99]
        runProgram [2, 4, 4, 5, 99, 0] `shouldBe` [2, 4, 4, 5, 99, 9801]
        runProgram [1, 1, 1, 4, 99, 5, 6, 0, 99]
          `shouldBe` [30, 1, 1, 4, 2, 5, 6, 0, 99]

      it "should halt when 99 is reached" $ do
        let doNothingProgram = [99, 0, 0, 0, 1, 0, 0, 0]
        runProgram doNothingProgram `shouldBe` doNothingProgram

    describe "Solution" $
      it "can solve part 1 and 2" $ do
        program <- readProgram
        part1Solution program `shouldBe` Just 9581917
        part2Solution program `shouldBe` Just 2505
