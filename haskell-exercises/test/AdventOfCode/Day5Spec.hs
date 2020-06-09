{-# LANGUAGE OverloadedStrings #-}

module AdventOfCode.Day5Spec (spec) where

import           AdventOfCode.Day5              ( readProgram
                                                , runProgram
                                                )
import           Test.Hspec

spec :: Spec
spec = describe "AOC Day 5" $ do
  describe "Part 1 runProgram function" $ do
    it "can return nothing" $ do
      runProgram 1 [99] `shouldBe` []
      runProgram 1 [99, 0, 0, 0, 1, 0, 0, 0] `shouldBe` []
      runProgram 1 [] `shouldBe` []
    it "can add and output" $ do
      runProgram 1 [1, 0, 0, 0, 4, 0, 99] `shouldBe` [2]
      runProgram 1 [1101, 2, 2, 0, 4, 0, 99] `shouldBe` [4]
      runProgram 1 [1101, 100, -1, 0, 4, 0, 99] `shouldBe` [99]
    it "can multiply and output" $ do
      runProgram 1 [2, 0, 0, 0, 4, 0, 99] `shouldBe` [4]
      runProgram 1 [0002, 0, 0, 0, 4, 0, 99] `shouldBe` [4]
      runProgram 1 [1102, 4, 4, 0, 4, 0, 99] `shouldBe` [16]
    it "can output immediate values" $ runProgram 1 [104, 7, 99] `shouldBe` [7]
  describe "Part 2 runProgram function" $ do
    it "compares input is equal to 8 using position mode." $ do
      let program = [3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8]
      runProgram 7 program `shouldBe` [0]
      runProgram 2 program `shouldBe` [0]
      runProgram 8 program `shouldBe` [1]
    it "compares input is less than 8 using position mode." $ do
      let program = [3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8]
      runProgram 7 program `shouldBe` [1]
      runProgram 1 program `shouldBe` [1]
      runProgram 8 program `shouldBe` [0]
      runProgram 10 program `shouldBe` [0]
    it "compares input is equal to 8 using immediate mode." $ do
      let program = [3, 3, 1108, -1, 8, 3, 4, 3, 99]
      runProgram 7 program `shouldBe` [0]
      runProgram 8 program `shouldBe` [1]
    it "compares input is less than 8 using immediate mode." $ do
      let program = [3, 3, 1107, -1, 8, 3, 4, 3, 99]
      runProgram 7 program `shouldBe` [1]
      runProgram 8 program `shouldBe` [0]
    it "tests jump using position mode" $ do
      -- outputs 0 if input was 0 and 1 if input was non-zero
      let program = [3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9]
      runProgram 1 program `shouldBe` [1]
      runProgram 0 program `shouldBe` [0]
    it "tests jump using immediate mode" $ do
      -- outputs 0 if input was 0 and 1 if input was non-zero
      let program = [3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1]
      runProgram 123 program `shouldBe` [1]
      runProgram 0 program `shouldBe` [0]
    it "tests longer program" $ do
      -- The program will then output 999 if the input value is below 8,
      -- output 1000 if the input value is equal to 8, or output 1001 if the
      -- input value is greater than 8.
      let program =
            [ 3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8
            , 21, 20, 1006, 20, 31, 1106, 0, 36, 98, 0, 0
            , 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104, 999
            , 1105, 1, 46, 1101, 1000, 1, 20, 4, 20, 1105
            , 1, 46, 98, 99
            ]
      runProgram 8 program `shouldBe` [1000]
      runProgram 800 program `shouldBe` [1001]

  describe "can get solutions" $ do
    it "can get solution for part 1 and 2" $ do
      program <- readProgram
      runProgram 1 program `shouldBe` [0,0,0,0,0,0,0,0,0,9938601]
      runProgram 5 program `shouldBe` [4283952]
