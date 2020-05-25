{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Test.Hspec

import qualified ProjectEuler.ProblemsSpec     as Euler
import qualified CommonSpec                    as Common
import qualified AdventOfCode.Day1Spec         as Day1
import qualified AdventOfCode.Day2Spec         as Day2
import qualified AdventOfCode.Day3Spec         as Day3
import qualified AdventOfCode.Day4Spec         as Day4
import qualified AdventOfCode.Day5Spec         as Day5

main :: IO ()
main = hspec $ do
  describe "Common Spec"        Common.spec
  describe "Project Euler Spec" Euler.spec
  describe "AOC 2019 Day1"      Day1.spec
  describe "AOC 2019 Day2"      Day2.spec
  describe "AOC 2019 Day3"      Day3.spec
  describe "AOC 2019 Day4"      Day4.spec
  describe "AOC 2019 Day5"      Day5.spec
