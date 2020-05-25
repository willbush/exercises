-- | Advent of Code 2019 day 1 solution.

module AdventOfCode.Day1 where

import qualified Data.ByteString.Lazy.Char8    as B
import           Data.Maybe                     ( mapMaybe )

type Mass = Int
type Fuel = Int

run :: IO ()
run = do
  text <- B.readFile "../inputs/aoc/2019/input-day1.txt"
  let moduleMasses = fmap fst $ mapMaybe B.readInt $ B.lines text
  putStrLn "== Day 1 =="
  putStrLn "Part 1:"
  print $ sum $ calcFuel <$> moduleMasses
  putStrLn "Part 2:"
  print $ sum $ calcModuleFuel <$> moduleMasses

calcModuleFuel :: Mass -> Fuel
calcModuleFuel = sum . takeWhile (> 0) . tail . iterate calcFuel

calcFuel :: Mass -> Fuel
calcFuel m = m `div` 3 - 2
