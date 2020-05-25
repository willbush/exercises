-- | Advent of Code 2019 day 1 solution.

module AdventOfCode.Day1 where

import qualified Data.ByteString.Lazy.Char8    as B
import           Data.Maybe                     ( mapMaybe )

type Mass = Int
type Fuel = Int

-- | read and parse module masses from input file.
readModuleMasses :: IO [Mass]
readModuleMasses = do
  text <- B.readFile "../inputs/aoc/2019/input-day1.txt"
  pure $ fmap fst $ mapMaybe B.readInt $ B.lines text

part1Solution :: [Mass] -> Fuel
part1Solution moduleMasses = sum $ calcFuel <$> moduleMasses

part2Solution :: [Mass] -> Fuel
part2Solution moduleMasses = sum $ calcModuleFuel <$> moduleMasses

calcModuleFuel :: Mass -> Fuel
calcModuleFuel = sum . takeWhile (> 0) . tail . iterate calcFuel

calcFuel :: Mass -> Fuel
calcFuel m = m `div` 3 - 2
