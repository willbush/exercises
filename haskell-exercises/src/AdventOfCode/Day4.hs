{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

-- | Advent of Code 2019 day 4 solution.
module AdventOfCode.Day4 (run, meetsCriteria, meetsCriteria', toDigits) where

import qualified Data.ByteString.Char8         as B
import           Data.Maybe                     ( mapMaybe )

run :: IO ()
run = do
  text <- B.readFile "../inputs/aoc/2019/input-day4.txt"
  let rangeValues = fmap fst $ mapMaybe B.readInt $ B.split '-' text

  case rangeValues of
    [begin, end] -> do
      putStrLn "== Day 4 =="
      putStrLn "Part 1"
      let nums = [begin .. end]
      print $ length $ filter meetsCriteria nums
      putStrLn "Part 2"
      print $ length $ filter meetsCriteria' nums
    _ -> putStrLn "Expected input to have two ranges."

-- | part 1 criteria returns true if the given number contains an adjacent pair
-- and each consecutive value is >= to the next.
meetsCriteria :: Int -> Bool
meetsCriteria n = go False (toDigits n)
 where
  go hasSamePair xs@(x : y : _)
    | y < x     = False
    | otherwise = go (hasSamePair || x == y) $ tail xs
  go hasSamePair _ = hasSamePair

data PairState = NoPair | MaybePair | CrowdedPair | LonePair

-- | part 2 criteria returns true if following conditions are meet:
--
--   - each consecutive value is >= to the next
--   - the given number contains a lone pair.
--
--   A lone pair are a pair of digits with the same value, but the pair is alone
--   meaning the values next to them are different.
meetsCriteria' :: Int -> Bool
meetsCriteria' n = go NoPair (toDigits n)
 where
  go :: PairState -> [Int] -> Bool
  go s xs@(x : y : _) =
    let xs' = tail xs
    in  y >= x && case s of
          NoPair      -> go (if x == y then MaybePair else NoPair) xs'
          MaybePair   -> go (if x == y then CrowdedPair else LonePair) xs'
          CrowdedPair -> go (if x == y then CrowdedPair else NoPair) xs'
          LonePair    -> go LonePair xs'
  go LonePair  _ = True
  go MaybePair _ = True
  go _         _ = False

toDigits :: Int -> [Int]
toDigits n = go n []
 where
  go x acc | x < 10    = x : acc
           | otherwise = go (x `div` 10) (x `mod` 10 : acc)
