{-# LANGUAGE BangPatterns #-}

module Common.Helpers where

reverseInt :: Int -> Int
reverseInt n | n < 0     = -go 0 (-n)
             | otherwise = go 0 n
 where
  go :: Int -> Int -> Int
  go !acc 0 = acc
  go !acc x = let (q, r) = quotRem x 10 in go (acc * 10 + r) q
