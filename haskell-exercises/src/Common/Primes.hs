-- {-# LANGUAGE Strict #-}

module Common.Primes where

primeFactors :: Int -> [Int]
primeFactors = go 2
 where
  go :: Int -> Int -> [Int]
  go _ 1 = []
  go d n | d * d > n      = [n]
         | n `rem` d == 0 = d : go d (n `quot` d)
         | d == 2         = go (d + 1) n
         | otherwise      = go (d + 2) n
