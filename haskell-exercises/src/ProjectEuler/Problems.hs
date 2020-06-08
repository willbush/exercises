module ProjectEuler.Problems where

import           Common.Fibonacci               ( fibFast )
import           Common.Primes                  ( primeFactors )
import qualified Common.Helpers                as H
import           Safe                           ( maximumMay )

-- | Given an number sum the natural numbers that are multiples of 3 or 5 up to,
-- but not including the given number.
--
-- Problem 1 - Multiples of 3 and 5:
-- "If we list all the natural numbers below 10 that are multiples of 3 or 5, we
-- get 3, 5, 6 and 9. The sum of these multiples is 23. Find the sum of all the
-- multiples of 3 or 5 below 1000."
sol1 :: Int -> Int
sol1 n = sum [ x | x <- [1 .. (n - 1)], x `mod` 3 == 0 || x `mod` 5 == 0 ]

-- | Problem 2 - Even Fibonacci numbers:
-- Each new term in the Fibonacci sequence is generated by adding the previous
-- two terms. By starting with 1 and 2, the first 10 terms will be:
--
-- 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
--
-- By considering the terms in the Fibonacci sequence whose values do not exceed
-- four million, find the sum of the even-valued terms.
sol2 :: Integer
sol2 = sum $ takeWhile (< 4000000) $ filter even $ fmap fibFast [0 ..]

-- | Problem 3 - Largest prime factor:
-- The prime factors of 13195 are 5, 7, 13 and 29.
-- What is the largest prime factor of the number 600851475143 ?
sol3 :: Maybe Int
sol3 = maximumMay $ primeFactors 600851475143

-- | Problem 4 - Largest palindrome product:
-- A palindromic number reads the same both ways. The largest palindrome made
-- from the product of two 2-digit numbers is 9009 = 91 × 99. Find the largest
-- palindrome made from the product of two 3-digit numbers.
sol4 :: Maybe Int
sol4 = maximumMay $ filter
  (\n -> n == H.reverseInt n)
  [ x * y | x <- [100 .. 999], y <- [100 .. x] ]
