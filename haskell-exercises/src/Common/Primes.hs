module Common.Primes where

import           Control.Monad                  ( forM_
                                                , when
                                                )
import qualified Data.Array.ST                 as ST
import qualified Data.Array.Unboxed            as U

primeFactors :: Int -> [Int]
primeFactors = go 2
 where
  go :: Int -> Int -> [Int]
  go _ 1 = []
  go d n | d * d > n      = [n]
         | n `rem` d == 0 = d : go d (n `quot` d)
         | d == 2         = go (d + 1) n
         | otherwise      = go (d + 2) n

-- | Get primes numbers up to n.
primesTo :: Int -> [Int]
primesTo top | top < 2   = []
             | otherwise = 2 : [ i * 2 + 1 | (i, True) <- U.assocs sieveUA ]
 where
  sieveUA :: U.UArray Int Bool
  sieveUA = ST.runSTUArray $ do
    let m = (top - 1) `div` 2
        r = floor . sqrt $ (fromIntegral top + 1 :: Double)
    sieve <- ST.newArray (1, m) True
    forM_ [1 .. r `div` 2] $ \i -> do
      isPrime <- ST.readArray sieve i
      when isPrime $ do
        forM_ [2 * i * (i + 1), 2 * i * (i + 2) + 1 .. m] $ \j -> do
          ST.writeArray sieve j False
    return sieve

-- | from https://wiki.haskell.org/Prime_numbers#Using_Immutable_Arrays
primes :: [Int]
primes = 2 : oddprimes ()
 where
  oddprimes = (3 :) . sieve 3 [] . oddprimes
  sieve _ _ [] = []
  sieve x fs (p : ps) =
    [ i * 2 + x | (i, True) <- U.assocs a ]
    ++ sieve (p * p) ((p, 0) : [ (s, rem (y - q) s) | (s, y) <- fs ]) ps
   where
    q = (p * p - x) `div` 2
    a :: U.UArray Int Bool
    a = U.accumArray (\_ _ -> False)
                     True
                     (1, q - 1)
                     [ (i, ()) | (s, y) <- fs, i <- [y + s, y + s + s .. q] ]
