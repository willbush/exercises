module Main where

import           Criterion.Main
import qualified Common.Fibonacci              as F
import qualified Common.Primes                 as P
import qualified Common.Helpers                as H

main :: IO ()
main = defaultMain
  [ bgroup "fibSlow" [bench "11" $ whnf F.fibSlow 11]
  , bgroup
    "fibFast"
    [bench "11" $ whnf F.fibFast 11, bench "1000" $ whnf F.fibFast 1000]
  , bgroup "primeFactors"
           [bench "600851475143" $ nf P.primeFactors 600851475143]
  , bgroup "reverseInt" [bench "max Int" $ whnf H.reverseInt (maxBound :: Int)]
  ]
