module CommonSpec (spec) where

import qualified Common.Fibonacci              as F
import qualified Common.Primes                 as P
import qualified Common.Helpers                as H
import           Test.Hspec

spec :: Spec
spec =
  describe "Common Library Functions" $ do
    describe "Fibonacci" $ do
      it "can generate the first 12 terms of the fibonacci sequence" $ do
        let expected = [0 , 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89]
            inputs = [0 .. 11]
        fmap F.fibFast inputs `shouldBe` expected
        fmap F.fibSlow inputs `shouldBe` expected
      it "returns any given negative number" $ do
        F.fibFast (-100) `shouldBe` (-100)
        F.fibSlow (-666) `shouldBe` (-666)

    describe "Prime Factors" $
      it "can give the expected prime factors" $ do
        P.primeFactors 2 `shouldBe` [2]
        P.primeFactors 3 `shouldBe` [3]
        P.primeFactors 4 `shouldBe` [2, 2]
        P.primeFactors 5 `shouldBe` [5]
        P.primeFactors 6 `shouldBe` [2, 3]
        P.primeFactors 7 `shouldBe` [7]
        P.primeFactors 8 `shouldBe` [2, 2, 2]
        P.primeFactors 9 `shouldBe` [3, 3]
        P.primeFactors 10 `shouldBe` [2, 5]
        P.primeFactors 26 `shouldBe` [2, 13]
        P.primeFactors 30 `shouldBe` [2, 3, 5]
        P.primeFactors 100 `shouldBe` [2, 2, 5, 5]
        P.primeFactors 966 `shouldBe` [2, 3, 7, 23]
        P.primeFactors 990 `shouldBe` [2, 3, 3, 5, 11]
        P.primeFactors 600851475143 `shouldBe` [71, 839, 1471, 6857]

    describe "Reverse Int" $ do
      it "can reverse n >= 0 integers" $ do
        H.reverseInt 0 `shouldBe` 0
        H.reverseInt 1 `shouldBe` 1
        H.reverseInt 12 `shouldBe` 21
        H.reverseInt 123 `shouldBe` 321
        H.reverseInt 1234 `shouldBe` 4321
      it "can reverse n < 0 integers" $ do
        H.reverseInt (-1)`shouldBe` (-1)
        H.reverseInt (-12)`shouldBe` (-21)
        H.reverseInt (-123)`shouldBe` (-321)
        H.reverseInt (-1234)`shouldBe` (-4321)

    describe "Prime up to" $ do
      it "returns nothing for numbers less than 2" $ do
        P.primesTo (-1) `shouldBe` []
        P.primesTo 0 `shouldBe` []
        P.primesTo 1 `shouldBe` []
      it "returns prime numbers up to a number" $ do
        P.primesTo 2 `shouldBe` [2]
        P.primesTo 11 `shouldBe` [2, 3, 5, 7, 11]
        P.primesTo 23 `shouldBe` [2, 3, 5, 7, 11, 13, 17, 19, 23]

    describe "infinite primes" $ do
      it "can give first 6" $
        take 6 P.primes `shouldBe` [2, 3, 5, 7, 11, 13]
