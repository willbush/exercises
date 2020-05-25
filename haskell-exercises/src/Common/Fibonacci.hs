{-# LANGUAGE StrictData #-}

module Common.Fibonacci (fibFast, fibSlow) where

import qualified Data.Semigroup as SG

data Matrix2x2 = Matrix
  { x00 :: Integer, x01 :: Integer
  , x10 :: Integer, x11 :: Integer
  }

instance Monoid Matrix2x2 where
  mempty =
    Matrix
      { x00 = 1, x01 = 0
      , x10 = 0, x11 = 1
      }

instance Semigroup Matrix2x2 where
  Matrix l00 l01 l10 l11 <> Matrix r00 r01 r10 r11 = Matrix
    { x00 = l00 * r00 + l01 * r10
    , x01 = l00 * r01 + l01 * r11
    , x10 = l10 * r00 + l11 * r10
    , x11 = l10 * r01 + l11 * r11
    }

-- | taken from and slightly modified to return negative numbers instead of
-- error:
-- http://www.haskellforall.com/2020/04/blazing-fast-fibonacci-numbers-using.html
fibFast :: Integer -> Integer
fibFast n | n <= 1    = n
          | otherwise = x01 (SG.mtimesDefault n matrix)
  where
    matrix =
      Matrix
        { x00 = 0, x01 = 1
        , x10 = 1, x11 = 1
        }

-- | The extremely slow recursive implementation of fibonacci that will just
-- return any given negative number.
fibSlow :: Integer -> Integer
fibSlow n | n <= 1    = n
          | otherwise = fibSlow (n - 1) + fibSlow (n - 2)
