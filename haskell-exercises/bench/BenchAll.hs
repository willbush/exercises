module Main where

import           Criterion.Main
import           Common.Fibonacci               ( fibFast
                                                , fibSlow
                                                )

main :: IO ()
main = defaultMain
  [ bgroup "fibSlow"
    [ bench "9" $ whnf fibSlow 9
    , bench "11" $ whnf fibSlow 11
    ]
  , bgroup "fibFast"
    [ bench "9" $ whnf fibFast 9
    , bench "11" $ whnf fibFast 11
    , bench "1000" $ whnf fibFast 1000
    ]
  ]
