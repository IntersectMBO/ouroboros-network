{-# LANGUAGE NumericUnderscores #-}

module Main (main) where

import Test.Tasty.Bench

import Test.Ouroboros.Network.PeerSelection.PeerMetric
           (microbenchmark1GenerateInput, microbenchmark1ProcessInput)

main :: IO ()
main =
    defaultMain
      [ bgroup "ouroboros-network:sim-benchmarks"
        [ bgroup "PeerMetrics"
          [ env (microbenchmark1GenerateInput False 1_000) $ \i ->
                bench "1k" $ nfAppIO microbenchmark1ProcessInput i
          , env (microbenchmark1GenerateInput False 10_000) $ \i ->
                bench "10k" $ nfAppIO microbenchmark1ProcessInput i
          , env (microbenchmark1GenerateInput False 100_000) $ \i ->
                bench "100k" $ nfAppIO microbenchmark1ProcessInput i
          ]
        ]
      ]
