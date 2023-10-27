{-# LANGUAGE NumericUnderscores #-}

module Main (main) where

import           Test.Tasty.Bench

import           Test.Ouroboros.Network.PeerSelection.PeerMetric
                     (microbenchmark1GenerateInput, microbenchmark1ProcessInput)

main :: IO ()
main = do
    is <- mapM (microbenchmark1GenerateInput False . snd) benchmarks
    defaultMain
      [bgroup "ouroboros-network:sim-benchmarks"
         [ bench (unwords ["microbenchmark1",name])
               $ nfAppIO microbenchmark1ProcessInput i
         | ((name,_),i) <- zip benchmarks is
         ]
      ]
  where
    benchmarks = [("1k"  ,   1000)
                 ,("10k" , 10_000)
                 ,("100k",100_000)
                 ]
