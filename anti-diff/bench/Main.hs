module Main (main) where

import           Test.Tasty.Bench

import           Bench.Data.FingerTree.RootMeasured.Strict (benchmarks)

main :: IO ()
main = defaultMain [
    bgroup "Bench" [
        bgroup "Data" [
            bgroup "FingerTree" [
                bgroup "RootMeasured" [
                    benchmarks
                  ]
              ]
          ]
      ]
  ]
