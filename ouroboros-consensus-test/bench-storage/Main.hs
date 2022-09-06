module Main (main) where

import           Test.Tasty.Bench

import qualified Bench.Ouroboros.Consensus.Storage.LedgerDB.HD as HD

main :: IO ()
main = defaultMain [
    bgroup "Ouroboros" [
        bgroup "Consensus" [
            bgroup "Storage" [
                bgroup "LedgerDB" [
                    HD.benchmarks
                  ]
              ]
          ]
      ]
  ]
