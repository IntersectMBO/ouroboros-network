module Main (main) where

import           Test.Tasty.Bench

import qualified Bench.Ouroboros.Consensus.Storage.LedgerDB.HD.DiffSeq as DS

main :: IO ()
main = defaultMain [
    bgroup "Ouroboros" [
        bgroup "Consensus" [
            bgroup "Storage" [
                bgroup "LedgerDB" [
                    bgroup "HD" [
                        DS.benchmarks
                      ]
                  ]
              ]
          ]
      ]
  ]
