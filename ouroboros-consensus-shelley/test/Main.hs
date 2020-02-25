module Main (main) where

import           Test.Tasty

import qualified Test.Consensus.Shelley.Ledger (tests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "shelley"
  [ Test.Consensus.Shelley.Ledger.tests
  ]
