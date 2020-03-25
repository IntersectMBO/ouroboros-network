module Main (main) where

import           Test.Tasty

import qualified Test.Consensus.Byron.Ledger (tests)
import qualified Test.ThreadNet.DualPBFT (tests)
import qualified Test.ThreadNet.RealPBFT (tests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "byron" $
  [ Test.Consensus.Byron.Ledger.tests
  , Test.ThreadNet.RealPBFT.tests
  , Test.ThreadNet.DualPBFT.tests
  ]
  `seq`
  [ Test.ThreadNet.RealPBFT.tests
  ]
