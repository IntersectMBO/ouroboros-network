module Main (main) where

import           Test.Tasty

import qualified Test.Consensus.Byron.Golden (tests)
import qualified Test.Consensus.Byron.Serialisation (tests)
import qualified Test.ThreadNet.DualPBFT (tests)
import qualified Test.ThreadNet.RealPBFT (tests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "byron"
  [ Test.Consensus.Byron.Golden.tests
  , Test.Consensus.Byron.Serialisation.tests
  , Test.ThreadNet.RealPBFT.tests
  , Test.ThreadNet.DualPBFT.tests
  ]
