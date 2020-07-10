module Main (main) where

import           Test.Tasty

import qualified Test.Consensus.Cardano.ByronCompatibility (tests)
import qualified Test.Consensus.Cardano.Golden (tests)
import qualified Test.Consensus.Cardano.Serialisation (tests)
import qualified Test.ThreadNet.Cardano (tests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "cardano"
  [ Test.Consensus.Cardano.ByronCompatibility.tests
  , Test.Consensus.Cardano.Golden.tests
  , Test.Consensus.Cardano.Serialisation.tests
  , Test.ThreadNet.Cardano.tests
  ]
