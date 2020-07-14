module Main (main) where

import           Test.Tasty
import           Test.Util.Nightly

import qualified Test.Consensus.Shelley.Golden (tests)
import qualified Test.Consensus.Shelley.Serialisation (tests)
import qualified Test.ThreadNet.RealTPraos (tests)

main :: IO ()
main = defaultMainWithIohkNightly tests

tests :: TestTree
tests =
  testGroup "shelley"
  [ Test.Consensus.Shelley.Golden.tests
  , Test.Consensus.Shelley.Serialisation.tests
  , Test.ThreadNet.RealTPraos.tests
  ]
