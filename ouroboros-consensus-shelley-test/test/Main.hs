module Main (main) where

import           Cardano.Crypto.Libsodium (sodiumInit)

import           Test.Tasty
import           Test.Util.Nightly

import qualified Test.Consensus.Shelley.Coherence (tests)
import qualified Test.Consensus.Shelley.Golden (tests)
import qualified Test.Consensus.Shelley.Serialisation (tests)
import qualified Test.ThreadNet.Shelley (tests)

main :: IO ()
main = sodiumInit >> defaultMainWithIohkNightly tests

tests :: TestTree
tests =
  testGroup "shelley"
  [ Test.Consensus.Shelley.Coherence.tests
  , Test.Consensus.Shelley.Golden.tests
  , Test.Consensus.Shelley.Serialisation.tests
  , Test.ThreadNet.Shelley.tests
  ]
