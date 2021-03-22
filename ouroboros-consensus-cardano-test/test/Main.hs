module Main (
    main
  ) where

import           Cardano.Crypto.Libsodium (sodiumInit)

import           Test.Tasty
import           Test.Util.Nightly

import qualified Test.Consensus.Cardano.ByronCompatibility (tests)
import qualified Test.Consensus.Cardano.Golden (tests)
import qualified Test.Consensus.Cardano.Serialisation (tests)
import qualified Test.ThreadNet.AllegraMary (tests)
import qualified Test.ThreadNet.Cardano (tests)
import qualified Test.ThreadNet.ShelleyAllegra (tests)

main :: IO ()
main = sodiumInit >> defaultMainWithIohkNightly tests

tests :: TestTree
tests =
  testGroup "cardano"
  [ Test.Consensus.Cardano.ByronCompatibility.tests
  , Test.Consensus.Cardano.Golden.tests
  , Test.Consensus.Cardano.Serialisation.tests
  , Test.ThreadNet.Cardano.tests
  , Test.ThreadNet.ShelleyAllegra.tests
  , Test.ThreadNet.AllegraMary.tests
  ]
