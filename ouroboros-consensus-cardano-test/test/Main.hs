module Main (main) where

import           System.IO (BufferMode (LineBuffering), hSetBuffering,
                     hSetEncoding, stdout, utf8)

import           Cardano.Crypto.Libsodium (sodiumInit)

import           Test.Tasty

import qualified Test.Consensus.Cardano.ByronCompatibility (tests)
import qualified Test.Consensus.Cardano.Golden (tests)
import qualified Test.Consensus.Cardano.Serialisation (tests)
import qualified Test.Consensus.Cardano.Translation (tests)
import qualified Test.ThreadNet.AllegraMary (tests)
import qualified Test.ThreadNet.Cardano (tests)
import qualified Test.ThreadNet.MaryAlonzo (tests)
import qualified Test.ThreadNet.ShelleyAllegra (tests)
import           Test.Util.TestEnv (defaultMainWithTestEnv,
                     defaultTestEnvConfig)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetEncoding stdout utf8
  sodiumInit
  defaultMainWithTestEnv defaultTestEnvConfig tests

tests :: TestTree
tests =
  testGroup "cardano"
  [ Test.Consensus.Cardano.ByronCompatibility.tests
  , Test.Consensus.Cardano.Golden.tests
  , Test.Consensus.Cardano.Serialisation.tests
  , testGroup "ThreadNet" [
      Test.ThreadNet.AllegraMary.tests
      , Test.ThreadNet.Cardano.tests
      , Test.ThreadNet.MaryAlonzo.tests
      , Test.ThreadNet.ShelleyAllegra.tests
      ]
  , Test.Consensus.Cardano.Translation.tests
  ]
