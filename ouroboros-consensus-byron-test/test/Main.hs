module Main (main) where

import qualified Test.Consensus.Byron.Golden (tests)
import qualified Test.Consensus.Byron.Serialisation (tests)
import           Test.Tasty
import qualified Test.ThreadNet.Byron (tests)
import qualified Test.ThreadNet.DualByron (tests)
import           Test.Util.TestEnv (defaultMainWithTestEnv,
                     defaultTestEnvConfig)

main :: IO ()
main = defaultMainWithTestEnv defaultTestEnvConfig tests

tests :: TestTree
tests =
  testGroup "byron"
  [ Test.Consensus.Byron.Golden.tests
  , Test.Consensus.Byron.Serialisation.tests
  , Test.ThreadNet.Byron.tests
  , Test.ThreadNet.DualByron.tests
  ]
