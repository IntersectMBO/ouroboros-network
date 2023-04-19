-- | Tests for the infrastructure used for running consensus-related tests.
--
-- The library @ouroboros-consensus-test@ provides a bunch of test utilities
-- that we use throughout the consensus layer tests. Here we test this
-- infrastructure. Some examples of aspects we test here include:
--
-- * Some consensus tests override the leader schedule from the underlying
--  protocol, instead explicitly recording which nodes lead when. If we use a
--  round-robin schedule for this, and then compute the expected fork length, we'd
--  expect to get no forks at all.
-- * Some invariants of various utility functions.
--
module Main (main) where

import qualified Ouroboros.Consensus.Util.Tests (tests)
import           Test.Tasty
import qualified Test.ThreadNet.Util.Tests (tests)
import qualified Test.Util.ChainUpdates.Tests (tests)
import qualified Test.Util.Schedule.Tests (tests)
import qualified Test.Util.Split.Tests (tests)
import           Test.Util.TestEnv (defaultMainWithTestEnv,
                     defaultTestEnvConfig)

main :: IO ()
main = defaultMainWithTestEnv defaultTestEnvConfig tests

tests :: TestTree
tests =
  testGroup "test-infra"
  [ Ouroboros.Consensus.Util.Tests.tests
  , Test.ThreadNet.Util.Tests.tests
  , Test.Util.ChainUpdates.Tests.tests
  , Test.Util.Schedule.Tests.tests
  , Test.Util.Split.Tests.tests
  ]
