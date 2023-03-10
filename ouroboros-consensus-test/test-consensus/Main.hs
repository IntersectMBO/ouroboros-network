module Main (main) where

import           Test.Tasty

import qualified Test.Consensus.BlockchainTime.Simple
import qualified Test.Consensus.HardFork.Combinator
import qualified Test.Consensus.HardFork.Forecast
import qualified Test.Consensus.HardFork.History
import qualified Test.Consensus.HardFork.Summary
import qualified Test.Consensus.Mempool
import qualified Test.Consensus.Mempool.Fairness
import qualified Test.Consensus.MiniProtocol.BlockFetch.Client
import qualified Test.Consensus.MiniProtocol.ChainSync.Client
import qualified Test.Consensus.MiniProtocol.LocalStateQuery.Server
import qualified Test.Consensus.Node
import qualified Test.Consensus.ResourceRegistry
import qualified Test.Consensus.Util.MonadSTM.RAWLock
import qualified Test.Consensus.Util.Versioned
import           Test.Util.TestEnv (defaultMainWithTestEnv,
                     defaultTestEnvConfig)

main :: IO ()
main = defaultMainWithTestEnv defaultTestEnvConfig tests

tests :: TestTree
tests =
  testGroup "ouroboros-consensus"
  [ Test.Consensus.BlockchainTime.Simple.tests
  , Test.Consensus.MiniProtocol.BlockFetch.Client.tests
  , Test.Consensus.MiniProtocol.ChainSync.Client.tests
  , Test.Consensus.MiniProtocol.LocalStateQuery.Server.tests
  , Test.Consensus.Mempool.tests
  , Test.Consensus.Mempool.Fairness.tests
  , Test.Consensus.Node.tests
  , Test.Consensus.ResourceRegistry.tests
  , Test.Consensus.Util.MonadSTM.RAWLock.tests
  , Test.Consensus.Util.Versioned.tests
  , testGroup "HardFork" [
        testGroup "History" [
            Test.Consensus.HardFork.Summary.tests
          , Test.Consensus.HardFork.History.tests
          ]
      , testGroup "Combinator" [
            Test.Consensus.HardFork.Forecast.tests
          , Test.Consensus.HardFork.Combinator.tests
          ]
      ]
  ]
