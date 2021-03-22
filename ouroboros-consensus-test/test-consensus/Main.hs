module Main (main) where

import           Test.Tasty

import qualified Test.Consensus.BlockchainTime.Simple (tests)
import qualified Test.Consensus.HardFork.Combinator (tests)
import qualified Test.Consensus.HardFork.Forecast (tests)
import qualified Test.Consensus.HardFork.History (tests)
import qualified Test.Consensus.HardFork.Summary (tests)
import qualified Test.Consensus.Mempool (tests)
import qualified Test.Consensus.MiniProtocol.ChainSync.Client (tests)
import qualified Test.Consensus.MiniProtocol.LocalStateQuery.Server (tests)
import qualified Test.Consensus.Node (tests)
import qualified Test.Consensus.ResourceRegistry (tests)
import qualified Test.Consensus.Util.MonadSTM.RAWLock (tests)
import qualified Test.Consensus.Util.Versioned (tests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "ouroboros-consensus"
  [ Test.Consensus.BlockchainTime.Simple.tests
  , Test.Consensus.MiniProtocol.ChainSync.Client.tests
  , Test.Consensus.MiniProtocol.LocalStateQuery.Server.tests
  , Test.Consensus.Mempool.tests
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
