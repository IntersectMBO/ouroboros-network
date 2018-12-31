module Main (main) where

import           Test.Tasty

import qualified Test.ChainGenerators (tests)
import qualified Test.Chain (tests)
import qualified Test.ChainFragment (tests)
import qualified Test.ChainProducerState (tests)
import qualified Test.Mux (tests)
import qualified Test.FetchLogic (tests)
import qualified Test.Pipe (tests)
import qualified Test.Ouroboros.Network.Node (tests)
import qualified Ouroboros.Network.Protocol.ChainSync.Test (tests)
import qualified Ouroboros.Network.Protocol.BlockFetch.Test (tests)
import qualified Ouroboros.Network.Protocol.PingPong.Test (tests)
import qualified Ouroboros.Network.Protocol.ReqResp.Test (tests)
import qualified Test.Socket (tests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "ouroboros-network"
  [ Test.ChainGenerators.tests
  , Test.Chain.tests
  , Test.ChainFragment.tests
  , Test.ChainProducerState.tests
  , Test.Mux.tests
  , Test.FetchLogic.tests
  , Test.Pipe.tests
  , Test.Socket.tests
  , Test.Ouroboros.Network.Node.tests
  , Ouroboros.Network.Protocol.ChainSync.Test.tests
  , Ouroboros.Network.Protocol.BlockFetch.Test.tests
  , Ouroboros.Network.Protocol.PingPong.Test.tests
  , Ouroboros.Network.Protocol.ReqResp.Test.tests
  ]
