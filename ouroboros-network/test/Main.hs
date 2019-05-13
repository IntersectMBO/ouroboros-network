module Main (main) where

import           Test.Tasty

import qualified Test.AnchoredFragment (tests)
import qualified Test.ChainGenerators (tests)
import qualified Test.Chain (tests)
import qualified Test.ChainFragment (tests)
import qualified Test.ChainProducerState (tests)
import qualified Test.Mux (tests)
import qualified Test.Pipe (tests)
import qualified Test.Ouroboros.Network.Node (tests)
import qualified Test.Ouroboros.Network.BlockFetch (tests)
import qualified Ouroboros.Network.Protocol.ChainSync.Test (tests)
import qualified Ouroboros.Network.Protocol.BlockFetch.Test (tests)
import qualified Ouroboros.Network.Protocol.PingPong.Test (tests)
import qualified Ouroboros.Network.Protocol.ReqResp.Test (tests)
import qualified Ouroboros.Network.Protocol.Handshake.Test (tests)
import qualified Test.Socket (tests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "ouroboros-network"
  [ Test.AnchoredFragment.tests
  , Test.ChainGenerators.tests
  , Test.Chain.tests
  , Test.ChainFragment.tests
  , Test.ChainProducerState.tests
  , Test.Mux.tests
  , Test.Pipe.tests
  , Test.Socket.tests
  , Test.Ouroboros.Network.Node.tests
  , Test.Ouroboros.Network.BlockFetch.tests
  , Ouroboros.Network.Protocol.ChainSync.Test.tests
  , Ouroboros.Network.Protocol.BlockFetch.Test.tests
  , Ouroboros.Network.Protocol.PingPong.Test.tests
  , Ouroboros.Network.Protocol.ReqResp.Test.tests
  , Ouroboros.Network.Protocol.Handshake.Test.tests
  ]
