module Main (main) where

import           Test.Tasty

import qualified Test.ChainGenerators (tests)
import qualified Test.Chain (tests)
import qualified Test.ChainFragment (tests)
import qualified Test.ChainProducerState (tests)
import qualified Test.Pipe (tests)
import qualified Test.Ouroboros.Network.Node (tests)
import qualified Test.Ouroboros.Network.Protocol.ChainSync (tests)
import qualified Test.Ouroboros.Network.Protocol.BlockFetch (tests)
import qualified Test.Ouroboros.Network.Protocol.PingPong (tests)
import qualified Test.Ouroboros.Network.Protocol.ReqResp (tests)
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
  , Test.Pipe.tests
  , Test.Socket.tests
  , Test.Ouroboros.Network.Node.tests
  , Test.Ouroboros.Network.Protocol.ChainSync.tests
  , Test.Ouroboros.Network.Protocol.BlockFetch.tests
  , Test.Ouroboros.Network.Protocol.PingPong.tests
  , Test.Ouroboros.Network.Protocol.ReqResp.tests
  ]
