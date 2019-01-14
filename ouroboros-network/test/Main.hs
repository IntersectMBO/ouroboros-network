module Main (main) where

import           Test.Tasty

import qualified Test.Chain (tests)
import qualified Test.ChainFragment (tests)
import qualified Test.ChainProducerState (tests)
import qualified Test.Pipe (tests)
import qualified Test.Ouroboros.Network.Node (tests)
import qualified Test.Ouroboros.Network.Protocol.Stream (tests)
import qualified Test.Ouroboros.Network.Protocol.ReqResp.Codec.Coherence (tests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "ouroboros-network"
  [ Test.Chain.tests
  , Test.ChainFragment.tests
  , Test.ChainProducerState.tests
  , Test.Pipe.tests
  , Test.Ouroboros.Network.Node.tests
  , Test.Ouroboros.Network.Protocol.Stream.tests
  , Test.Ouroboros.Network.Protocol.ReqResp.Codec.Coherence.tests
  ]
