module Main (main) where

import           Test.Tasty

import qualified Test.Chain (tests)
import qualified Test.ChainProducerState (tests)
import qualified Test.Node (tests)
import qualified Test.Pipe (tests)
import qualified Test.Sim (tests)
import qualified Test.Ouroboros.Network.Protocol.Stream (tests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "ouroboros-network"
  [ Test.Chain.tests
  , Test.ChainProducerState.tests
  , Test.Sim.tests
  , Test.Node.tests
  , Test.Pipe.tests
  , Test.Ouroboros.Network.Protocol.Stream.tests
  ]
