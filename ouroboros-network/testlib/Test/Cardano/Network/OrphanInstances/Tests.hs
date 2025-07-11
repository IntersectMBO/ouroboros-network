module Test.Cardano.Network.OrphanInstances.Tests (tests) where

import Data.Aeson

import Cardano.Network.OrphanInstances ()
import Ouroboros.Network.OrphanInstances ()
import Ouroboros.Network.Protocol.Handshake.Test hiding (tests)

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck


tests :: TestTree
tests = testGroup "Cardano.Network.OrphanInstances"
        [ testProperty "NodeToNodeVersion" prop_json_NodeToNodeVersion
        , testProperty "NodeToClientVersion" prop_json_NodeToClientVersion
        ]


prop_json_NodeToNodeVersion
  :: ArbitraryNodeToNodeVersion
  -> Property
prop_json_NodeToNodeVersion (ArbitraryNodeToNodeVersion v) =
  case eitherDecode (encode v) of
    Right v' -> v === v'
    Left  e  -> counterexample e False


prop_json_NodeToClientVersion
  :: ArbitraryNodeToClientVersion
  -> Property
prop_json_NodeToClientVersion (ArbitraryNodeToClientVersion v) =
  case eitherDecode (encode v) of
    Right v' -> v === v'
    Left  e  -> counterexample e False
