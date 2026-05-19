module Test.Cardano.Network.OrphanInstances.Tests (tests) where

import Data.Aeson
import Data.Aeson.Types (parse)

import Ouroboros.Network.Diffusion.Topology
import Ouroboros.Network.PeerSelection.LedgerPeers.Type (AfterSlot (..),
           UseLedgerPeers (..))

import Cardano.Network.OrphanInstances ()
import Cardano.Network.PeerSelection.Bootstrap
import Cardano.Network.PeerSelection.PeerTrustable
import Cardano.Network.Protocol.Handshake.Test hiding (tests)
import Cardano.Slotting.Slot (SlotNo (..))

import Test.Cardano.Network.PeerSelection.Instances ()
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests = testGroup "Cardano.Network.OrphanInstances"
        [ testProperty "NodeToNodeVersion" prop_json_NodeToNodeVersion
        , testProperty "NodeToClientVersion" prop_json_NodeToClientVersion
        , testProperty "NetworkTopology" prop_json_NetworkTopology
        ]


prop_json_NetworkTopology :: NetworkTopology UseBootstrapPeers PeerTrustable -> Property
prop_json_NetworkTopology topo =
  parse (parseJSON . toJSON) topo
    === Data.Aeson.Success (translate topo)
  where
    translate
      nt@(NetworkTopology {useLedgerPeers = UseLedgerPeers (After (SlotNo 0))})
      = nt {useLedgerPeers = UseLedgerPeers Always}
    translate nt = nt


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
