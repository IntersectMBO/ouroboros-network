
module Test.Ouroboros.Network.PeerSelection.KnownPeers (tests) where
import           Data.Map (Map)
import           Ouroboros.Network.PeerSelection.PeerAdvertise (PeerAdvertise)
import           Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing)
import           Ouroboros.Network.PeerSelection.RelayAccessPoint
import           Ouroboros.Network.PeerSelection.RootPeersDNS.LedgerPeers
import qualified Ouroboros.Network.PeerSelection.State.KnownPeers as KnownPeers
import           Test.Ouroboros.Network.PeerSelection.Instances ()
import           Test.QuickCheck (Property, counterexample)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup "Ouroboros.Network.PeerSelection"
  [ testGroup "KnownPeers"
    [ testProperty "insert is idempotent" prop_insert_idempotent
    ]
  ]

prop_insert_idempotent
  :: Map RelayAccessPoint (Maybe PeerSharing, Maybe PeerAdvertise, Maybe IsLedgerPeer)
  -> Property
prop_insert_idempotent m =
  let knownPeers = KnownPeers.insert m KnownPeers.empty
      knownPeers' = KnownPeers.insert m knownPeers
   in counterexample ("KnownPeers 1: " ++ show knownPeers ++ "\n"
                    ++ "KnownPeers 2: " ++ show knownPeers') $
      knownPeers == knownPeers'

