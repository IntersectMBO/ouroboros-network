module Test.Ouroboros.Network.PeerSelection.Json
  ( tests ) where

import           Data.Aeson (decode, encode, fromJSON, toJSON)
import           Ouroboros.Network.PeerSelection.RootPeersDNS
                   (DomainAccessPoint (..), RelayAccessPoint (..))
import           Ouroboros.Network.PeerSelection.Types (PeerAdvertise)
import           Test.Ouroboros.Network.PeerSelection.Instances()

import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup "Ouroboros.Network.PeerSelection"
  [ testGroup "JSON"
    [ testProperty "DomainAccessPoint roundtrip" prop_roundtrip_DomainAccessPoint_JSON
    , testProperty "RelayAccessPoint roundtrip"  prop_roundtrip_RelayAccessPoint_JSON
    , testProperty "PeerAdvertise roundtrip"     prop_roundtrip_PeerAdvertise_JSON
    ]
  ]

prop_roundtrip_DomainAccessPoint_JSON :: DomainAccessPoint -> Property
prop_roundtrip_DomainAccessPoint_JSON da =
    decode (encode da) === Just da
    .&&.
    fromJSON (toJSON da) === pure da

prop_roundtrip_RelayAccessPoint_JSON :: RelayAccessPoint -> Property
prop_roundtrip_RelayAccessPoint_JSON ra =
    decode (encode ra) === Just ra
    .&&.
    fromJSON (toJSON ra) === pure ra

prop_roundtrip_PeerAdvertise_JSON :: PeerAdvertise -> Property
prop_roundtrip_PeerAdvertise_JSON pa =
    decode (encode pa) === Just pa
    .&&.
    fromJSON (toJSON pa) === pure pa

