module Test.Ouroboros.Network.PeerSelection.Json (tests) where

import Data.Aeson (decode, encode, fromJSON, toJSON)
import Data.ByteString.Char8 (snoc, unsnoc)
import Test.Ouroboros.Network.PeerSelection.Instances ()

import Ouroboros.Network.PeerSelection.PeerAdvertise (PeerAdvertise)
import Ouroboros.Network.PeerSelection.RelayAccessPoint (RelayAccessPoint (..))
import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup "Ouroboros.Network.PeerSelection"
  [ testGroup "JSON"
    [ testProperty "RelayAccessPoint roundtrip"  prop_roundtrip_RelayAccessPoint_JSON
    , testProperty "PeerAdvertise roundtrip"     prop_roundtrip_PeerAdvertise_JSON
    ]
  ]

-- | Test round trip identify, modulo fully qualified domain names
--
prop_roundtrip_RelayAccessPoint_JSON :: RelayAccessPoint -> Property
prop_roundtrip_RelayAccessPoint_JSON ra =
    (let tripped = decode (encode ra)
     in tripped === Just ra .||. tripped === Just (fullyQualified ra))
    .&&.
    (let tripped = fromJSON (toJSON ra)
     in tripped === pure ra .||. tripped === pure (fullyQualified ra))
  where
    fullyQualified :: RelayAccessPoint -> RelayAccessPoint
    fullyQualified it@(RelayAccessAddress {}) = it
    fullyQualified (RelayAccessDomain d p) =
      RelayAccessDomain (fullyQualified' d) p
    fullyQualified (RelayAccessSRVDomain d) =
      RelayAccessSRVDomain (fullyQualified' d)

    fullyQualified' domain = case domain of
      _ | Just (_, '.') <- unsnoc domain -> domain
        | otherwise -> domain `snoc` '.'

prop_roundtrip_PeerAdvertise_JSON :: PeerAdvertise -> Property
prop_roundtrip_PeerAdvertise_JSON pa =
    decode (encode pa) === Just pa
    .&&.
    fromJSON (toJSON pa) === pure pa
