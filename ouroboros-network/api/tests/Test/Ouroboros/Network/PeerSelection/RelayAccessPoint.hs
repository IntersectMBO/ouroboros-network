module Test.Ouroboros.Network.PeerSelection.RelayAccessPoint (tests) where

import Codec.CBOR.Read as CBOR
import Codec.CBOR.Write as CBOR
import Data.Aeson qualified as Aeson
import Data.ByteString.Char8 qualified as BSC

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Ouroboros.Network.PeerSelection.PeerAdvertise
import Ouroboros.Network.PeerSelection.RelayAccessPoint

import Test.Ouroboros.Network.OrphanInstances ()
import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests = testGroup "Ouroboros.Network.PeerSelection"
  [ testGroup "cbor"
    [ testProperty "LedgerRelayAccessPoint" prop_cbor_LedgerRelayAccessPoint
    , testProperty "RelayAccessPoint"       prop_cbor_RelayAccessPoint
    ]
  , testGroup "json"
    [ testProperty "RelayAccessPoint"       prop_json_RelayAccessPoint
    , testProperty "LedgerRelayAccessPoint" prop_json_LedgerRelayAccessPoint
    , testProperty "PeerAdvertise"          prop_json_PeerAdvertise
    ]
  ]

prop_cbor_LedgerRelayAccessPoint :: LedgerRelayAccessPoint
                                 -> Property
prop_cbor_LedgerRelayAccessPoint ap =
  case CBOR.deserialiseFromBytes fromCBOR (CBOR.toLazyByteString $ toCBOR ap) of
    Left err       -> counterexample (show err) False
    Right (_, ap') -> ap === ap'


prop_cbor_RelayAccessPoint :: RelayAccessPoint
                                 -> Property
prop_cbor_RelayAccessPoint ap =
  case CBOR.deserialiseFromBytes fromCBOR (CBOR.toLazyByteString $ toCBOR ap) of
    Left err       -> counterexample (show err) False
    Right (_, ap') -> ap === ap'


prop_json_LedgerRelayAccessPoint :: LedgerRelayAccessPoint
                                 -> Property
prop_json_LedgerRelayAccessPoint ap =
    Aeson.decode (Aeson.encode ap) === Just (fullyQualified ap)
    .&&.
    Aeson.fromJSON (Aeson.toJSON ap) === pure (fullyQualified ap)
  where
    fullyQualified :: LedgerRelayAccessPoint -> LedgerRelayAccessPoint
    fullyQualified it@(LedgerRelayAccessAddress {}) = it
    fullyQualified (LedgerRelayAccessDomain d p) =
      LedgerRelayAccessDomain (fullyQualified' d) p
    fullyQualified (LedgerRelayAccessSRVDomain d) =
      LedgerRelayAccessSRVDomain (fullyQualified' d)

    fullyQualified' domain = case domain of
      _ | Just (_, '.') <- BSC.unsnoc domain -> domain
        | otherwise -> domain `BSC.snoc` '.'


-- | Test round trip identify, modulo fully qualified domain names
--
prop_json_RelayAccessPoint :: RelayAccessPoint -> Property
prop_json_RelayAccessPoint ra =
    (let tripped = Aeson.decode (Aeson.encode ra)
     in tripped === Just ra .||. tripped === Just (fullyQualified ra))
    .&&.
    (let tripped = Aeson.fromJSON (Aeson.toJSON ra)
     in tripped === pure ra .||. tripped === pure (fullyQualified ra))
  where
    fullyQualified :: RelayAccessPoint -> RelayAccessPoint
    fullyQualified it@(RelayAccessAddress {}) = it
    fullyQualified (RelayAccessDomain d p) =
      RelayAccessDomain (fullyQualified' d) p
    fullyQualified (RelayAccessSRVDomain d) =
      RelayAccessSRVDomain (fullyQualified' d)

    fullyQualified' domain = case domain of
      _ | Just (_, '.') <- BSC.unsnoc domain -> domain
        | otherwise -> domain `BSC.snoc` '.'


prop_json_PeerAdvertise :: PeerAdvertise -> Property
prop_json_PeerAdvertise pa =
    Aeson.decode (Aeson.encode pa) === Just pa
    .&&.
    Aeson.fromJSON (Aeson.toJSON pa) === pure pa
