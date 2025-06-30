{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Ouroboros.Network.PeerSelection.RelayAccessPoint (tests) where

import Codec.CBOR.Read as CBOR
import Codec.CBOR.Write as CBOR
import Data.Aeson qualified as Aeson
import Data.ByteString.Char8 qualified as BSC
import Data.IP qualified as IP
import Data.Word

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Ouroboros.Network.PeerSelection.PeerAdvertise
import Ouroboros.Network.PeerSelection.RelayAccessPoint

import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests = testGroup "Ouroboros.Network.PeerSelection"
  [ testGroup "cbor"
    [ testProperty "LedgeRelayAccessPoint" prop_cbor_LedgerRelayAccessPoint
    , testProperty "RelayAccessPoint"      prop_cbor_RelayAccessPoint
    ]
  , testGroup "json"
    [ testProperty "RelayAccessPoint"  prop_json_RelayAccessPoint
    , testProperty "PeerAdvertise"     prop_json_PeerAdvertise
    ]
  ]

-- TODO:
-- These instances & generators belong to `ouroboros-network-testing`, but we
-- cannot put them there until we pack all packages as a one package with
-- sublibraries (due to cyclic depndecies).
--
-- These instances are also useful in `ouroboros-network:testlib`, and for now
-- they are duplicated.

genIPv4 :: Gen IP.IP
genIPv4 =
    IP.IPv4 . IP.toIPv4w <$> resize 200 arbitrary `suchThat` (> 100)

genIPv6 :: Gen IP.IP
genIPv6 =
    IP.IPv6 . IP.toIPv6w <$> genFourWord32
  where
    genFourWord32 :: Gen (Word32, Word32, Word32, Word32)
    genFourWord32 =
       (,,,) <$> resize 200 arbitrary `suchThat` (> 100)
             <*> arbitrary
             <*> arbitrary
             <*> arbitrary

instance Arbitrary PortNumber where
  arbitrary = elements [1000..1100]
  shrink = map fromIntegral
         . filter (>=1000)
         . shrink
         . fromIntegral @PortNumber @Word16

instance Arbitrary RelayAccessPoint where
    arbitrary = prefixLedgerRelayAccessPoint "_prefix._tcp" <$> arbitrary

instance Arbitrary LedgerRelayAccessPoint where
  arbitrary =
      frequency [ (4, LedgerRelayAccessAddress <$> oneof [genIPv4, genIPv6] <*> arbitrary)
                , (4, LedgerRelayAccessDomain <$> genDomainName <*> arbitrary)
                , (1, LedgerRelayAccessSRVDomain <$> genDomainName)]
    where
      genDomainName = elements $ (\i -> "test" <> (BSC.pack . show $ i)) <$> [1..6 :: Int]

instance Arbitrary PeerAdvertise where
  arbitrary = elements [ DoAdvertisePeer, DoNotAdvertisePeer ]

  shrink DoAdvertisePeer    = []
  shrink DoNotAdvertisePeer = [DoAdvertisePeer]


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
