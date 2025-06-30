{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Ouroboros.Network.PeerSelection.RelayAccessPoint (tests) where

import Codec.CBOR.Read as CBOR
import Codec.CBOR.Write as CBOR
import Data.ByteString.Char8 qualified as BSC
import Data.IP qualified as IP
import Data.Word

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Ouroboros.Network.PeerSelection.RelayAccessPoint

import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests = testGroup "Ouroboros.Network.PeerSelection"
  [ testGroup "LedgerRelayAccessPoint"
    [ testProperty "cbor" prop_cbor_LedgerRelayAccessPoint
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

prop_cbor_LedgerRelayAccessPoint :: LedgerRelayAccessPoint
                                 -> Property
prop_cbor_LedgerRelayAccessPoint ap =
  case CBOR.deserialiseFromBytes fromCBOR (CBOR.toLazyByteString $ toCBOR ap) of
    Left err       -> counterexample (show err) False
    Right (_, ap') -> ap === ap'
