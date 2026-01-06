{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications   #-}

{-# OPTIONS_GHC -Wno-orphans    #-}

module Test.Ouroboros.Network.OrphanInstances
  ( -- * Generators
    genIPv4
  , genIPv6
  ) where

import Data.ByteString.Char8 qualified as BSC
import Data.Hashable (Hashable (hashWithSalt), hashUsing)
import Data.IP qualified as IP
import Data.Word (Word16, Word32, Word64)

import Cardano.Slotting.Slot (SlotNo (..))

import Ouroboros.Network.ConnectionManager.Types (Provenance (..))
import Ouroboros.Network.Diffusion.Topology (LocalRootPeersGroup (..),
           LocalRootPeersGroups (..), LocalRoots (..), NetworkTopology (..),
           PublicRootPeers (..), RootConfig (..))
import Ouroboros.Network.DiffusionMode (DiffusionMode (..))
import Ouroboros.Network.PeerSelection.LedgerPeers.Type (AfterSlot (..),
           UseLedgerPeers (..))
import Ouroboros.Network.PeerSelection.PeerAdvertise (PeerAdvertise (..))
import Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing (..))
import Ouroboros.Network.PeerSelection.RelayAccessPoint
           (LedgerRelayAccessPoint (..), PortNumber, RelayAccessPoint,
           SRVPrefix, prefixLedgerRelayAccessPoint)
import Ouroboros.Network.PeerSelection.State.LocalRootPeers (HotValency (..),
           LocalRootConfig (..), WarmValency (..))

import Test.QuickCheck (Arbitrary (..), Gen, elements, frequency, oneof, resize,
           suchThat)

-- * Generators

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

-- * Orphans

instance Hashable IP.IPv4
instance Hashable IP.IPv6
instance Hashable IP.IP

instance Hashable PortNumber where
  hashWithSalt salt pn =
    hashUsing (fromIntegral :: PortNumber -> Word16) salt pn

deriving via Word64 instance Arbitrary SlotNo

instance Arbitrary PeerAdvertise where
  arbitrary = elements [ DoAdvertisePeer, DoNotAdvertisePeer ]

  shrink DoAdvertisePeer    = []
  shrink DoNotAdvertisePeer = [DoAdvertisePeer]

instance Arbitrary PeerSharing where
  arbitrary = elements [ PeerSharingDisabled, PeerSharingEnabled ]
  shrink PeerSharingDisabled = []
  shrink PeerSharingEnabled  = [PeerSharingDisabled]

instance Arbitrary AfterSlot where
  arbitrary = oneof [ pure Always
                    , After <$> arbitrary
                    ]

instance Arbitrary PortNumber where
  arbitrary = elements [1000..1100]
  shrink = map fromIntegral
         . filter (>=1000)
         . shrink
         . fromIntegral @PortNumber @Word16

instance Arbitrary RelayAccessPoint where
    arbitrary = prefixLedgerRelayAccessPoint srvPrefix <$> arbitrary
      where
        srvPrefix :: SRVPrefix
        srvPrefix = "_cardano._tcp"

instance Arbitrary LedgerRelayAccessPoint where
  arbitrary =
      frequency [ (4, LedgerRelayAccessAddress <$> oneof [genIPv4, genIPv6] <*> arbitrary)
                , (4, LedgerRelayAccessDomain <$> genDomainName <*> arbitrary)
                , (1, LedgerRelayAccessSRVDomain <$> genDomainName)]
    where
      genDomainName = elements $ (\i -> "test" <> (BSC.pack . show $ i) <> ".") <$> [1..6 :: Int]

instance Arbitrary UseLedgerPeers where
    arbitrary = frequency
      [ (2, pure DontUseLedgerPeers)
      , (8, UseLedgerPeers <$> arbitrary)
      ]

instance Arbitrary extraFlags => Arbitrary (LocalRootConfig extraFlags) where
  arbitrary = LocalRootConfig
                <$> arbitrary
                <*> arbitrary
                <*> arbitrary
                <*> arbitrary
  shrink a@LocalRootConfig { peerAdvertise,
                             extraLocalRootFlags = peerTrustable,
                             diffusionMode
                           } =
    [ a { extraLocalRootFlags = peerTrustable' }
    | peerTrustable' <- shrink peerTrustable
    ]
    ++
    [ a { peerAdvertise = peerAdvertise' }
    | peerAdvertise' <- shrink peerAdvertise
    ]
    ++
    [ a { diffusionMode = diffusionMode' }
    | diffusionMode' <- case diffusionMode of
        InitiatorOnlyDiffusionMode         -> []
        InitiatorAndResponderDiffusionMode -> [InitiatorOnlyDiffusionMode]
    ]

instance (Arbitrary extraConfig, Arbitrary extraFlags) => Arbitrary (NetworkTopology extraConfig extraFlags) where
  arbitrary = NetworkTopology
                <$> arbitrary
                <*> arbitrary
                <*> arbitrary
                <*> arbitrary
                <*> arbitrary

instance Arbitrary PublicRootPeers where
  arbitrary = PublicRootPeers <$> arbitrary

instance (Arbitrary extraFlags) => Arbitrary (LocalRootPeersGroups extraFlags) where
  arbitrary = LocalRootPeersGroups <$> arbitrary

instance (Arbitrary extraFlags) => Arbitrary (LocalRootPeersGroup extraFlags) where
  arbitrary = LocalRootPeersGroup
                <$> arbitrary
                <*> arbitrary
                <*> arbitrary
                <*> arbitrary
                <*> arbitrary

instance Arbitrary LocalRoots where
  arbitrary = LocalRoots <$> arbitrary <*> arbitrary

instance Arbitrary RootConfig where
  arbitrary = RootConfig <$> arbitrary <*> arbitrary

instance Arbitrary Provenance where
  arbitrary = elements [Inbound, Outbound]

instance Arbitrary DiffusionMode where
  arbitrary = elements
                [ InitiatorOnlyDiffusionMode
                , InitiatorAndResponderDiffusionMode
                ]

instance Arbitrary HotValency where
  arbitrary = HotValency <$> arbitrary

instance Arbitrary WarmValency where
  arbitrary = WarmValency <$> arbitrary
