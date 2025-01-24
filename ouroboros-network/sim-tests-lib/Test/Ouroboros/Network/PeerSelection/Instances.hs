{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Ouroboros.Network.PeerSelection.Instances
  ( -- test types
    PeerAddr (..)
  , TestSeed (..)
    -- generators
  , genIPv4
  , genIPv6
  , genPort
    -- generator tests
  , prop_arbitrary_PeerSelectionTargets
  , prop_shrink_PeerSelectionTargets
  ) where

import Data.ByteString.Char8 qualified as BSC
import Data.Hashable
import Data.IP qualified as IP
import Data.Word (Word16, Word32, Word64)

import Cardano.Slotting.Slot (SlotNo (..))

import Ouroboros.Network.ConsensusMode
import Ouroboros.Network.NodeToNode.Version (DiffusionMode (..))
import Ouroboros.Network.PeerSelection.Bootstrap (UseBootstrapPeers (..))
import Ouroboros.Network.PeerSelection.Governor
import Ouroboros.Network.PeerSelection.LedgerPeers.Type (AfterSlot (..),
           UseLedgerPeers (..))
import Ouroboros.Network.PeerSelection.PeerAdvertise (PeerAdvertise (..))
import Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing (..))
import Ouroboros.Network.PeerSelection.PeerTrustable (PeerTrustable (..))
import Ouroboros.Network.PeerSelection.RelayAccessPoint
import Ouroboros.Network.PeerSelection.State.LocalRootPeers
           (LocalRootConfig (..))

import Test.Ouroboros.Network.Utils (ShrinkCarefully, prop_shrink_nonequal,
           prop_shrink_valid)
import Test.QuickCheck


--
-- QuickCheck instances
--

-- | Seed for domain lookups
--
newtype TestSeed = TestSeed { unTestSeed :: Int }
  deriving (Eq, Show)

instance Arbitrary TestSeed where
  arbitrary = TestSeed <$> chooseInt(minBound, maxBound)
  shrink _ = []

-- | Simple address representation for the tests
--
newtype PeerAddr = PeerAddr Int
  deriving (Eq, Ord, Show, Hashable)

-- | We mostly avoid using this instance since we need careful control over
-- the peer addrs, e.g. to make graphs work, and sets overlap etc. But it's
-- here for the few cases that need it, and it is used for (lack-of) shrinking.
--
instance Arbitrary PeerAddr where
  arbitrary = PeerAddr <$> arbitrarySizedNatural
  shrink _  = []

deriving via Word64 instance Arbitrary SlotNo

instance Arbitrary PeerAdvertise where
  arbitrary = elements [ DoAdvertisePeer, DoNotAdvertisePeer ]

  shrink DoAdvertisePeer    = []
  shrink DoNotAdvertisePeer = [DoAdvertisePeer]

instance Arbitrary PeerSharing where
  arbitrary = elements [ PeerSharingDisabled, PeerSharingEnabled ]
  shrink PeerSharingDisabled = []
  shrink PeerSharingEnabled  = [PeerSharingDisabled]

instance Arbitrary ConsensusMode where
  arbitrary = elements [PraosMode, GenesisMode]
  shrink GenesisMode = [PraosMode]
  shrink PraosMode   = []

instance Arbitrary AfterSlot where
  arbitrary = oneof [ pure Always
                    , After <$> arbitrary
                    ]

instance Arbitrary UseBootstrapPeers where
  arbitrary = frequency [ (1, pure DontUseBootstrapPeers)
                        , (1, UseBootstrapPeers <$> arbitrary)
                        ]

  shrink DontUseBootstrapPeers = []
  shrink (UseBootstrapPeers bp) | [] <- bp = [DontUseBootstrapPeers]
                                | [_] <- bp = [DontUseBootstrapPeers]
  shrink (UseBootstrapPeers (hd : _)) = [UseBootstrapPeers [hd]]

instance Arbitrary UseLedgerPeers where
    arbitrary = frequency
      [ (2, pure DontUseLedgerPeers)
      , (8, UseLedgerPeers <$> arbitrary)
      ]

instance Arbitrary PeerTrustable where
  arbitrary = elements [ IsNotTrustable, IsTrustable ]

instance Arbitrary PeerSelectionTargets where
  arbitrary = do
    targetNumberOfKnownPeers       <- getNonNegative <$> resize 1000 arbitrary
    targetNumberOfRootPeers        <- choose (0, min 100  targetNumberOfKnownPeers)
    targetNumberOfEstablishedPeers <- choose (0, min 1000 targetNumberOfKnownPeers)
    targetNumberOfActivePeers      <- choose (0, min 100  targetNumberOfEstablishedPeers)

    targetNumberOfKnownBigLedgerPeers
      <- getNonNegative <$> resize 1000 arbitrary
    targetNumberOfEstablishedBigLedgerPeers
      <- choose (0 , min 1000 targetNumberOfKnownBigLedgerPeers)
    targetNumberOfActiveBigLedgerPeers
      <- choose (0, min 100 targetNumberOfEstablishedBigLedgerPeers)

    return PeerSelectionTargets {
      targetNumberOfRootPeers,
      targetNumberOfKnownPeers,
      targetNumberOfEstablishedPeers,
      targetNumberOfActivePeers,
      targetNumberOfKnownBigLedgerPeers,
      targetNumberOfEstablishedBigLedgerPeers,
      targetNumberOfActiveBigLedgerPeers
    }

  shrink (PeerSelectionTargets r k e a kb eb ab) =
    [ targets'
    | (r',k',e',a',kb',eb',ab') <- shrink (r,k,e,a,kb,eb,ab)
    , let targets' = PeerSelectionTargets r' k' e' a' kb' eb' ab'
    , sanePeerSelectionTargets targets' ]

-- GovernorMockEnvironment is responsible for generating valid targets
-- which account for local roots from random peer graph, but a shrink
-- is useful here for recursively shrinking TimedScript.
--
instance Arbitrary ConsensusModePeerTargets where
  arbitrary = error "not implemented"

  shrink ConsensusModePeerTargets { deadlineTargets, syncTargets } =
    let syncTargets'     = shrink syncTargets
        deadlineTargets' = shrink deadlineTargets
    in [ConsensusModePeerTargets { deadlineTargets = deadlineTargets'', syncTargets = syncTargets'' }
       | deadlineTargets'' <- deadlineTargets',
         syncTargets'' <- syncTargets']

genIPv4 :: Gen IP.IP
genIPv4 =
    IP.IPv4 . IP.toIPv4w <$> resize 200 arbitrary `suchThat` (> 100)

genPort :: Gen PortNumber
genPort =
    fromIntegral <$> (arbitrary :: Gen Word16)

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

instance Arbitrary RelayAccessPoint where
  arbitrary =
      frequency [ (4, RelayAccessAddress <$> oneof [genIPv4, genIPv6] <*> genPort)
                , (4, RelayAccessDomain <$> genDomainName <*> genPort)
                , (1, RelayAccessSRVDomain <$> genDomainName)]
    where
      genDomainName = elements $ (\i -> "test" <> (BSC.pack . show $ i)) <$> [1..6 :: Int]

prop_arbitrary_PeerSelectionTargets :: PeerSelectionTargets -> Bool
prop_arbitrary_PeerSelectionTargets =
    sanePeerSelectionTargets

prop_shrink_PeerSelectionTargets :: ShrinkCarefully PeerSelectionTargets -> Property
prop_shrink_PeerSelectionTargets x =
      prop_shrink_valid sanePeerSelectionTargets x
 .&&. prop_shrink_nonequal x


instance Arbitrary LocalRootConfig where
  arbitrary = LocalRootConfig <$> arbitrary <*> arbitrary <*> elements [InitiatorAndResponderDiffusionMode, InitiatorOnlyDiffusionMode]
  shrink a@LocalRootConfig { peerAdvertise, peerTrustable, diffusionMode } =
    [ a { peerTrustable = peerTrustable' }
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
