{-# LANGUAGE NamedFieldPuns     #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications   #-}

module Test.Cardano.Network.PeerSelection.LocalRootPeers (tests) where

import Data.Map.Strict qualified as Map


import Cardano.Network.ConsensusMode (ConsensusMode (..))
import Cardano.Network.PeerSelection.ExtraRootPeers qualified as Cardano.ExtraPeers
import Cardano.Network.PeerSelection.Governor.Monitor qualified as Monitor
import Cardano.Network.PeerSelection.Governor.PeerSelectionState qualified as Cardano.ExtraState
import Cardano.Network.PeerSelection.PeerTrustable (PeerTrustable)
import Cardano.Network.PeerSelection.State.LocalRootPeers (LocalRootPeers)
import Cardano.Network.PeerSelection.State.LocalRootPeers qualified as LocalRootPeers

import Test.Cardano.Network.PeerSelection.Instances ()
import Test.Ouroboros.Network.PeerSelection.Instances (PeerAddr)
import Test.Ouroboros.Network.PeerSelection.LocalRootPeers (AdvertiseChange,
           governorAdvertiseChanges)

import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup "Ouroboros.Network.PeerSelection.Cardano"
  [ testGroup "LocalRootPeers"
    [ testProperty "clampToTrustable" prop_clampToTrustable
    , testProperty "the governor gives the known peers the new flags"
                   prop_governor_advertiseChanges
    ]
  ]

prop_clampToTrustable :: LocalRootPeers PeerTrustable PeerAddr
                      -> Property
prop_clampToTrustable localRootPeers =

    let trustedPeers = LocalRootPeers.keysSet
                     $ LocalRootPeers.clampToTrustable localRootPeers

     in counterexample (show $ Map.restrictKeys (LocalRootPeers.toMap localRootPeers) trustedPeers)
      $ all (`LocalRootPeers.isPeerTrustable` localRootPeers) trustedPeers

-- | The Cardano governor's local roots monitor, from a state holding the old
-- local roots and known peers with their flags, reads the new ones and
-- leaves the known peers as 'knownPeersAfter' requires; it does not fire
-- when nothing changed. The node is not in a sensitive state, so no local
-- root is clamped away.
prop_governor_advertiseChanges :: AdvertiseChange PeerTrustable -> Property
prop_governor_advertiseChanges =
    governorAdvertiseChanges
      Monitor.localRoots
      (Cardano.ExtraState.empty PraosMode (Cardano.ExtraState.NumberOfBigLedgerPeers 0))
      Cardano.ExtraPeers.empty
      Cardano.ExtraPeers.cardanoPublicRootPeersAPI
