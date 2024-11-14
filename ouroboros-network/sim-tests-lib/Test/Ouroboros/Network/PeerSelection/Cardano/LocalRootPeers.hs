{-# LANGUAGE NamedFieldPuns     #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications   #-}

module Test.Ouroboros.Network.PeerSelection.Cardano.LocalRootPeers (tests) where

import Data.Map.Strict qualified as Map

import Cardano.Network.PeerSelection.PeerTrustable (PeerTrustable)
import Ouroboros.Network.PeerSelection.State.LocalRootPeers
           (LocalRootPeers (..))
import Ouroboros.Network.PeerSelection.State.LocalRootPeers qualified as LocalRootPeers
import Test.Ouroboros.Network.PeerSelection.Cardano.Instances ()
import Test.Ouroboros.Network.PeerSelection.Instances (PeerAddr)
import Test.Ouroboros.Network.PeerSelection.LocalRootPeers ()
import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup "Ouroboros.Network.PeerSelection.Cardano"
  [ testGroup "LocalRootPeers"
    [ testProperty "clampToTrustable" prop_clampToTrustable
    ]
  ]

prop_clampToTrustable :: LocalRootPeers PeerTrustable PeerAddr -> Property
prop_clampToTrustable localRootPeers =

    let trustedPeers = LocalRootPeers.keysSet
                     $ LocalRootPeers.clampToTrustable localRootPeers

     in counterexample (show $ Map.restrictKeys (LocalRootPeers.toMap localRootPeers) trustedPeers)
      $ all (`LocalRootPeers.isPeerTrustable` localRootPeers) trustedPeers

