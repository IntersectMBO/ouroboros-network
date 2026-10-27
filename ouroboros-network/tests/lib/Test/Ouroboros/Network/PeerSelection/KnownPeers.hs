module Test.Ouroboros.Network.PeerSelection.KnownPeers (tests) where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
import NoThunks.Class (unsafeNoThunks)
import System.Random (mkStdGen)

import Ouroboros.Network.PeerSelection.Governor.Types (PeerSelectionState (..),
           PublicPeerSelectionState (..), emptyPeerSelectionState,
           toPublicState)
import Ouroboros.Network.PeerSelection.LedgerPeers (RelayAccessPoint)
import Ouroboros.Network.PeerSelection.PeerAdvertise (PeerAdvertise)
import Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing)
import Ouroboros.Network.PeerSelection.State.KnownPeers qualified as KnownPeers
import Ouroboros.Network.PeerSelection.State.LocalRootPeers (LocalRootPeers)

import Test.Ouroboros.Network.PeerSelection.Instances (PeerAddr)
import Test.Ouroboros.Network.PeerSelection.LocalRootPeers ()
import Test.QuickCheck (Property, checkCoverage, counterexample, cover,
           forAllShrink, property, shrinkList, sublistOf)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup "Ouroboros.Network.PeerSelection"
  [ testGroup "KnownPeers"
    [ testProperty "insert is idempotent" prop_insert_idempotent
    ]
  , testGroup "PublicPeerSelectionState"
    [ testProperty "built strict: no thunk once in weak head normal form"
                   prop_toPublicState_noThunks
    ]
  ]

-- | The governor's public state is strict: forced to weak head normal form,
-- as the write to its variable forces it, it holds no thunk, so a write
-- keeps neither the known peers nor the local roots it was computed from
-- alive until a reader comes.  Some of the known peers have connected
-- once, as the sharing filter asks of a peer it shares: a fresh one is
-- never shared, and a state sharing nobody holds an empty set whatever
-- it was computed from.
prop_toPublicState_noThunks
  :: Map PeerAddr (Maybe PeerSharing, Maybe PeerAdvertise)
  -> LocalRootPeers () PeerAddr
  -> Property
prop_toPublicState_noThunks m roots =
  forAllShrink (sublistOf (Map.keys m)) (shrinkList (const [])) $ \connected ->
  let st :: PeerSelectionState () () () PeerAddr ()
      st = (emptyPeerSelectionState (mkStdGen 0) () ())
             { knownPeers     = KnownPeers.setSuccessfulConnectionFlag
                                  (Set.fromList connected)
                                  (KnownPeers.insert m KnownPeers.empty)
             , localRootPeers = roots }
      public = toPublicState st
      -- the verdict before the coverage: reading a field for the coverage
      -- would force a thunk in it away before the check
      verdict = public `seq` unsafeNoThunks public
   in verdict `seq`
      checkCoverage $
      cover 30 (not (Set.null (availableToShare public))) "shares a peer" $
      cover 30 (not (Set.null (localRootAddrs public))) "has a local root" $
      case verdict of
        Nothing   -> property True
        Just info -> counterexample ("a thunk in the public state: " ++ show info) False

prop_insert_idempotent
  :: Map RelayAccessPoint (Maybe PeerSharing, Maybe PeerAdvertise)
  -> Property
prop_insert_idempotent m =
  let known  = KnownPeers.insert m KnownPeers.empty
      known' = KnownPeers.insert m known
   in counterexample ("KnownPeers 1: " ++ show known ++ "\n"
                    ++ "KnownPeers 2: " ++ show known') $
      known == known'

