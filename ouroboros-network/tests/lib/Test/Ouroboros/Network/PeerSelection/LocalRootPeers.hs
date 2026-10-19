{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Ouroboros.Network.PeerSelection.LocalRootPeers
  ( arbitraryLocalRootPeers
  , AdvertiseChange
  , governorAdvertiseChanges
  , restrictKeys
  , tests
  , LocalRootPeers (..)
  , HotValency (..)
  , WarmValency (..)
  ) where

import Control.Applicative ((<|>))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe.Strict (StrictMaybe (..))
import System.Random (mkStdGen)

import Control.Monad.Class.MonadSTM (STM, atomically)
import Control.Monad.Class.MonadTime.SI (Time (..))
import Control.Monad.IOSim (IOSim, runSimOrThrow)
import Data.Set (Set)
import Data.Set qualified as Set

import Ouroboros.Network.PeerSelection.Governor
import Ouroboros.Network.PeerSelection.Governor.Monitor qualified as Monitor
import Ouroboros.Network.PeerSelection.Governor.Types (Guarded (..))
import Ouroboros.Network.PeerSelection.Governor.Types qualified as Governor
import Ouroboros.Network.PeerSelection.PeerAdvertise (PeerAdvertise (..))
import Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing)
import Ouroboros.Network.PeerSelection.State.KnownPeers (KnownPeerInfo (..))
import Ouroboros.Network.PeerSelection.State.KnownPeers qualified as KnownPeers
import Ouroboros.Network.PeerSelection.State.LocalRootPeers (HotValency (..),
           LocalRootConfig (..), LocalRootPeers (..), WarmValency (..))
import Ouroboros.Network.PeerSelection.State.LocalRootPeers qualified as LocalRootPeers
import Ouroboros.Network.PeerSelection.Types (PublicExtraPeersAPI,
           nullPublicExtraPeersAPI)

import Test.Ouroboros.Network.PeerSelection.Instances
import Test.Ouroboros.Network.Utils (ShrinkCarefully, prop_shrink_nonequal,
           prop_shrink_valid, renderRanges)

import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)


tests :: TestTree
tests =
  testGroup "Ouroboros.Network.PeerSelection"
  [ testGroup "LocalRootPeers"
    [ testProperty "arbitrary"    (prop_arbitrary_LocalRootPeers @NoExtraFlags)
    , testProperty "fromToGroups" (prop_fromToGroups @NoExtraFlags)
    , testProperty "fromGroups"   (prop_fromGroups @NoExtraFlags)
    , testProperty "shrink"       (prop_shrink_LocalRootPeers @NoExtraFlags)
    , testProperty "clampToLimit" (prop_clampToLimit @NoExtraFlags)
    , testProperty "the governor gives the known peers the new flags"
                   prop_governor_advertiseChanges
    ]
  ]

type NoExtraFlags = ()

-- | Check that we can forcibly bring the localRootPeers into compliance
-- with the governor's state invariant, which requires that the local
-- root peers fit within the current targets.
--
-- The precondition on the targets is that they are sane.
-- See sanePeerSelectionTargets.
prop_clampToLimit
  :: Arbitrary extraFlags
  => LocalRootPeers extraFlags PeerAddr
  -> PeerSelectionTargets
  -> Property
prop_clampToLimit localRootPeers targets =

    let sizeLimit       = targetNumberOfKnownPeers targets
        localRootPeers' = LocalRootPeers.clampToLimit sizeLimit localRootPeers

     in counterexample ("sizeLimit   = " ++ show sizeLimit) $

        LocalRootPeers.size localRootPeers'
          === min sizeLimit
                 (LocalRootPeers.size localRootPeers)

-- | A change of local roots, with the known peers before it: the old local
-- roots with their flags, and other peers.
data AdvertiseChange extraFlags = AdvertiseChange {
    acOld    :: LocalRootPeers extraFlags PeerAddr,
    acNew    :: LocalRootPeers extraFlags PeerAddr,
    acOthers :: Map PeerAddr (Maybe PeerSharing, Maybe PeerAdvertise)
  }
  deriving Show

-- | The new local roots are mostly an edit of the old ones: entries dropped,
-- flags flipped, a group of new peers added. Shrinking removes a peer from
-- both, or undoes a flag's change, so a kept peer stays kept.
instance (Arbitrary extraFlags, Eq extraFlags)
      => Arbitrary (AdvertiseChange extraFlags) where
    arbitrary = do
      old <- arbitrary
      new <- frequency
        [ (1, arbitrary)
        , (4, do groups <- traverse edit (LocalRootPeers.toGroups old)
                 extra  <- frequency [ (2, pure [])
                                     , (1, LocalRootPeers.toGroups
                                             <$> arbitraryLocalRootPeers (fresh old)) ]
                 return (LocalRootPeers.fromGroups (groups ++ extra)))
        ]
      AdvertiseChange old new <$> arbitrary
      where
        fresh lrps = Set.fromList [ PeerAddr (top + i) | i <- [1 .. 3] ]
          where top = maximum (0 : [ a | PeerAddr a <- Set.toList (LocalRootPeers.keysSet lrps) ])
        edit (h, w, m) = do
          m' <- Map.fromList <$> (sublistOf (Map.toList m) >>= traverse flip')
          return (h, w, m')
        flip' (p, c) = do
          b <- arbitrary
          return (p, if b then c { peerAdvertise = otherAdvertise (peerAdvertise c) } else c)

    shrink (AdvertiseChange old new others) =
         [ AdvertiseChange (without p old) (without p new) others
         | p <- Set.toList (LocalRootPeers.keysSet old <> LocalRootPeers.keysSet new) ]
      ++ [ AdvertiseChange old (restore p a) others
         | (p, a) <- Map.toList (Map.intersectionWith (\o n -> (peerAdvertise o, peerAdvertise n))
                                                      (LocalRootPeers.toMap old)
                                                      (LocalRootPeers.toMap new))
                       >>= \(p, (o, n)) -> [ (p, o) | o /= n ] ]
      ++ [ AdvertiseChange old new others' | others' <- shrink others ]
      where
        without p lrps = restrictKeys lrps (Set.delete p (LocalRootPeers.keysSet lrps))
        restore p a = LocalRootPeers.fromGroups
                        [ (h, w, Map.adjust (\c -> c { peerAdvertise = a }) p m)
                        | (h, w, m) <- LocalRootPeers.toGroups new ]

otherAdvertise :: PeerAdvertise -> PeerAdvertise
otherAdvertise DoAdvertisePeer    = DoNotAdvertisePeer
otherAdvertise DoNotAdvertisePeer = DoAdvertisePeer

-- | The known peers before the change: the governor inserted each old local
-- root with its flag.
knownPeersBefore :: AdvertiseChange extraFlags -> KnownPeers.KnownPeers PeerAddr
knownPeersBefore AdvertiseChange { acOld, acOthers } =
    KnownPeers.insert (Map.map (\c -> (Nothing, Just (peerAdvertise c)))
                               (LocalRootPeers.toMap acOld))
                      (KnownPeers.insert acOthers KnownPeers.empty)

-- | The kinds of change the properties must see.
coverAdvertiseChange :: AdvertiseChange extraFlags -> Property -> Property
coverAdvertiseChange AdvertiseChange { acOld, acNew } =
      cover 5 (any (\(o, n) -> peerAdvertise o == DoAdvertisePeer
                               && peerAdvertise n == DoNotAdvertisePeer) kept)
              "advertising revoked"
    . cover 5 (any (\(o, n) -> peerAdvertise o == DoNotAdvertisePeer
                               && peerAdvertise n == DoAdvertisePeer) kept)
              "advertising granted"
    . cover 5 (not (Map.null (newMap Map.\\ oldMap))) "a peer added"
    . cover 5 (not (Map.null (oldMap Map.\\ newMap))) "a peer removed"
  where
    newMap = LocalRootPeers.toMap acNew
    oldMap = LocalRootPeers.toMap acOld
    kept   = Map.intersectionWith (,) oldMap newMap

-- | The known peers after the change hold the new flag of each new local
-- root, and nothing changed about any other peer. With every peer connected
-- once and never failed, a new local root is then shared exactly when it is
-- advertised: one whose flag was revoked is no longer.
knownPeersAfter :: AdvertiseChange extraFlags
                -> KnownPeers.KnownPeers PeerAddr
                -> Property
knownPeersAfter change@AdvertiseChange { acNew } known1 =
       conjoin [ counterexample (show p ++ " holds " ++ show (knownPeerAdvertise <$> info)
                                 ++ ", configured " ++ show (peerAdvertise c))
                   (fmap knownPeerAdvertise info == Just (SJust (peerAdvertise c)))
               | (p, c) <- Map.toList newMap
               , let info = Map.lookup p (KnownPeers.allPeers known1) ]
  .&&. conjoin [ counterexample (show p ++ " changed: " ++ show info0
                                 ++ " became " ++ show info1)
                   (Just info0 == info1)
               | (p, info0) <- Map.toList (KnownPeers.allPeers known0)
               , p `Map.notMember` newMap
               , let info1 = Map.lookup p (KnownPeers.allPeers known1) ]
  .&&. conjoin [ counterexample (show p ++ " shared: " ++ show (p `Set.member` shared))
                   ((p `Set.member` shared) == (peerAdvertise c == DoAdvertisePeer))
               | (p, c) <- Map.toList newMap ]
  where
    known0 = knownPeersBefore change
    newMap = LocalRootPeers.toMap acNew
    shared = KnownPeers.getPeerSharingResponsePeers
               (KnownPeers.setSuccessfulConnectionFlag
                  (KnownPeers.toSet known1) known1)

-- | The governor's local roots monitor, from a state holding the old local
-- roots and known peers with their flags, reads the new ones and leaves the
-- known peers as 'knownPeersAfter' requires; it does not fire when nothing
-- changed.
prop_governor_advertiseChanges :: AdvertiseChange NoExtraFlags -> Property
prop_governor_advertiseChanges =
    governorAdvertiseChanges Monitor.localRoots () () nullPublicExtraPeersAPI

-- | A governor's local roots monitor, from a state holding the old local
-- roots and known peers with their flags, reads the new ones and leaves the
-- known peers as 'knownPeersAfter' requires; it does not fire when nothing
-- changed. The monitor runs once, on a state with the given extra state and
-- extra peers.
governorAdvertiseChanges
  :: forall extraState extraFlags extraPeers.
     (Eq extraFlags, Show extraFlags)
  => (forall s. Governor.PeerSelectionActions extraState extraFlags extraPeers ()
                                               PeerAddr () (IOSim s)
             -> Governor.PeerSelectionState extraState extraFlags extraPeers PeerAddr ()
             -> Guarded (STM (IOSim s))
                        (Governor.TimedDecision (IOSim s) extraState () extraFlags extraPeers
                                                PeerAddr ()))
  -> extraState
  -> extraPeers
  -> PublicExtraPeersAPI extraPeers PeerAddr
  -> AdvertiseChange extraFlags
  -> Property
governorAdvertiseChanges monitor extraState extraPeers extraPeersAPI
                         change@AdvertiseChange { acOld, acNew } =
    checkCoverage $
    coverAdvertiseChange change $
    case runSimOrThrow run of
      Nothing     -> counterexample "the monitor did not fire" (acNew === acOld)
      Just known1 -> knownPeersAfter change known1
  where
    targets = nullPeerSelectionTargets { targetNumberOfKnownPeers = 10000 }

    run :: IOSim s (Maybe (KnownPeers.KnownPeers PeerAddr))
    run =
      case monitor actions state of
        Guarded _ decide ->
          atomically $ (Just . knownPeersOf . ($ Time 0) <$> decide) <|> pure Nothing
        GuardedSkip _ -> return Nothing

    knownPeersOf :: Governor.Decision (IOSim s) extraState () extraFlags extraPeers PeerAddr ()
                 -> KnownPeers.KnownPeers PeerAddr
    knownPeersOf = Governor.knownPeers . Governor.decisionState

    state :: Governor.PeerSelectionState extraState extraFlags extraPeers PeerAddr ()
    state = (Governor.emptyPeerSelectionState (mkStdGen 0) extraState extraPeers) {
              Governor.localRootPeers = acOld,
              Governor.knownPeers     = knownPeersBefore change,
              Governor.targets        = targets
            }

    actions :: Governor.PeerSelectionActions extraState extraFlags extraPeers () PeerAddr () (IOSim s)
    actions = Governor.PeerSelectionActions {
                Governor.peerSelectionTargets       = targets,
                Governor.readPeerSelectionTargets   = pure targets,
                Governor.readLocalRootPeersFromFile = error "readLocalRootPeersFromFile",
                Governor.readLocalRootPeers         = pure (LocalRootPeers.toGroups acNew),
                Governor.readInboundPeers           = error "readInboundPeers",
                Governor.peerSharing                = error "peerSharing",
                Governor.peerConnToPeerSharing      = error "peerConnToPeerSharing",
                Governor.extraPeersAPI              = extraPeersAPI,
                Governor.requestPublicRootPeers     = error "requestPublicRootPeers",
                Governor.requestPeerShare           = error "requestPeerShare",
                Governor.peerStateActions           = error "peerStateActions",
                Governor.getLedgerStateCtx          = error "getLedgerStateCtx",
                Governor.readLedgerPeerSnapshot     = error "readLedgerPeerSnapshot"
              }

arbitraryLocalRootPeers :: (Ord peeraddr, Arbitrary extraFlags)
                        => Set peeraddr -> Gen (LocalRootPeers extraFlags peeraddr)
arbitraryLocalRootPeers peeraddrs = do
    -- divide into a few disjoint groups
    ngroups     <- choose (1, 5 :: Int)
    gassignment <- vectorOf (Set.size peeraddrs) (choose (1, ngroups))
    advertise   <- vectorOf (Set.size peeraddrs) arbitrary
    let groups = Map.elems $
                 Map.fromListWith Map.union
                   [ (gn, Map.singleton p a)
                   | (p, gn, a) <- zip3 (Set.toList peeraddrs)
                                         gassignment
                                         advertise
                   ]
    targets <- mapM (\g -> do
                      warmValency <- WarmValency <$> choose (0, Map.size g)
                      hotValency <- HotValency <$> choose (0, getWarmValency warmValency)
                      return (hotValency, warmValency)
                    ) groups

    return (LocalRootPeers.fromGroups (zipWith (\(h, w) g -> (h, w, g))
                                               targets
                                               groups))

instance (Arbitrary extraFlags, Arbitrary peeraddr, Ord peeraddr) =>
         Arbitrary (LocalRootPeers extraFlags peeraddr) where
    arbitrary = do
        peeraddrs <- scale (`div` 4) arbitrary
        arbitraryLocalRootPeers peeraddrs

    shrink lrps =
        map LocalRootPeers.fromGroups (shrink (LocalRootPeers.toGroups lrps))

restrictKeys :: Ord peeraddr
             => LocalRootPeers extraFlags peeraddr
             -> Set peeraddr
             -> LocalRootPeers extraFlags peeraddr
restrictKeys lrps ks =
    LocalRootPeers.fromGroups
  . map (\(h, w, g) -> (h, w, Map.restrictKeys g ks))
  . LocalRootPeers.toGroups
  $ lrps

prop_arbitrary_LocalRootPeers :: Arbitrary extraFlags => LocalRootPeers extraFlags PeerAddr -> Property
prop_arbitrary_LocalRootPeers lrps =
    tabulate "total size"    [size] $
    tabulate "num groups"    [numGroups] $
    tabulate "group size"    sizeGroups $
    tabulate "targets"       targets $

    LocalRootPeers.invariant lrps
  where
    thrd (_, _, c) = c
    size       = renderRanges 5 (LocalRootPeers.size lrps)
    numGroups  = show (length (LocalRootPeers.toGroups lrps))
    sizeGroups = map (show . Set.size . thrd) (LocalRootPeers.toGroupSets lrps)
    targets    = [ case () of
                    _ | h == 0                        -> "none active"
                      | w == 0                        -> "none established"
                      | h == HotValency (Set.size g)  -> "all active"
                      | w == WarmValency (Set.size g) -> "all established"
                      | otherwise                     -> "some"
                 | (h, w, g) <- LocalRootPeers.toGroupSets lrps ]


prop_shrink_LocalRootPeers
  :: ( Eq extraFlags
     , Show extraFlags
     , Arbitrary extraFlags
     )
  => ShrinkCarefully (LocalRootPeers extraFlags PeerAddr)
  -> Property
prop_shrink_LocalRootPeers x =
      prop_shrink_valid LocalRootPeers.invariant x
 .&&. prop_shrink_nonequal x

prop_fromGroups :: [(HotValency, WarmValency, Map PeerAddr (LocalRootConfig extraFlags))]
                -> Bool
prop_fromGroups = LocalRootPeers.invariant . LocalRootPeers.fromGroups

prop_fromToGroups
  :: Eq extraFlags
  => LocalRootPeers extraFlags PeerAddr
  -> Bool
prop_fromToGroups lrps =
    LocalRootPeers.fromGroups (LocalRootPeers.toGroups lrps) == lrps

