{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Test.Ouroboros.Network.PeerSelection.Utils where

import Control.Monad.Class.MonadTime.SI
import Data.Set (Set)
import Data.Set qualified as Set
import System.Random (mkStdGen)

import Ouroboros.Network.PeerSelection.Governor (AssociationMode (..),
           PeerSelectionTargets (..), TracePeerSelection (..))
import Ouroboros.Network.PeerSelection.Governor qualified as Governor
import Ouroboros.Network.PeerSelection.PublicRootPeers qualified as PublicRootPeers

import Test.Ouroboros.Network.Data.Signal (Events, Signal)
import Test.Ouroboros.Network.Data.Signal qualified as Signal

import Test.Ouroboros.Network.PeerSelection.Cardano.MockEnvironment hiding
           (targets, tests)
import Test.Ouroboros.Network.PeerSelection.Instances


takeFirstNHours :: DiffTime -> [(Time, a)] -> [(Time, a)]
takeFirstNHours h = takeWhile (\(t,_) -> t < Time (60*60*h))


selectEnvEvents :: Events (TestTraceEvent extraState extraFlags extraPeers extraCounters) -> Events TraceMockEnv
selectEnvEvents = Signal.selectEvents
                    (\case MockEnvEvent e -> Just $! e
                           _              -> Nothing)

selectGovEvents :: Events (TestTraceEvent extraState extraFlags extraPeers extracounters)
                -> Events (TracePeerSelection extraState extraFlags extraPeers PeerAddr)
selectGovEvents = Signal.selectEvents
                    (\case GovernorEvent e -> Just $! e
                           _               -> Nothing)

selectGovCounters :: Events (TestTraceEvent extraState extraFlags extraPeers extraCounters)
                  -> Events (Governor.PeerSelectionCounters extraCounters)
selectGovCounters = Signal.selectEvents
                      (\case GovernorCounters e -> Just $! e
                             _                  -> Nothing)

selectGovAssociationMode :: Events (TestTraceEvent extraState extraFlags extraPeers extraCounters)
                         -> Events AssociationMode
selectGovAssociationMode = Signal.selectEvents
                             (\case GovernorAssociationMode e -> Just $! e
                                    _                         -> Nothing)

selectGovState :: Eq a
               => (forall peerconn. Governor.PeerSelectionState extraState extraFlags extraPeers PeerAddr peerconn -> a)
               -> extraState
               -> extraPeers
               -> Events (TestTraceEvent extraState extraFlags extraPeers extraCounters)
               -> Signal a
selectGovState f es ep =
    Signal.nub
  -- TODO: #3182 Rng seed should come from quickcheck.
  --       and `NumberOfBigLedgerPeers`
  . Signal.fromChangeEvents (f $! Governor.emptyPeerSelectionState (mkStdGen 42) es ep)
  . Signal.selectEvents
      (\case GovernorDebug (Governor.TraceGovernorState _ _ st) -> Just $! f st
             _                                                  -> Nothing)

selectEnvTargets :: Eq a
                 => (PeerSelectionTargets -> a)
                 -> Events (TestTraceEvent extraState extraFlags extraPeers extraCounters)
                 -> Signal a
selectEnvTargets f =
    Signal.nub
  . fmap f
  . Signal.fromChangeEvents Governor.nullPeerSelectionTargets
  . Signal.selectEvents
      (\case TraceEnvSetTargets targets -> Just $! targets
             _                          -> Nothing)
  . selectEnvEvents


-- | filter big ledger peers
--
takeBigLedgerPeers
    :: (Governor.PeerSelectionState extraState extraFlags extraPeers PeerAddr peerconn -> Set PeerAddr)
    ->  Governor.PeerSelectionState extraState extraFlags extraPeers PeerAddr peerconn -> Set PeerAddr
takeBigLedgerPeers f =
  \st -> f st `Set.intersection` (PublicRootPeers.getBigLedgerPeers . Governor.publicRootPeers) st

-- | filter out big ledger peers
--
dropBigLedgerPeers
    :: (Governor.PeerSelectionState extraState extraFlags extraPeers PeerAddr peerconn -> Set PeerAddr)
    ->  Governor.PeerSelectionState extraState extraFlags extraPeers PeerAddr peerconn -> Set PeerAddr
dropBigLedgerPeers f =
  \st -> f st Set.\\ (PublicRootPeers.getBigLedgerPeers . Governor.publicRootPeers) st
