{-# LANGUAGE ScopedTypeVariables #-}

module PeerSelection where

import Algebra.Graph.Labelled.AdjacencyMap
import Algebra.Graph.Labelled.AdjacencyMap.Builder

import Control.Monad (forM, forM_)

import Ouroboros.Network.PeerSelection.Governor
import Ouroboros.Network.PeerSelection.Test as Test
import Ouroboros.Network.PeerSelection.Types
import Control.Monad.IOSim
import Control.Monad.Class.MonadTime
import qualified Ouroboros.Network.PeerSelection.KnownPeers as KnownPeers

import qualified Data.List.NonEmpty as NE
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map.Strict as Map

import Control.Monad.Class.MonadSTM
import Data.List.NonEmpty
import Data.Void (Void)

vertexRepToPeerAddr :: VertexRep -> PeerAddr
vertexRepToPeerAddr word32 = PeerAddr (fromIntegral word32)

-- Assumes the PeerAddr is not negative.
peerAddrToVertexRep :: PeerAddr -> VertexRep
peerAddrToVertexRep (PeerAddr int) = fromIntegral int

-- This one is in `m` because we have to set up some mutable cells.
-- A breadth-first policy: new entries to the map (inferred by way of retry and map differences)
-- are enqueued, and the head of the queue is used to generate the next requests.
bfsGossipPolicy :: (Ord peeraddr, MonadSTM m) => m (Map peeraddr KnownPeers.KnownPeerInfo -> Int -> STM m (NonEmpty peeraddr))
bfsGossipPolicy = do
  (queue, candidatesVar) <- atomically ((,) <$> newTQueue <*> newTVar mempty)
  pure (bfsGossipWithQueue queue candidatesVar)

bfsGossipWithQueue
  :: (Ord peeraddr, MonadSTM m)
  => TQueue m peeraddr
  -> TVar m (Map peeraddr KnownPeers.KnownPeerInfo)
  -> Map peeraddr KnownPeers.KnownPeerInfo
  -> Int
  -> STM m (NonEmpty peeraddr)
bfsGossipWithQueue queue candidatesVar newCandidates maxReqs = do
  candidates <- readTVar candidatesVar
  let diff = newCandidates Map.\\ candidates
      -- We don't care about the peer info, at least not in this iteration.
      shouldContact = Set.toList (Map.keysSet diff)
  -- To make it DFS, we'd use an STM stack (TVar [peeraddr])
  forM_ shouldContact (writeTQueue queue)
  toContact <- forM [1..(maxReqs-1)] (const (readTQueue queue))
  case Data.List.NonEmpty.nonEmpty toContact of
    -- This means that either
    --   maxReqs <= 0, so whatever, we're never going to make progress
    --   shouldContact = [], else the queue would certainly not be empty when we read it
    Nothing -> retry
    Just it -> do
      writeTVar candidatesVar newCandidates
      pure it

selectedPeerSets
  :: PeerSelectionTargets
  -> DiffTime -- Ignore anything that happens after this amount of "time" in hours passes (the governor runs "forever")
  -> AdjacencyMap anyEdgeLabel VertexRep
  -> NonEmpty (Set VertexRep)
selectedPeerSets targets delta gr
  = fmap (Set.map peerAddrToVertexRep)
  . Data.List.NonEmpty.reverse
  . Prelude.foldl collect (pure Set.empty)
  . takeFirstNHours delta
  . selectPeerSelectionTraceEvents
  $ runSimTrace act
  
  where
  
  act :: MonadSTM m => m Void
  act = undefined {- do
    actions <- pure mkPeerSelectionActions
    policy  <- pure mkPeerSelectionPolicy
    -- dynamicTracer is from the PeerSelection.Test module
    --peerSelectionGovernor dynamicTracer actions policy
    pure undefined -}
  
  mkPeerSelectionActions :: MonadSTM m => PeerSelectionActions PeerAddr m
  mkPeerSelectionActions = undefined
  
  mkPeerSelectionPolicy :: MonadSTM m => PeerSelectionPolicy PeerAddr m
  mkPeerSelectionPolicy  = undefined
  
  collect :: NonEmpty (Set PeerAddr) -> (Time, TracePeerSelection PeerAddr) -> NonEmpty (Set PeerAddr)
  collect (s :| ss) (_, TraceGovernorLoopDebug st _) =
    if Map.keysSet (KnownPeers.toMap (knownPeers st)) == s
    then s :| ss
    else Map.keysSet (KnownPeers.toMap (knownPeers st)) Data.List.NonEmpty.:| (s : ss)
  collect ss _ = ss
