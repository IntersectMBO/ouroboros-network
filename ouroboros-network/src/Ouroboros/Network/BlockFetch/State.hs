{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}

module Ouroboros.Network.BlockFetch.State
  ( fetchLogicIterations
  , FetchDecisionPolicy (..)
  , FetchTriggerVariables (..)
  , FetchNonTriggerVariables (..)
  , FetchDecision
  , FetchDecline (..)
  , FetchMode (..)
  , TraceLabelPeer (..)
  , TraceFetchClientState (..)
  ) where

import Data.Foldable (for_)
import Data.Functor.Contravariant (contramap)
import Data.Hashable (Hashable)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Void

import Control.Concurrent.Class.MonadSTM.Strict.TVar (modifyTVar)
import Control.Concurrent.Class.MonadSTM.Strict.TVar.Checked (newTVarIO, StrictTVar, readTVarIO, writeTVar)
import Control.Exception (assert)
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Tracer (Tracer, traceWith)

import Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import Ouroboros.Network.AnchoredFragment qualified as AF
import Ouroboros.Network.Block

import Ouroboros.Network.BlockFetch.ClientState (FetchClientStateVars (..),
           FetchRequest (..), PeerFetchInFlight (..), PeerFetchStatus (..),
           TraceFetchClientState (..), TraceLabelPeer (..), addNewFetchRequest,
           readFetchClientState, PeersOrder (..), PeerFetchBlockInFlight (..))
import Ouroboros.Network.BlockFetch.Decision (FetchDecision,
           FetchDecisionPolicy (..), FetchDecline (..), FetchMode (..),
           PeerInfo, fetchDecisions)
import Ouroboros.Network.BlockFetch.DeltaQ (PeerGSV (..))
import Ouroboros.Network.BlockFetch.ConsensusInterface (ChainSelStarvation)


fetchLogicIterations
  :: ( HasHeader header
     , HasHeader block
     , HeaderHash header ~ HeaderHash block
     , MonadDelay m
     , MonadSTM m
     , Ord peer
     , Hashable peer
     )
  => Tracer m [TraceLabelPeer peer (FetchDecision [Point header])]
  -> Tracer m (TraceLabelPeer peer (TraceFetchClientState header))
  -> FetchDecisionPolicy header
  -> FetchTriggerVariables peer header m
  -> FetchNonTriggerVariables peer header block m
  -> (peer -> m ()) -- ^ Action to call to demote the dynamo of ChainSync jumping.
  -> m Void
fetchLogicIterations decisionTracer clientStateTracer
                     fetchDecisionPolicy
                     fetchTriggerVariables
                     fetchNonTriggerVariables
                     demoteCSJDynamo = do

    peersOrderVar <- newTVarIO $ PeersOrder {
      peersOrderStart = Time 0,
      peersOrderAll = []
      }

    iterateForever initialFetchStateFingerprint $ \stateFingerprint -> do

      -- Run a single iteration of the fetch logic:
      --
      -- + wait for the state to change and make decisions for the new state
      -- + act on those decisions
      start <- getMonotonicTime
      stateFingerprint' <- fetchLogicIteration
        decisionTracer clientStateTracer
        fetchDecisionPolicy
        fetchTriggerVariables
        fetchNonTriggerVariables
        stateFingerprint
        (peersOrderVar, demoteCSJDynamo)
      end <- getMonotonicTime
      let delta = diffTime end start
      -- Limit decision is made once every decisionLoopInterval.
      threadDelay $ decisionLoopInterval fetchDecisionPolicy - delta
      return stateFingerprint'


iterateForever :: Monad m => a -> (a -> m a) -> m Void
iterateForever x0 m = go x0 where go x = m x >>= go


-- | A single iteration of the fetch logic.
--
-- This involves:
--
-- * waiting for the state that the fetch decisions depend upon to change;
-- * taking a snapshot of the state;
-- * deciding for each peer if we will initiate a new fetch request
--
fetchLogicIteration
  :: (Hashable peer, MonadSTM m, Ord peer,
      HasHeader header, HasHeader block,
      HeaderHash header ~ HeaderHash block,
      MonadMonotonicTime m)
  => Tracer m [TraceLabelPeer peer (FetchDecision [Point header])]
  -> Tracer m (TraceLabelPeer peer (TraceFetchClientState header))
  -> FetchDecisionPolicy header
  -> FetchTriggerVariables peer header m
  -> FetchNonTriggerVariables peer header block m
  -> FetchStateFingerprint peer header block
  -> (StrictTVar m (PeersOrder peer), peer -> m ())
  -> m (FetchStateFingerprint peer header block)
fetchLogicIteration decisionTracer clientStateTracer
                    fetchDecisionPolicy
                    fetchTriggerVariables
                    fetchNonTriggerVariables
                    stateFingerprint
                    (peersOrderVar, demoteCSJDynamo) = do

    -- Gather a snapshot of all the state we need.
    (stateSnapshot, stateFingerprint') <-
      atomically $
        readStateVariables
          fetchTriggerVariables
          fetchNonTriggerVariables
          stateFingerprint
    peersOrder <- readTVarIO peersOrderVar

    -- TODO: allow for boring PeerFetchStatusBusy transitions where we go round
    -- again rather than re-evaluating everything.
    assert (stateFingerprint' /= stateFingerprint) $ return ()

    -- TODO: log the difference in the fingerprint that caused us to wake up

    -- Make all the fetch decisions
    decisions <- fetchDecisionsForStateSnapshot
                      fetchDecisionPolicy
                      stateSnapshot
                      (peersOrder,
                       atomically . writeTVar peersOrderVar,
                       demoteCSJDynamoAndIgnoreInflightBlocks)

    -- If we want to trace timings, we can do it here after forcing:
    -- _ <- evaluate (force decisions)

    -- Trace the batch of fetch decisions
    traceWith decisionTracer
      [ TraceLabelPeer peer (fmap fetchRequestPoints decision)
      | (decision, (_, _, _, peer, _)) <- decisions ]

    -- Tell the fetch clients to act on our decisions
    statusUpdates <- fetchLogicIterationAct clientStateTracer
                                            fetchDecisionPolicy
                                            (map swizzleReqVar decisions)
    let !stateFingerprint'' =
          updateFetchStateFingerprintPeerStatus statusUpdates stateFingerprint'

    return stateFingerprint''
  where
    swizzleReqVar (d,(_,_,g,_,(rq,p))) = (d,g,rq,p)

    fetchRequestPoints :: HasHeader hdr => FetchRequest hdr -> [Point hdr]
    fetchRequestPoints (FetchRequest headerss) =
      -- Flatten multiple fragments and trace points, not full headers
      [ blockPoint header
      | headers <- headerss
      , header  <- AF.toOldestFirst headers ]

    demoteCSJDynamoAndIgnoreInflightBlocks peer = do
      demoteCSJDynamo peer
      atomically $ do
        peerStateVars <- readStatePeerStateVars fetchNonTriggerVariables
        for_ peerStateVars $ \peerStateVar ->
          modifyTVar (fetchClientInFlightVar peerStateVar) $ \pfif ->
            pfif { peerFetchBlocksInFlight =
                     fmap (const (PeerFetchBlockInFlight True)) (peerFetchBlocksInFlight pfif)
                 }

-- | Do a bit of rearranging of data before calling 'fetchDecisions' to do the
-- real work.
--
fetchDecisionsForStateSnapshot
  :: (HasHeader header,
      HeaderHash header ~ HeaderHash block,
      Ord peer,
      Hashable peer,
      MonadMonotonicTime m)
  => FetchDecisionPolicy header
  -> FetchStateSnapshot peer header block m
  -> ( PeersOrder peer
     , PeersOrder peer -> m ()
     , peer -> m ()
     )
  -> m [( FetchDecision (FetchRequest header),
        PeerInfo header peer (FetchClientStateVars m header, peer)
      )]

fetchDecisionsForStateSnapshot
    fetchDecisionPolicy
    FetchStateSnapshot {
      fetchStateCurrentChain,
      fetchStatePeerChains,
      fetchStatePeerStates,
      fetchStatePeerGSVs,
      fetchStateFetchedBlocks,
      fetchStateFetchedMaxSlotNo,
      fetchStateFetchMode,
      fetchStateChainSelStarvation
    }
    peersOrderHandlers =
    assert (                 Map.keysSet fetchStatePeerChains
            `Set.isSubsetOf` Map.keysSet fetchStatePeerStates) $

    assert (                 Map.keysSet fetchStatePeerStates
            `Set.isSubsetOf` Map.keysSet fetchStatePeerGSVs) $

    fetchDecisions
      fetchDecisionPolicy
      fetchStateFetchMode
      fetchStateCurrentChain
      fetchStateFetchedBlocks
      fetchStateFetchedMaxSlotNo
      fetchStateChainSelStarvation
      peersOrderHandlers
      peerChainsAndPeerInfo
  where
    peerChainsAndPeerInfo =
      map swizzle . Map.toList $
      Map.intersectionWith (,)
        (Map.intersectionWith (,) fetchStatePeerChains fetchStatePeerStates)
        fetchStatePeerGSVs

    swizzle (peer, ((chain, (status, inflight, vars)), gsvs)) =
      (chain, (status, inflight, gsvs, peer, (vars, peer)))


-- | Act on decisions to send new requests. In fact all we do here is update
-- request variables that are shared with the threads running the block fetch
-- protocol with each peer.
--
fetchLogicIterationAct :: (MonadSTM m, HasHeader header)
                       => Tracer m (TraceLabelPeer peer (TraceFetchClientState header))
                       -> FetchDecisionPolicy header
                       -> [(FetchDecision (FetchRequest header),
                            PeerGSV,
                            FetchClientStateVars m header,
                            peer)]
                       -> m [(peer, PeerFetchStatus header)]
fetchLogicIterationAct clientStateTracer FetchDecisionPolicy{blockFetchSize}
                       decisions =
    sequence
      [ (,) peer <$> addNewFetchRequest
                       (contramap (TraceLabelPeer peer) clientStateTracer)
                       blockFetchSize
                       request gsvs
                       stateVars
      | (Right request, gsvs, stateVars, peer) <- decisions ]


-- | STM actions to read various state variables that the fetch logic depends
-- upon. Any change in these variables is a trigger to re-evaluate the decision
-- on what blocks to fetch.
--
-- Note that this is a \"level trigger\" not an \"edge trigger\": we do not
-- have to re-evaluate on every change, it is sufficient to re-evaluate at some
-- stage after one or more changes. This means it is ok to get somewhat behind,
-- and it is not necessary to determine exactly what changed, just that there
-- was some change.
--
data FetchTriggerVariables peer header m = FetchTriggerVariables {
       readStateCurrentChain    :: STM m (AnchoredFragment header),
       readStateCandidateChains :: STM m (Map peer (AnchoredFragment header)),
       readStatePeerStatus      :: STM m (Map peer (PeerFetchStatus header))
     }

-- | STM actions to read various state variables that the fetch logic uses.
-- While the decisions do make use of the values of these variables, it is not
-- necessary to re-evaluate when these variables change.
--
data FetchNonTriggerVariables peer header block m = FetchNonTriggerVariables {
       readStateFetchedBlocks    :: STM m (Point block -> Bool),
       readStatePeerStateVars    :: STM m (Map peer (FetchClientStateVars m header)),
       readStatePeerGSVs         :: STM m (Map peer PeerGSV),
       readStateFetchMode        :: STM m FetchMode,
       readStateFetchedMaxSlotNo :: STM m MaxSlotNo,
       readStateChainSelStarvation :: STM m ChainSelStarvation
     }


data FetchStateFingerprint peer header block =
     FetchStateFingerprint
       !(Maybe (Point block))
       !(Map peer (Point header))
       !(Map peer (PeerFetchStatus header))
  deriving Eq

initialFetchStateFingerprint :: FetchStateFingerprint peer header block
initialFetchStateFingerprint =
    FetchStateFingerprint
      Nothing
      Map.empty
      Map.empty

updateFetchStateFingerprintPeerStatus :: Ord peer
                                      => [(peer, PeerFetchStatus header)]
                                      -> FetchStateFingerprint peer header block
                                      -> FetchStateFingerprint peer header block
updateFetchStateFingerprintPeerStatus statuses'
    (FetchStateFingerprint current candidates statuses) =
    FetchStateFingerprint
      current
      candidates
      (Map.union (Map.fromList statuses') statuses) -- left overrides right

-- |
--
-- Note that the domain of 'fetchStatePeerChains' is a subset of the domain
-- of 'fetchStatePeerStates' and 'fetchStatePeerReqVars'.
--
data FetchStateSnapshot peer header block m = FetchStateSnapshot {
       fetchStateCurrentChain     :: AnchoredFragment header,
       fetchStatePeerChains       :: Map peer (AnchoredFragment header),
       fetchStatePeerStates       :: Map peer (PeerFetchStatus   header,
                                               PeerFetchInFlight header,
                                               FetchClientStateVars m header),
       fetchStatePeerGSVs         :: Map peer PeerGSV,
       fetchStateFetchedBlocks    :: Point block -> Bool,
       fetchStateFetchMode        :: FetchMode,
       fetchStateFetchedMaxSlotNo :: MaxSlotNo,
       fetchStateChainSelStarvation :: ChainSelStarvation
     }

readStateVariables :: (MonadSTM m, Eq peer,
                       HasHeader header, HasHeader block,
                       HeaderHash header ~ HeaderHash block)
                   => FetchTriggerVariables peer header m
                   -> FetchNonTriggerVariables peer header block m
                   -> FetchStateFingerprint peer header block
                   -> STM m (FetchStateSnapshot peer header block m,
                             FetchStateFingerprint peer header block)
readStateVariables FetchTriggerVariables{..}
                   FetchNonTriggerVariables{..}
                   fetchStateFingerprint = do

    -- Read all the trigger state variables
    fetchStateCurrentChain  <- readStateCurrentChain
    fetchStatePeerChains    <- readStateCandidateChains
    fetchStatePeerStatus    <- readStatePeerStatus

    -- Construct the change detection fingerprint
    let !fetchStateFingerprint' =
          FetchStateFingerprint
            (Just (castPoint (AF.headPoint fetchStateCurrentChain)))
            (Map.map AF.headPoint fetchStatePeerChains)
            fetchStatePeerStatus

    -- Check the fingerprint changed, or block and wait until it does
    check (fetchStateFingerprint' /= fetchStateFingerprint)

    -- Now read all the non-trigger state variables
    fetchStatePeerStates       <- readStatePeerStateVars
                              >>= traverse readFetchClientState
    fetchStatePeerGSVs         <- readStatePeerGSVs
    fetchStateFetchedBlocks    <- readStateFetchedBlocks
    fetchStateFetchMode        <- readStateFetchMode
    fetchStateFetchedMaxSlotNo <- readStateFetchedMaxSlotNo
    fetchStateChainSelStarvation <- readStateChainSelStarvation

    -- Construct the overall snapshot of the state
    let fetchStateSnapshot =
          FetchStateSnapshot {
            fetchStateCurrentChain,
            fetchStatePeerChains,
            fetchStatePeerStates,
            fetchStatePeerGSVs,
            fetchStateFetchedBlocks,
            fetchStateFetchMode,
            fetchStateFetchedMaxSlotNo,
            fetchStateChainSelStarvation
          }

    return (fetchStateSnapshot, fetchStateFingerprint')
