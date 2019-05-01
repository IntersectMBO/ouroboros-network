{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}

module Ouroboros.Network.BlockFetch.State (
    fetchLogicIterations,
    FetchDecisionPolicy(..),
    FetchTriggerVariables(..),
    FetchNonTriggerVariables(..),
    FetchDecision,
    FetchDecline(..),
    FetchMode(..)
  ) where

import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import qualified Data.Set as Set
import           Data.Void

import           Control.Monad.Class.MonadSTM
import           Control.Exception (assert)
import           Control.Tracer (Tracer, traceWith)

import           Ouroboros.Network.Block
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment(..))
import qualified Ouroboros.Network.AnchoredFragment as AnchoredFragment
import qualified Ouroboros.Network.ChainFragment    as ChainFragment

import           Ouroboros.Network.BlockFetch.ClientState
                   ( FetchRequest(..)
                   , PeerFetchInFlight(..)
                   , PeerFetchStatus(..)
                   , FetchClientStateVars
                   , addNewFetchRequest
                   , readFetchClientState
                   )
import           Ouroboros.Network.BlockFetch.Decision
                   ( fetchDecisions
                   , PeerInfo
                   , FetchDecisionPolicy(..)
                   , FetchMode(..)
                   , FetchDecision
                   , FetchDecline(..)
                   )
import           Ouroboros.Network.BlockFetch.DeltaQ
                   ( PeerGSV(..) )


fetchLogicIterations
  :: (MonadSTM m, Ord peer,
      HasHeader header, HasHeader block,
      HeaderHash header ~ HeaderHash block)
  => Tracer m [FetchDecision [Point header]]
  -> FetchDecisionPolicy header
  -> FetchTriggerVariables peer header m
  -> FetchNonTriggerVariables peer header block m
  -> m Void
fetchLogicIterations decisionTracer
                     fetchDecisionPolicy
                     fetchTriggerVariables
                     fetchNonTriggerVariables =

    iterateForever initialFetchStateFingerprint $ \stateFingerprint ->

      -- Run a single iteration of the fetch logic:
      --
      -- + wait for the state to change and make decisions for the new state
      -- + act on those decisions

      fetchLogicIteration
        decisionTracer
        fetchDecisionPolicy
        fetchTriggerVariables
        fetchNonTriggerVariables
        stateFingerprint


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
  :: (MonadSTM m, Ord peer,
      HasHeader header, HasHeader block,
      HeaderHash header ~ HeaderHash block)
  => Tracer m [FetchDecision [Point header]]
  -> FetchDecisionPolicy header
  -> FetchTriggerVariables peer header m
  -> FetchNonTriggerVariables peer header block m
  -> FetchStateFingerprint peer header block
  -> m (FetchStateFingerprint peer header block)
fetchLogicIteration decisionTracer
                    fetchDecisionPolicy
                    fetchTriggerVariables
                    fetchNonTriggerVariables
                    stateFingerprint = do

    -- Gather a snapshot of all the state we need.
    (stateSnapshot, stateFingerprint') <-
      atomically $
        readStateVariables
          fetchTriggerVariables
          fetchNonTriggerVariables
          stateFingerprint

    -- TODO: allow for boring PeerFetchStatusBusy transitions where we go round
    -- again rather than re-evaluating everything.
    assert (stateFingerprint' /= stateFingerprint) $ return ()

    -- TODO: log the difference in the fingerprint that caused us to wake up

    -- Make all the fetch decisions
    let decisions = fetchDecisionsForStateSnapshot
                      fetchDecisionPolicy
                      stateSnapshot

    -- If we want to trace timings, we can do it here after forcing:
    -- _ <- evaluate (force decisions)

    -- Trace the batch of fetch decisions
    traceWith decisionTracer (map (fmap fetchRequestPoints . fst) decisions)

    -- Tell the fetch clients to act on our decisions
    statusUpdates <- fetchLogicIterationAct fetchDecisionPolicy
                                            (map swizzleReqVar decisions)
    let stateFingerprint'' =
          updateFetchStateFingerprintPeerStatus statusUpdates stateFingerprint'

    return stateFingerprint''
  where
    swizzleReqVar (d,(_,_,g,(rq,p))) = (d,g,rq,p)

    fetchRequestPoints :: HasHeader hdr => FetchRequest hdr -> [Point hdr]
    fetchRequestPoints (FetchRequest headerss) =
      -- Flatten multiple fragments and trace points, not full headers
      [ blockPoint header
      | headers <- headerss
      , header  <- ChainFragment.toOldestFirst headers ]

-- | Do a bit of rearranging of data before calling 'fetchDecisions' to do the
-- real work.
--
fetchDecisionsForStateSnapshot
  :: (HasHeader header, HasHeader block,
      HeaderHash header ~ HeaderHash block,
      Ord peer)
  => FetchDecisionPolicy header
  -> FetchStateSnapshot peer header block m
  -> [( FetchDecision (FetchRequest header),
        PeerInfo header (FetchClientStateVars m header, peer)
      )]

fetchDecisionsForStateSnapshot
    fetchDecisionPolicy
    FetchStateSnapshot {
      fetchStateCurrentChain,
      fetchStatePeerChains,
      fetchStatePeerStates,
      fetchStatePeerGSVs,
      fetchStateFetchedBlocks,
      fetchStateFetchMode
    } =
    assert (                 Map.keysSet fetchStatePeerChains
            `Set.isSubsetOf` Map.keysSet fetchStatePeerStates) $

    assert (Map.keysSet fetchStatePeerStates
         == Map.keysSet fetchStatePeerGSVs) $

    fetchDecisions
      fetchDecisionPolicy
      fetchStateFetchMode
      fetchStateCurrentChain
      fetchStateFetchedBlocks
      peerChainsAndPeerInfo
  where
    peerChainsAndPeerInfo =
      map swizzle . Map.toList $
      Map.intersectionWith (,)
        (Map.intersectionWith (,) fetchStatePeerChains fetchStatePeerStates)
        fetchStatePeerGSVs

    swizzle (peer, ((chain, (status, inflight, vars)), gsvs)) =
      (chain, (status, inflight, gsvs, (vars, peer)))


-- | Act on decisions to send new requests. In fact all we do here is update
-- request variables that are shared with the threads running the block fetch
-- protocol with each peer.
--
fetchLogicIterationAct :: (MonadSTM m, HasHeader header)
                       => FetchDecisionPolicy header
                       -> [(FetchDecision (FetchRequest header),
                            PeerGSV,
                            FetchClientStateVars m header,
                            peer)]
                       -> m [(peer, PeerFetchStatus header)]
fetchLogicIterationAct FetchDecisionPolicy{blockFetchSize} decisions =
    sequence
      [ (,) peer <$> addNewFetchRequest blockFetchSize stateVars request gsvs
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
       readStateFetchedBlocks :: STM m (Point block -> Bool),
       readStatePeerStateVars :: STM m (Map peer (FetchClientStateVars m header)),
       readStatePeerGSVs      :: STM m (Map peer PeerGSV),
       readStateFetchMode     :: STM m FetchMode
     }


data FetchStateFingerprint peer header block =
     FetchStateFingerprint
       (Maybe (Point block))
       (Map peer (Point header))
       (Map peer (PeerFetchStatus header))
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
       fetchStateCurrentChain  :: AnchoredFragment header,
       fetchStatePeerChains    :: Map peer (AnchoredFragment header),
       fetchStatePeerStates    :: Map peer (PeerFetchStatus   header,
                                            PeerFetchInFlight header,
                                            FetchClientStateVars m header),
       fetchStatePeerGSVs      :: Map peer PeerGSV,
       fetchStateFetchedBlocks :: Point block -> Bool,
       fetchStateFetchMode     :: FetchMode
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
    let fetchStateFingerprint' =
          FetchStateFingerprint
            (Just (castPoint (AnchoredFragment.headPoint fetchStateCurrentChain)))
            (Map.map AnchoredFragment.headPoint fetchStatePeerChains)
            fetchStatePeerStatus

    -- Check the fingerprint changed, or block and wait until it does
    check (fetchStateFingerprint' /= fetchStateFingerprint)

    -- Now read all the non-trigger state variables
    fetchStatePeerStates    <- readStatePeerStateVars
                           >>= traverse readFetchClientState
    fetchStatePeerGSVs      <- readStatePeerGSVs
    fetchStateFetchedBlocks <- readStateFetchedBlocks
    fetchStateFetchMode     <- readStateFetchMode

    -- Construct the overall snapshot of the state
    let fetchStateSnapshot =
          FetchStateSnapshot {
            fetchStateCurrentChain,
            fetchStatePeerChains,
            fetchStatePeerStates,
            fetchStatePeerGSVs,
            fetchStateFetchedBlocks,
            fetchStateFetchMode
          }

    return (fetchStateSnapshot, fetchStateFingerprint')

