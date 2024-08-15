{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Ouroboros.Network.BlockFetch.Decision
  ( -- * Deciding what to fetch
    fetchDecisions
  , FetchDecisionPolicy (..)
  , FetchMode (..)
  , PeerInfo
  , FetchDecision
  , FetchDecline (..)
    -- ** Components of the decision-making process
  , filterPlausibleCandidates
  , selectForkSuffixes
  , dropAlreadyFetched
  , dropAlreadyInFlightWithPeer
  , prioritisePeerChains
  , fetchRequestDecisions
  ) where

import Data.Hashable
import Control.Monad.Class.MonadTime.SI (MonadMonotonicTime(..))
import Control.Tracer (Tracer)

import Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import Ouroboros.Network.Block
import Ouroboros.Network.BlockFetch.ClientState (FetchRequest (..), PeersOrder (..))
import Ouroboros.Network.BlockFetch.ConsensusInterface (FetchMode (..), ChainSelStarvation)

import Ouroboros.Network.BlockFetch.Decision.Deadline (FetchDecisionPolicy (..), PeerInfo, FetchDecision, FetchDecline (..),
                                                     filterPlausibleCandidates, dropAlreadyFetched, dropAlreadyInFlightWithPeer,
                                                     selectForkSuffixes, fetchDecisionsDeadline, prioritisePeerChains, fetchRequestDecisions)
import Ouroboros.Network.BlockFetch.Decision.Genesis (fetchDecisionsGenesisM)
import Ouroboros.Network.BlockFetch.Decision.Trace (TraceDecisionEvent)

fetchDecisions
  :: forall peer header block m extra.
     (Ord peer,
      Hashable peer,
      HasHeader header,
      HeaderHash header ~ HeaderHash block, MonadMonotonicTime m)
  => Tracer m (TraceDecisionEvent peer header)
  -> FetchDecisionPolicy header
  -> FetchMode
  -> AnchoredFragment header
  -> (Point block -> Bool)
  -> MaxSlotNo
  -> ChainSelStarvation
  -> ( PeersOrder peer
     , PeersOrder peer -> m ()
     , peer -> m ()
     )
  -> [(AnchoredFragment header, PeerInfo header peer extra)]
  -> m [(FetchDecision (FetchRequest header), PeerInfo header peer extra)]

fetchDecisions
  _tracer
  fetchDecisionPolicy
  FetchModeDeadline
  currentChain
  fetchedBlocks
  fetchedMaxSlotNo
  _chainSelStarvation
  _peersOrderHandlers
  candidatesAndPeers
  =
    pure
      $ fetchDecisionsDeadline
        fetchDecisionPolicy
        currentChain
        fetchedBlocks
        fetchedMaxSlotNo
        candidatesAndPeers

fetchDecisions
  tracer
  fetchDecisionPolicy
  FetchModeGenesis
  currentChain
  fetchedBlocks
  fetchedMaxSlotNo
  chainSelStarvation
  peersOrderHandlers
  candidatesAndPeers
  =
      fetchDecisionsGenesisM
        tracer
        fetchDecisionPolicy
        currentChain
        fetchedBlocks
        fetchedMaxSlotNo
        chainSelStarvation
        peersOrderHandlers
        candidatesAndPeers
