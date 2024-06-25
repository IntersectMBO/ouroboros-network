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
  , filterNotAlreadyFetched
  , filterNotAlreadyInFlightWithPeer
  , prioritisePeerChains
  , fetchRequestDecisions
  ) where

import Data.Bifunctor (Bifunctor(..))
import Data.Hashable
import Data.List (singleton)
import Data.Maybe (listToMaybe)

import Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import Ouroboros.Network.Block
import Ouroboros.Network.BlockFetch.ClientState (FetchRequest (..), PeersOrder (..))
import Ouroboros.Network.BlockFetch.ConsensusInterface (FetchMode (..))

import Ouroboros.Network.BlockFetch.Decision.Common (FetchDecisionPolicy (..), PeerInfo, FetchDecision, FetchDecline (..),
                                                     filterPlausibleCandidates, filterNotAlreadyFetched, filterNotAlreadyInFlightWithPeer,
                                                     selectForkSuffixes)
import Ouroboros.Network.BlockFetch.Decision.Deadline (fetchDecisionsDeadline,
                                                      prioritisePeerChains, fetchRequestDecisions)
import Ouroboros.Network.BlockFetch.Decision.BulkSync (fetchDecisionsBulkSync)

fetchDecisions
  :: (Ord peer,
      Hashable peer,
      HasHeader header,
      HeaderHash header ~ HeaderHash block,
      Monad m)
  => FetchDecisionPolicy header
  -> FetchMode
  -> AnchoredFragment header
  -> (Point block -> Bool)
  -> MaxSlotNo
  -> ( PeersOrder peer
     , PeersOrder peer -> m ()
     )
  -> [(AnchoredFragment header, PeerInfo header peer extra)]
  -> m [(FetchDecision (FetchRequest header), PeerInfo header peer extra)]

fetchDecisions
  fetchDecisionPolicy
  FetchModeDeadline
  currentChain
  fetchedBlocks
  fetchedMaxSlotNo
  _peersOrder
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
  fetchDecisionPolicy
  FetchModeBulkSync
  currentChain
  fetchedBlocks
  fetchedMaxSlotNo
  ( PeersOrder {peersOrderAll},
    writePeersOrder
    )
  candidatesAndPeers = do
    -- Align the peers order with the actual peers; this consists in removing
    -- all peers from the peers order that are not in the actual peers list and
    -- adding at the end of the peers order all the actual peers that were not
    -- there before.
    let actualPeers = map (\(_, (_, _, _, peer, _)) -> peer) candidatesAndPeers
        peersOrderAll' =
          filter (`elem` actualPeers) peersOrderAll
            ++ filter (`notElem` peersOrderAll) actualPeers
        peersOrder' = PeersOrder peersOrderAll'

    -- FIXME: If ChainSel was starved, push the current peer to the end of the
    -- peers order priority queue.

    -- Compute the actual block fetch decision. This contains only declines and
    -- at most one request. 'theDecision' is therefore a 'Maybe'.
    let (theDecision, declines) =
          fetchDecisionsBulkSync
            fetchDecisionPolicy
            currentChain
            fetchedBlocks
            fetchedMaxSlotNo
            peersOrder'
            candidatesAndPeers

    -- If the peer that is supposed to fetch the block is not the first in the
    -- peers order, then we have changed focused peer. We move the new peer at
    -- the beginning of the queue, but we do not push the other one away,
    -- because it has not done anything wrong.
    case theDecision of
      Just (_, (_, _, _, thePeer, _))
        | Just thePeer /= listToMaybe peersOrderAll -> do
            let peersOrder'' = PeersOrder $ thePeer : filter (/= thePeer) peersOrderAll'
            -- FIXME: Record the current time as the first time we chose that
            -- new peer.
            writePeersOrder peersOrder''
      _ -> pure ()

    pure $
      maybe [] (singleton . first Right) theDecision
        ++ map (first Left) declines
