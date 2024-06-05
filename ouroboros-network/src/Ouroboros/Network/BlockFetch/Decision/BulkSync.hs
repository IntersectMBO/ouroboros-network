{-# LANGUAGE NamedFieldPuns #-}

-- | This module contains the part of the block fetch decisions process that is
-- specific to the bulk sync mode.
module Ouroboros.Network.BlockFetch.Decision.BulkSync (
  fetchDecisionsBulkSync
, filterNotAlreadyInFlightWithOtherPeers) where

import Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import Ouroboros.Network.Block
import Ouroboros.Network.BlockFetch.ClientState (FetchRequest (..),
           PeerFetchInFlight (..), PeerFetchStatus (..))

import Ouroboros.Network.BlockFetch.Decision.Common
import Control.Monad (guard)
import qualified Data.Set as Set
import Data.List (foldl', singleton)
-- REVIEW: We should not import anything from 'Decision.Deadline'; if the need
-- arises, we should move the interesting piece of code to 'Decision.Common'.
-- This is to be done on demand.

fetchDecisionsBulkSync ::
     FetchDecisionPolicy header
  -> AnchoredFragment header
  -> (Point block -> Bool)
  -> MaxSlotNo
  -> [(AnchoredFragment header, PeerInfo header peer extra)]
  -> [(FetchDecision (FetchRequest header), PeerInfo header peer extra)]

fetchDecisionsBulkSync
  _fetchDecisionPolicy@FetchDecisionPolicy {
    plausibleCandidateChain
  }
  currentChain
  _fetchedBlocks
  _fetchedMaxSlotNo
  =
    map (\(fd, peer) ->
           (FetchRequest . singleton <$> fd, peer)
        )

  -- First, filter to keep chains the consensus layer tells us are plausible.
  . filterPlausibleCandidates
      plausibleCandidateChain
      currentChain

-- | A penultimate step of filtering, but this time across peers, rather than
-- individually for each peer. If we're following the parallel fetch
-- mode then we filter out blocks that are already in-flight with other
-- peers.
--
-- Note that this does /not/ cover blocks that are proposed to be fetched in
-- this round of decisions. That step is covered  in 'fetchRequestDecisions'.
--
filterNotAlreadyInFlightWithOtherPeers
  :: HasHeader header
  => [( FetchDecision [AnchoredFragment header]
      , PeerFetchStatus header
      , PeerFetchInFlight header
      , peerinfo )]
  -> [(FetchDecision [AnchoredFragment header], peerinfo)]

filterNotAlreadyInFlightWithOtherPeers chains =
    [ (mcandidatefragments',      peer)
    | (mcandidatefragments, _, _, peer) <- chains
    , let mcandidatefragments' = do
            chainfragments <- mcandidatefragments
            let fragments = concatMap (filterWithMaxSlotNo
                                        notAlreadyInFlight
                                        maxSlotNoInFlightWithOtherPeers)
                                      chainfragments
            guard (not (null fragments)) ?! FetchDeclineInFlightOtherPeer
            return fragments
    ]
  where
    notAlreadyInFlight b =
      blockPoint b `Set.notMember` blocksInFlightWithOtherPeers

   -- All the blocks that are already in-flight with all peers
    blocksInFlightWithOtherPeers =
      Set.unions
        [ case status of
            PeerFetchStatusShutdown -> Set.empty
            PeerFetchStatusStarting -> Set.empty
            PeerFetchStatusAberrant -> Set.empty
            _other                  -> peerFetchBlocksInFlight inflight
        | (_, status, inflight, _) <- chains ]

    -- The highest slot number that is or has been in flight for any peer.
    maxSlotNoInFlightWithOtherPeers = foldl' max NoMaxSlotNo
      [ peerFetchMaxSlotNo inflight | (_, _, inflight, _) <- chains ]
