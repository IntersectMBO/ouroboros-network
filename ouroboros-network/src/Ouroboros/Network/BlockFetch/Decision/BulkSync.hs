{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}

-- | This module contains the part of the block fetch decisions process that is
-- specific to the bulk sync mode.
module Ouroboros.Network.BlockFetch.Decision.BulkSync (
  fetchDecisionsBulkSync
, filterNotAlreadyInFlightWithOtherPeers) where

import Control.Monad (guard)
import Data.Bifunctor (first)
import Data.List (foldl', sortOn)
import Data.Ord (Down(Down))
import qualified Data.Set as Set

import Ouroboros.Network.AnchoredFragment (AnchoredFragment, headBlockNo)
import Ouroboros.Network.Block
import Ouroboros.Network.BlockFetch.ClientState (FetchRequest (..),
           PeerFetchInFlight (..), PeerFetchStatus (..))
import Ouroboros.Network.BlockFetch.ConsensusInterface (FetchMode(FetchModeBulkSync))

import Ouroboros.Network.BlockFetch.Decision.Common
-- REVIEW: We should not import anything from 'Decision.Deadline'; if the need
-- arises, we should move the interesting piece of code to 'Decision.Common'.
-- This is to be done on demand.

fetchDecisionsBulkSync
  :: (HasHeader header,
      HeaderHash header ~ HeaderHash block)
  =>FetchDecisionPolicy header
  -> AnchoredFragment header
  -> (Point block -> Bool)
  -> MaxSlotNo
  -> [(AnchoredFragment header, PeerInfo header peer extra)]
  -> [(FetchDecision (FetchRequest header), PeerInfo header peer extra)]

fetchDecisionsBulkSync
  _fetchDecisionPolicy@FetchDecisionPolicy {plausibleCandidateChain}
  currentChain
  fetchedBlocks
  fetchedMaxSlotNo
  candidatesAndPeers =
    -- Sort candidates from longest to shortest and filter out all unplausible
    -- candidates. This gives a list of already-declined candidates and a list
    -- of plausible candidates.
    let (declinedCandidates, candidates) =
          partitionEithersFirst
            -- Filter to keep chains the consensus layer tells us are plausible.
            . filterPlausibleCandidates
              plausibleCandidateChain
              currentChain
            -- Sort the candidates by descending block number of their heads, that is
            -- consider longest fragments first.
            . sortOn (Down . headBlockNo . fst)
            $ candidatesAndPeers
        declinedCandidates' = map (first Left) declinedCandidates
     in
      -- If there are no candidates remaining, we are done. Otherwise, pick the
      -- first one and try to fetch it. Decline all the others.
      case candidates of
          [] -> declinedCandidates'
          ((theCandidate, _thePeer) : otherCandidates) ->
            let declinedOtherCandidates = map (first (const (Left (FetchDeclineConcurrencyLimit FetchModeBulkSync 1)))) otherCandidates
             in fetchTheCandidate
                  candidatesAndPeers
                  theCandidate
                  : (declinedCandidates' ++ declinedOtherCandidates)
    where
      partitionEithersFirst :: [(Either a b, c)] -> ([(a, c)], [(b, c)])
      partitionEithersFirst =
        foldr
          ( \(e, c) (as, bs) -> case e of
              Left a -> ((a, c) : as, bs)
              Right b -> (as, (b, c) : bs)
          )
          ([], [])

fetchTheCandidate ::
  -- (HasHeader header, HeaderHash header ~ HeaderHash block) =>
  [(AnchoredFragment header, PeerInfo header peer extra)] ->
  AnchoredFragment header ->
  (FetchDecision (FetchRequest header), PeerInfo header peer extra)
fetchTheCandidate _candidatesAndPeers _theCandidate =
  undefined

-- -- FIXME: Wrap in a 'FetchRequest'.
  -- map (first (fmap FetchRequest))

  --   -- Filter to keep blocks that are not already in-flight with other peers.
  -- . filterNotAlreadyInFlightWithOtherPeers
  -- . map (swizzleSI . first (fmap snd))

  --   -- Filter to keep blocks that are not already in-flight for this peer.
  -- . filterNotAlreadyInFlightWithPeer'
  -- . map swizzleI

  --   -- Filter to keep blocks that have not already been downloaded.
  -- . filterNotAlreadyFetched'
  --     fetchedBlocks
  --     fetchedMaxSlotNo

  --   -- Select the suffix up to the intersection with the current chain.
  -- . selectForkSuffixes
  --     currentChain

  -- where
  --   -- Data swizzling functions to get the right info into each stage.
  --   swizzleI   (c, p@(_,     inflight,_,_,      _)) = (c,         inflight,       p)
  --   swizzleSI  (c, p@(status,inflight,_,_,      _)) = (c, status, inflight,       p)

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
