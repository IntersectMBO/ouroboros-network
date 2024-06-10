{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TupleSections #-}

-- | This module contains the part of the block fetch decisions process that is
-- specific to the bulk sync mode.
module Ouroboros.Network.BlockFetch.Decision.BulkSync (
  fetchDecisionsBulkSync
) where

import Cardano.Prelude (maybeToEither)
import Data.Bifunctor (first)
import Data.List (foldl', sortOn)
import Data.Ord (Down(Down))
import qualified Data.Set as Set

import Ouroboros.Network.AnchoredFragment (AnchoredFragment, headBlockNo)
import Ouroboros.Network.Block
import Ouroboros.Network.BlockFetch.ClientState (FetchRequest (..),
           PeerFetchInFlight (..), PeerFetchStatus (..))
import Ouroboros.Network.BlockFetch.ConsensusInterface (FetchMode(FetchModeBulkSync))
import Ouroboros.Network.BlockFetch.DeltaQ (calculatePeerFetchInFlightLimits)

import Ouroboros.Network.BlockFetch.Decision.Common
-- REVIEW: We should not import anything from 'Decision.Deadline'; if the need
-- arises, we should move the interesting piece of code to 'Decision.Common'.
-- This is to be done on demand.

fetchDecisionsBulkSync ::
  ( HasHeader header,
    HeaderHash header ~ HeaderHash block,
    Eq peer
  ) =>
  FetchDecisionPolicy header ->
  AnchoredFragment header ->
  (Point block -> Bool) ->
  MaxSlotNo ->
  [(AnchoredFragment header, PeerInfo header peer extra)] ->
  [(FetchDecision (FetchRequest header), PeerInfo header peer extra)]
fetchDecisionsBulkSync
  fetchDecisionPolicy@FetchDecisionPolicy {plausibleCandidateChain}
  currentChain
  fetchedBlocks
  fetchedMaxSlotNo
  candidatesAndPeers =
    -- Sort candidates from longest to shortest and filter out all unplausible
    -- candidates. This gives a list of already-declined candidates and a list
    -- of plausible candidates.
    let (declinedCandidates, candidates) =
          partitionEithersFirst
            -- Select the suffix up to the intersection with the current chain.
            . selectForkSuffixes
              currentChain
            -- Filter to keep chains the consensus layer tells us are plausible.
            . filterPlausibleCandidates
              plausibleCandidateChain
              currentChain
            -- Sort the candidates by descending block number of their heads, that is
            -- consider longest fragments first.
            . sortOn (Down . headBlockNo . fst)
            $ candidatesAndPeers
     in -- If there are no candidates remaining, we are done. Otherwise, pick the
        -- first one and try to fetch it. Decline all the others.
        map (first Left) declinedCandidates
          ++ case candidates of
            [] -> []
            ((candidate, peer) : otherCandidates) ->
              case fetchTheCandidate
                fetchDecisionPolicy
                fetchedBlocks
                fetchedMaxSlotNo
                candidatesAndPeers
                candidate of
                Left declined ->
                  -- If fetching the candidate did not work, report the reason and
                  -- decline all the others for concurrency reasons.
                  (Left declined, peer) : declineConcurrent otherCandidates
                Right (theRequest, thePeer) ->
                  -- If fetching the candidate _did_ work, then we have a request
                  -- potentially for another peer, so we report this request and
                  -- decline all the peers except for that specific one.
                  (Right theRequest, thePeer)
                    : filter ((not . eqPeerInfo thePeer) . snd) (declineConcurrent candidates)
    where
      partitionEithersFirst :: [(Either a b, c)] -> ([(a, c)], [(b, c)])
      partitionEithersFirst =
        foldr
          ( \(e, c) (as, bs) -> case e of
              Left a -> ((a, c) : as, bs)
              Right b -> (as, (b, c) : bs)
          )
          ([], [])
      declineConcurrent = map (first (const (Left (FetchDeclineConcurrencyLimit FetchModeBulkSync 1))))
      eqPeerInfo (_, _, _, p1, _) (_, _, _, p2, _) = p1 == p2

fetchTheCandidate ::
  ( HasHeader header,
    HeaderHash header ~ HeaderHash block
  ) =>
  FetchDecisionPolicy header ->
  (Point block -> Bool) ->
  MaxSlotNo ->
  [(AnchoredFragment header, PeerInfo header peer extra)] ->
  ChainSuffix header ->
  FetchDecision ((FetchRequest header), PeerInfo header peer extra)
fetchTheCandidate
  fetchDecisionPolicy
  fetchedBlocks
  fetchedMaxSlotNo
  candidatesAndPeers
  chainSuffix =
    do
      -- Filter to keep blocks that have not already been downloaded or that are
      -- not already in-flight with any peer.
      fragments <-
        filterNotAlreadyFetched fetchedBlocks fetchedMaxSlotNo chainSuffix
          >>= filterNotAlreadyInFlightWithAnyPeer statusInflightInfo
      -- For each peer, try to create a request for those fragments. Then take
      -- the first one that is successful.
      maybeToEither FetchDeclineAlreadyFetched $
        firstRight $
            map
              ( \(_, peerInfo@(status, inflight, gsvs, _, _)) ->
                  -- let fragments' = filterWithMaxSlotNo (not . blockAlreadyFetched) fragments
                  --  in (FetchRequest fragments', peer)
                  (,peerInfo)
                    <$> fetchRequestDecision
                      fetchDecisionPolicy
                      FetchModeBulkSync
                      nConcurrentFetchPeers
                      (calculatePeerFetchInFlightLimits gsvs)
                      inflight
                      status
                      (Right fragments) -- FIXME: This is a hack to avoid having to change the signature of 'fetchRequestDecisions'.
              )
              candidatesAndPeers
    where
      statusInflightInfo = map (\(_, (status, inflight, _, _, _)) -> (status, inflight)) candidatesAndPeers
      nConcurrentFetchPeers =
        -- REVIEW: A bit weird considering that this should be '0' or '1'.
        fromIntegral $ length $ filter (\(_, inflight) -> peerFetchReqsInFlight inflight > 0) statusInflightInfo
      firstRight [] = Nothing
      firstRight (Right x : _) = Just x
      firstRight (Left _ : xs) = firstRight xs

-- | A penultimate step of filtering, but this time across peers, rather than
-- individually for each peer. If we're following the parallel fetch
-- mode then we filter out blocks that are already in-flight with other
-- peers.
--
-- Note that this does /not/ cover blocks that are proposed to be fetched in
-- this round of decisions. That step is covered  in 'fetchRequestDecisions'.
filterNotAlreadyInFlightWithAnyPeer ::
  (HasHeader header) =>
  [(PeerFetchStatus header, PeerFetchInFlight header)] ->
  CandidateFragments header ->
  FetchDecision [AnchoredFragment header]
filterNotAlreadyInFlightWithAnyPeer statusInflightInfos chainfragments =
  if null fragments
    then Left FetchDeclineInFlightOtherPeer
    else Right fragments
  where
    fragments = concatMap (filterWithMaxSlotNo notAlreadyInFlight maxSlotNoInFlight) $ snd chainfragments
    notAlreadyInFlight b = blockPoint b `Set.notMember` blocksInFlightWithOtherPeers
    -- All the blocks that are already in-flight with all peers
    blocksInFlightWithOtherPeers =
      Set.unions
        [ case status of
            PeerFetchStatusShutdown -> Set.empty
            PeerFetchStatusStarting -> Set.empty
            PeerFetchStatusAberrant -> Set.empty
            _other -> peerFetchBlocksInFlight inflight
          | (status, inflight) <- statusInflightInfos
        ]
    -- The highest slot number that is or has been in flight for any peer.
    maxSlotNoInFlight = foldl' max NoMaxSlotNo (map (peerFetchMaxSlotNo . snd) statusInflightInfos)
