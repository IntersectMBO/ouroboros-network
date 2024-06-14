{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TupleSections #-}

-- | This module contains the part of the block fetch decisions process that is
-- specific to the bulk sync mode.
module Ouroboros.Network.BlockFetch.Decision.BulkSync (
  fetchDecisionsBulkSync
) where

import Data.Bifunctor (first, Bifunctor (..))
import Data.List (foldl', sortOn)
import Data.List.NonEmpty (nonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Ord (Down(Down))
import qualified Data.Set as Set

import Ouroboros.Network.AnchoredFragment (AnchoredFragment, headBlockNo)
import qualified Ouroboros.Network.AnchoredFragment as AF
import Ouroboros.Network.Block
import Ouroboros.Network.BlockFetch.ClientState (FetchRequest (..),
           PeerFetchInFlight (..), PeerFetchStatus (..))
import Ouroboros.Network.BlockFetch.ConsensusInterface (FetchMode(FetchModeBulkSync))
import Ouroboros.Network.BlockFetch.DeltaQ (calculatePeerFetchInFlightLimits)

import Ouroboros.Network.BlockFetch.Decision.Common
-- REVIEW: We should not import anything from 'Decision.Deadline'; if the need
-- arises, we should move the interesting piece of code to 'Decision.Common'.
-- This is to be done on demand.

-- | Given a list of candidate fragments and their associated peers, choose what
-- to sync from who in the bulk sync mode.
fetchDecisionsBulkSync ::
  ( HasHeader header,
    HeaderHash header ~ HeaderHash block,
    Eq peer
  ) =>
  FetchDecisionPolicy header ->
  AnchoredFragment header ->
  (Point block -> Bool) ->
  MaxSlotNo ->
  -- | Order of the peers, from most to least preferred.
  [peer] ->
  -- | Association list of the candidate fragments and their associated peers.
  [(AnchoredFragment header, PeerInfo header peer extra)] ->
  -- | Association list of the requests and their associated peers.
  [(FetchDecision (FetchRequest header), PeerInfo header peer extra)]

fetchDecisionsBulkSync
  fetchDecisionPolicy
  currentChain
  fetchedBlocks
  fetchedMaxSlotNo
  peersOrder =
    -- Select the candidate to sync from, then try to fetch it from the peers
    -- that are still in the race to serve it, which gives us one peer to fetch
    -- from and all the others to decline.
    uncurry (++)
      . ( second $
            maybe
              []
              ( uncurry $
                  fetchTheCandidate
                    fetchDecisionPolicy
                    fetchedBlocks
                    fetchedMaxSlotNo
                    peersOrder
              )
        )
      . ( selectTheCandidate
            fetchDecisionPolicy
            currentChain
        )

-- FIXME: The 'FetchDeclineConcurrencyLimit' should only be used for
-- 'FetchModeDeadline', and 'FetchModeBulkSync' should have its own reasons.

-- | Given a list of candidate fragments and their associated peers, select the
-- candidate to sync from. Return this fragment, the list of peers that are
-- still in race to serve it, and the list of peers that are already being
-- declined.
selectTheCandidate ::
  ( HasHeader header
  ) =>
  FetchDecisionPolicy header ->
  -- | The current chain.
  AnchoredFragment header ->
  -- | The candidate fragments and their associated peers.
  [(AnchoredFragment header, peerInfo)] ->
  -- | The pair of: (a) a list of peers that we have decided are not right, eg.
  -- because they presented us with a chain forking too deep, and (b) the
  -- selected candidate that we choose to sync from and a list of peers that are
  -- still in the race to serve that candidate.
  ( [(FetchDecision any, peerInfo)],
    Maybe (ChainSuffix header, [(ChainSuffix header, peerInfo)])
  )

selectTheCandidate
  FetchDecisionPolicy {plausibleCandidateChain}
  currentChain =
    separateDeclinedAndStillInRace
      -- Select the suffix up to the intersection with the current chain. This can
      -- eliminate candidates that fork too deep.
      . selectForkSuffixes currentChain
      -- Filter to keep chains the consensus layer tells us are plausible.
      . filterPlausibleCandidates plausibleCandidateChain currentChain
      -- Sort the candidates by descending block number of their heads, that is
      -- consider longest fragments first.
      . sortOn (Down . headBlockNo . fst)
    where
      -- Very ad-hoc helper.
      separateDeclinedAndStillInRace :: [(Either a b, c)] -> ([(Either a any, c)], Maybe (b, [(b, c)]))
      separateDeclinedAndStillInRace xs =
        let (declined, inRace) = partitionEithersFirst xs
         in ( map (first Left) declined,
              ((,inRace) . fst . NE.head) <$> nonEmpty inRace
            )

-- | Given a candidate to sync from and a list of peers in race to serve that,
-- choose which peer to sync from and decline the others.
--
-- PRECONDITION: The set of peers must be included in the peer order queue.
--
-- POSTCONDITION: The returned list contains at most one @Right@ element.
fetchTheCandidate ::
  ( HasHeader header,
    HeaderHash header ~ HeaderHash block,
    Eq peer
  ) =>
  FetchDecisionPolicy header ->
  (Point block -> Bool) ->
  MaxSlotNo ->
  -- | Order of the peers, from most to least preferred.
  [peer] ->
  -- | The candidate fragment that we have selected to sync from, as suffix of
  -- the immutable tip.
  ChainSuffix header ->
  -- | Association list of candidate fragments (as suffixes of the immutable
  -- tip) and their associated peers.
  [(ChainSuffix header, PeerInfo header peer extra)] ->
  -- | Association list of the requests and the peers that they are associated
  -- with. The requests can only be declined.
  [(FetchDecision (FetchRequest header), PeerInfo header peer extra)]

fetchTheCandidate
  fetchDecisionPolicy
  fetchedBlocks
  fetchedMaxSlotNo
  peersOrder
  chainSuffix
  candidates =
    let -- Keep blocks that have not already been downloaded or that are not
        -- already in-flight with any peer. This returns a 'FetchDecision'.
        fragments =
          filterNotAlreadyFetched fetchedBlocks fetchedMaxSlotNo chainSuffix
            >>= filterNotAlreadyInFlightWithAnyPeer statusInflightInfo

        -- For each peer with its candidate, try to create a request for those
        -- fragments. We are not yet deciding which peer to fetch from.
        requests =
          map
            ( \(candidate, peerInfo@(status, inflight, gsvs, _, _)) ->
                (,peerInfo) $ do
                  -- Trim the fragments to each specific peer's candidate, keeping only
                  -- blocks that they may actually serve. If they cannot serve any of the
                  -- blocks, filter them out.
                  trimmedFragments <- trimFragmentsToCandidate candidate =<< fragments

                  -- For each peer, try to create a request for those fragments.
                  -- 'fetchRequestDecision' enforces respect of concurrency limits, and
                  -- 'FetchModeBulkSync' should have a limit of 1. This creates a fairly
                  -- degenerate situation with two extreme cases of interest:
                  --
                  -- 1. If there is no currently in-flight request, then all the peers are
                  --    eligible, and all of them will manage to create a request.
                  --
                  -- 2. If there are currently in-flight requests, then they are all with
                  --    the same peer, and only that peer will manage to create a request.
                  request <-
                    fetchRequestDecision
                      fetchDecisionPolicy
                      FetchModeBulkSync
                      nConcurrentFetchPeers
                      (calculatePeerFetchInFlightLimits gsvs)
                      inflight
                      status
                      (Right trimmedFragments) -- FIXME: This is a hack to avoid having to change the signature of 'fetchRequestDecisions'.
                  pure request
            )
            candidates

        -- Order the requests according to the peer order that we have been
        -- given, then separate between declined and accepted requests.
        (declinedRequests, requestsOrdered) =
          partitionEithersFirst
            [ (request, peerInfo)
              | (request, peerInfo@(_, _, _, peer, _)) <- requests,
                peer' <- peersOrder,
                peer == peer'
            ]
     in -- Return the first peer in that order, and decline all the ones that were
        -- not already declined.
        case requestsOrdered of
          [] -> []
          (theRequest, thePeer) : otherRequests ->
            (Right theRequest, thePeer)
              : map (first (const (Left (FetchDeclineConcurrencyLimit FetchModeBulkSync 1)))) otherRequests
              ++ map (first Left) declinedRequests
    where
      statusInflightInfo =
        map (\(_, (status, inflight, _, _, _)) -> (status, inflight)) candidates
      nConcurrentFetchPeers =
        -- REVIEW: A bit weird considering that this should be '0' or '1'.
        fromIntegral $ length $ filter (\(_, inflight) -> peerFetchReqsInFlight inflight > 0) statusInflightInfo

-- | Given a candidate and some fragments, keep only the parts of the fragments
-- that are within the candidate. Decline if nothing remains, and return a
-- non-empty list of non-empty fragments otherwise.
trimFragmentsToCandidate ::
  (HasHeader header) =>
  ChainSuffix header ->
  [AnchoredFragment header] ->
  FetchDecision [AnchoredFragment header]
trimFragmentsToCandidate candidate fragments =
  let trimmedFragments =
        -- FIXME: This can most definitely be improved considering that the
        -- property to be in `candidate` is monotonic.
        concatMap
          (AF.filter (flip AF.withinFragmentBounds (getChainSuffix candidate) . blockPoint))
          fragments
   in if null trimmedFragments
        then Left FetchDeclineAlreadyFetched
        else Right trimmedFragments

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

-- | Partition eithers on the first component of the pair.
partitionEithersFirst :: [(Either a b, c)] -> ([(a, c)], [(b, c)])
partitionEithersFirst =
  foldr
    ( \(e, c) (as, bs) -> case e of
        Left a -> ((a, c) : as, bs)
        Right b -> (as, (b, c) : bs)
    )
    ([], [])
