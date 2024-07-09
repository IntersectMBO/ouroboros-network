{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module contains the part of the block fetch decisions process that is
-- specific to the bulk sync mode.
module Ouroboros.Network.BlockFetch.Decision.BulkSync (
  fetchDecisionsBulkSync
) where

import Control.Monad (filterM)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Control.Monad.Writer.CPS (Writer, runWriter, MonadWriter (tell))
import Data.Bifunctor (first, Bifunctor (..))
import Data.Foldable (foldl')
import Data.List (sortOn)
import Data.List.NonEmpty (nonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Set as Set
import Data.Ord (Down(Down))

import Cardano.Prelude (guard)

import Ouroboros.Network.AnchoredFragment (AnchoredFragment, headBlockNo)
import qualified Ouroboros.Network.AnchoredFragment as AF
import Ouroboros.Network.Block
import Ouroboros.Network.BlockFetch.ClientState (FetchRequest (..), PeersOrder (..), mcons, PeerFetchBlockInFlight (..), PeerFetchStatus (..), PeerFetchInFlight (..))
import Ouroboros.Network.BlockFetch.ConsensusInterface (FetchMode(FetchModeBulkSync))
import Ouroboros.Network.BlockFetch.DeltaQ (calculatePeerFetchInFlightLimits)

import Ouroboros.Network.BlockFetch.Decision.Common
-- REVIEW: We should not import anything from 'Decision.Deadline'; if the need
-- arises, we should move the interesting piece of code to 'Decision.Common'.
-- This is to be done on demand.

-- | A trivial foldable data structure with a 'Semigroup' instance that
-- concatenates in @O(1)@. Only meant for short-term use, followed by one fold.
data ListConcat a = List [a] | Concat (ListConcat a) (ListConcat a)

instance Semigroup (ListConcat a) where
  (<>) = Concat

instance Monoid (ListConcat a) where
  mempty = List []

listConcatToList :: ListConcat a -> [a]
listConcatToList = flip go []
  where
    go (List xs) acc = xs ++ acc
    go (Concat x y) acc = go x (go y acc)

type WithDeclined peer = Writer (ListConcat (FetchDecline, peer))

runWithDeclined :: WithDeclined peer a -> (a, ListConcat (FetchDecline, peer))
runWithDeclined = runWriter

-- | Given a list of candidate fragments and their associated peers, choose what
-- to sync from who in the bulk sync mode.
fetchDecisionsBulkSync ::
  ( HasHeader header,
    HeaderHash header ~ HeaderHash block,
    Eq peer
  ) =>
  FetchDecisionPolicy header ->
  -- | The current chain, anchored at the immutable tip.
  AnchoredFragment header ->
  (Point block -> Bool) ->
  MaxSlotNo ->
  PeersOrder peer ->
  -- | Association list of the candidate fragments and their associated peers.
  -- The candidate fragments are anchored in the current chain (not necessarily
  -- at the tip; and not necessarily forking off immediately).
  [(AnchoredFragment header, PeerInfo header peer extra)] ->
  -- | Association list of the requests and their associated peers. There is at
  -- most one accepted request; everything else is declined. Morally, this is a
  -- map from peers to @'FetchDecision' ('FetchRequest' header)@ with at most
  -- one @'FetchRequest' header@.
  ( Maybe (FetchRequest header, PeerInfo header peer extra),
    [(FetchDecline, PeerInfo header peer extra)]
  )
fetchDecisionsBulkSync
  fetchDecisionPolicy
  currentChain
  fetchedBlocks
  fetchedMaxSlotNo
  peersOrder
  candidatesAndPeers = combineWithDeclined $ do
    -- Step 1: Select the candidate to sync from. This already eliminates peers
    -- that have an implausible candidate. It returns the remaining candidates
    -- (with their corresponding peer) as suffixes of the immutable tip.
    ( theCandidate :: ChainSuffix header,
      candidatesAndPeers' :: [(ChainSuffix header, PeerInfo header peer extra)]
      ) <-
      MaybeT $
        selectTheCandidate
          fetchDecisionPolicy
          currentChain
          candidatesAndPeers

    -- Step 2: Filter out from the chosen candidate fragment the blocks that
    -- have already been downloaded, or that have a request in flight (except
    -- for the requests in flight that are ignored).
    let (theFragments :: FetchDecision (CandidateFragments header)) =
          pure theCandidate
            >>= filterNotAlreadyFetched fetchedBlocks fetchedMaxSlotNo
            >>= filterNotAlreadyInFlightWithAnyPeerNonIgnored candidatesAndPeers

    -- Step 3: Select the peer to sync from. This eliminates peers that cannot
    -- serve a reasonable batch of the candidate, then chooses the peer to sync
    -- from, then again declines the others.
    ( thePeerCandidate :: ChainSuffix header,
      thePeer :: PeerInfo header peer extra
      ) <-
      MaybeT $
        selectThePeer
          fetchDecisionPolicy
          peersOrder
          theFragments
          candidatesAndPeers'

    -- Step 4: Fetch the candidate from the selected peer, potentially declining
    -- it (eg. if the peer is already too busy).
    MaybeT $
      fetchTheCandidate
        fetchDecisionPolicy
        theFragments
        thePeer
        thePeerCandidate
    where
      combineWithDeclined ::
        MaybeT (WithDeclined peer) (a, peer) ->
        ( Maybe (a, peer),
          [(FetchDecline, peer)]
        )
      combineWithDeclined = second listConcatToList . runWithDeclined . runMaybeT

-- FIXME: The 'FetchDeclineConcurrencyLimit' should only be used for
-- 'FetchModeDeadline', and 'FetchModeBulkSync' should have its own reasons.

-- | Given a list of candidate fragments and their associated peers, select the
-- candidate to sync from. Return this fragment, the list of peers that are
-- still in race to serve it, and the list of peers that are already being
-- declined.
selectTheCandidate ::
  forall header peerInfo.
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
  WithDeclined
    peerInfo
    (Maybe (ChainSuffix header, [(ChainSuffix header, peerInfo)]))
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
      separateDeclinedAndStillInRace ::
        [(FetchDecision (ChainSuffix header), peerInfo)] ->
        WithDeclined peerInfo (Maybe (ChainSuffix header, [(ChainSuffix header, peerInfo)]))
      separateDeclinedAndStillInRace decisions = do
        inRace <-
          catMaybes
            <$> traverse
              ( \(decision, peer) ->
                  case decision of
                    Left reason -> tell (List [(reason, peer)]) >> pure Nothing
                    Right candidate -> pure $ Just (candidate, peer)
              )
              decisions
        return $ ((,inRace) . fst . NE.head) <$> nonEmpty inRace

-- | Given _the_ candidate fragment to sync from, and a list of peers (with
-- their corresponding candidate fragments), choose which peer to sync _the_
-- candidate fragment from.
--
-- We first filter out all the peers that cannot even serve a reasonable batch
-- of _the_ candidate fragment, and then we choose the first one according to
-- the ordering passed as argument.
--
-- PRECONDITION: The set of peers must be included in the peer order queue.
selectThePeer ::
  forall header peer extra.
  ( HasHeader header,
    Eq peer
  ) =>
  FetchDecisionPolicy header ->
  PeersOrder peer ->
  -- | The candidate fragment that we have selected to sync from, as suffix of
  -- the immutable tip.
  FetchDecision (CandidateFragments header) ->
  -- | Association list of candidate fragments (as suffixes of the immutable
  -- tip) and their associated peers.
  [(ChainSuffix header, PeerInfo header peer extra)] ->
  WithDeclined
    (PeerInfo header peer extra)
    (Maybe (ChainSuffix header, PeerInfo header peer extra))
selectThePeer
  FetchDecisionPolicy {blockFetchSize}
  peersOrder
  theFragments
  candidates = do
    -- Create a fetch request for the blocks in question. The request is made
    -- to fit in 1MB but ignores everything else. It is gross in that sense.
    -- It will only be used to choose the peer to fetch from, but we will
    -- later craft a more refined request for that peer.
    let (grossRequest :: FetchDecision (FetchRequest header)) =
          selectBlocksUpToLimits
            blockFetchSize
            0 -- number of request in flight
            maxBound -- maximum number of requests in flight
            0 -- bytes in flight
            (1024 * 1024) -- maximum bytes in flight; one megabyte
            . snd
            <$> theFragments

    -- For each peer, check whether its candidate contains the gross request
    -- in its entirety, otherwise decline it.
    peers <-
      filterM
        ( \(candidate, peer) ->
            case checkRequestInCandidate candidate =<< grossRequest of
              Left reason -> tell (List [(reason, peer)]) >> pure False
              Right () -> pure True
        )
        candidates

    -- Order the peers according to the peer order that we have been given,
    -- then separate between declined peers and the others.
    let peersOrdered =
          [ (candidate, peerInfo)
            | (candidate, peerInfo@(_, _, _, peer, _)) <- peers,
              peer' <- mcons (peersOrderCurrent peersOrder) (peersOrderOthers peersOrder),
              peer == peer'
          ]

    -- Return the first peer in that order, and decline all the ones that were
    -- not already declined.
    case peersOrdered of
      [] -> return Nothing
      (thePeerCandidate, thePeer) : otherPeers -> do
        tell $ List $ map (first (const (FetchDeclineConcurrencyLimit FetchModeBulkSync 1))) otherPeers
        return $ Just (thePeerCandidate, thePeer)
    where
      checkRequestInCandidate ::
        ChainSuffix header -> FetchRequest header -> FetchDecision ()
      checkRequestInCandidate candidate request =
        if all isSubfragmentOfCandidate $ fetchRequestFragments request
          then pure ()
          else Left $ FetchDeclineAlreadyFetched -- FIXME: A custom decline reason for this?
        where
          isSubfragmentOfCandidate fragment =
            AF.withinFragmentBounds (AF.anchorPoint fragment) (getChainSuffix candidate)
              && AF.withinFragmentBounds (AF.headPoint fragment) (getChainSuffix candidate)

-- | Given a candidate and a peer to sync from, create a request for that
-- specific peer. We might take the 'FetchDecision' to decline the request, but
-- only for “good” reasons, eg. if the peer is already too busy.
fetchTheCandidate ::
  ( HasHeader header
  ) =>
  FetchDecisionPolicy header ->
  -- | The candidate fragment that we have selected to sync from, as suffix of
  -- the immutable tip.
  FetchDecision (CandidateFragments header) ->
  -- | The peer that we have selected to sync from.
  PeerInfo header peer extra ->
  -- | Its candidate fragment as suffix of the immutable tip.
  ChainSuffix header ->
  WithDeclined
    (PeerInfo header peer extra)
    (Maybe (FetchRequest header, PeerInfo header peer extra))
fetchTheCandidate
  fetchDecisionPolicy
  theFragments
  thePeer@(status, inflight, gsvs, _, _)
  thePeerCandidate =
    let theDecision = do
          -- Keep blocks that have not already been downloaded or that are not
          -- already in-flight with this peer.
          fragments <- filterNotAlreadyInFlightWithPeer inflight =<< theFragments

          -- Trim the fragments to the peer's candidate, keeping only blocks that
          -- they may actually serve.
          trimmedFragments <- trimFragmentsToCandidate thePeerCandidate (snd fragments)

          -- Try to create a request for those fragments.
          fetchRequestDecision
            fetchDecisionPolicy
            FetchModeBulkSync
            0 -- bypass all concurrency limits. REVIEW: is this really what we want?
            (calculatePeerFetchInFlightLimits gsvs)
            inflight
            status
            (Right trimmedFragments) -- FIXME: This is a hack to avoid having to change the signature of 'fetchRequestDecisions'.
     in case theDecision of
          Left reason -> tell (List [(reason, thePeer)]) >> pure Nothing
          Right theRequest -> pure $ Just (theRequest, thePeer)
    where
      trimFragmentsToCandidate candidate fragments =
        let trimmedFragments =
              mapMaybe
                ( \fragment ->
                    -- 'candidate' is anchored at the immutable tip, so we don't
                    -- need to look for something more complicated than this.
                    (\(_, prefix, _, _) -> prefix)
                      <$> AF.intersect (getChainSuffix candidate) fragment
                )
                fragments
         in if null trimmedFragments
              then Left FetchDeclineAlreadyFetched
              else Right trimmedFragments

filterNotAlreadyInFlightWithAnyPeerNonIgnored ::
  (HasHeader header) =>
  [(any, PeerInfo header peer extra)] ->
  CandidateFragments header ->
  FetchDecision (CandidateFragments header)
filterNotAlreadyInFlightWithAnyPeerNonIgnored candidates theCandidate = do
  let theFragments =
        concatMap
          ( filterWithMaxSlotNo
              notAlreadyInFlightNonIgnored
              maxSlotNoInFlightWithPeers
          )
          (snd theCandidate)
  guard (not (null theFragments)) ?! FetchDeclineInFlightOtherPeer
  return $ (fst theCandidate, theFragments)
  where
    notAlreadyInFlightNonIgnored b =
      blockPoint b `Set.notMember` blocksInFlightWithPeersNonIgnored
    -- All the blocks that are already in-flight with all peers and not ignored.
    blocksInFlightWithPeersNonIgnored =
      Set.unions
        [ case status of
            PeerFetchStatusShutdown -> Set.empty
            PeerFetchStatusStarting -> Set.empty
            PeerFetchStatusAberrant -> Set.empty
            _other -> Map.keysSet $ Map.filter (\(PeerFetchBlockInFlight b) -> not b) $ peerFetchBlocksInFlight inflight
          | (_, (status, inflight, _, _, _)) <- candidates
        ]
    -- The highest slot number that is or has been in flight for any peer.
    maxSlotNoInFlightWithPeers =
      foldl'
        max
        NoMaxSlotNo
        [peerFetchMaxSlotNo inflight | (_, (_, inflight, _, _, _)) <- candidates]
