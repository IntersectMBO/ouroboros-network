{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | BulkSync decision logic
--
-- This module contains the part of the block fetch decisions process that is
-- specific to the bulk sync mode. This logic reuses parts of the logic for the
-- deadline mode, but it is inherently different.
--
-- Natural language specification
-- ------------------------------
--
-- Definitions:
--
-- - Let @inflight :: peer -> Set blk@ be the outstanding blocks, those that
--   have been requested and are expected to arrive but have not yet.
--
-- - Let @inflightIgnored :: Set blk@ be the outstanding blocks that have been
--   ignored. We have that @inflightIgnored@ is included in the union for all
--   peer @p@ of @inflight(p)@. We name @inflightNonIgnored@ the difference.
--
-- - Let @peersOrder@ be an order of preference among the peers. This order is
--   not set in stone and will evolve as we go.
--
-- - Let @currentPeer :: Maybe peer@ be the “current peer” with which we are
--   interacting. If it exists, this peer must be the best according to
--   @peersOrder@, and the last fetch request must have been sent to them.
--
-- - Let @currentStart :: Time@ be the latest time a fetch request was sent
--   while there were no outstanding blocks.
--
-- - Let @gracePeriod@ be a small duration (eg. 10s), during which a “cold” peer
--   is allowed to warm up (eg. grow the TCP window) before being expected to
--   feed blocks faster than we can validate them.
--
-- One iteration of this decision logic:
--
-- - If @inflight@ is non-empty and the block validation component has idled at
--   any point after @currentStart@ plus @gracePeriod@, then the peer
--   @currentPeer@ has failed to promptly serve @inflight(currentPeer)@, and:
--
--   - If @currentPeer@ is the ChainSync Jumping dynamo, then it must
--     immediately be replaced as the dynamo.
--
--   - Assume @currentPeer@ will never finish replying to that fetch request and
--     add all of @inflight(currentPeer)@ to @inflightIgnored@. REVIEW: Nick's
--     description says to add all of @inflight@ (for all the peers) to
--     @inflightIgnored@, probably because it assumes/enforces that @inflight ==
--     inflight(currentPeer)@, that is only the current peer is allowed to have
--     in-flight blocks.
--
--   - Stop considering the peer “current” and make them the worst according to
--     the @peersOrder@.
--
-- - Let @theCandidate :: AnchoredFragment (Header blk)@ be the best candidate
--   header chain among the ChainSync clients (eg. best raw tiebreaker among the
--   longest).
--
-- - Let @grossRequest@ be the oldest blocks on @theCandidate@ that have not
--   already been downloaded, are not in @inflightNonIgnored@, and total less
--   than 20 mebibytes.
--
-- - If @grossRequest@ is empty, then terminate this iteration. Otherwise, pick
--   the best peer (according to @peersOrder@) offering all of the blocks in
--   @grossRequest@. We will call it @thePeer@. Because @currentPeer@, if it
--   exists, is the best according to @peersOrder@, then it will be our
--   preferred peer, as long as it can provide the @grossRequest@s.
--
-- - If the byte size of @inflight(thePeer)@ is below the low-water mark, then
--   terminate this iteration. Otherwise, decide and send the actual next batch
--   request, as influenced by exactly which blocks are actually already
--   currently in-flight with the chosen peer.
--
-- - Update @currentPeer@ and @currentStart@, if needed. Namely:
--
--   - If @thePeer /= currentPeer@, then make @thePeer@ the current peer and the
--     best according to @peersOrder@, and reset @currentStart@ to now.
--
--   - If @thePeer == currentPeer@, but @inflight(thePeer)@ is empty, the reset
--     @currentStart@ to now.
--
-- Terminate this iteration.
--
-- Interactions with ChainSync Jumping (CSJ)
-- -----------------------------------------
--
-- This decision logic is not so obviously coupled with CSJ, but it is in some
-- subtle ways:
--
-- - Because we always require our peers to be able to serve a gross request of
--   oldest blocks, peers with longer chains have a better chance to pass this
--   criteria and to be selected as current peer. The CSJ dynamo, being always
--   ahead of jumpers, has therefore more chances to be selected as the current
--   peer. It is still possible for a jumper or a disengaged peer to be
--   selected.
--
-- - If the current peer is the CSJ dynamo, but it is a dishonest peer serving
--   headers fast but retaining headers, it might be able to drastically leash
--   us, because its ChainSync client will be stuck behind the forecast horizon
--   (and therefore not subject to ChainSync punishments such as the Limit on
--   Patience). This is why we need to consider starvation of ChainSel and
--   demote peers that let us starve.
--
-- About the gross request
-- -----------------------
--
-- Morally, we want to select a peer that is able to serve us a batch of oldest
-- blocks of @theCandidate@. However, the actual requests depend not only on the
-- size of the blocks to fetch, but also on the network performances of the peer
-- and what requests it already has in-flight. Looking at what peer can create
-- an actual request for @theCandidate@ can be misleading: indeed, our
-- @currentPeer@ might not be able to create a request simply because it is
-- already busy answering other requests from us. This calls for the
-- introduction of an objective criterium, which the gross request provides.
--
-- If the gross request is included in a peer's candidate, it means that this
-- peer can serve at least the first 20 mebibytes of the blocks that we wish to
-- fetch. The actual request might be smaller than that, depending on the actual
-- in-flight limits, but it might also be bigger because the peer can have more
-- blocks than just those.
--
module Ouroboros.Network.BlockFetch.Decision.BulkSync (
  fetchDecisionsBulkSyncM
) where

import Control.Exception (assert)
import Control.Monad (filterM, when)
import Control.Monad.Class.MonadTime.SI (MonadMonotonicTime (getMonotonicTime), addTime)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Control.Monad.Writer.Strict (Writer, runWriter, MonadWriter (tell))
import Data.Bifunctor (first, Bifunctor (..))
import Data.Foldable (foldl')
import qualified Data.List as List
import Data.List.NonEmpty (nonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe, maybeToList, fromJust)
import qualified Data.Set as Set
import Data.Ord (Down(Down))

import Cardano.Prelude (guard, partitionEithers, (&))

import Ouroboros.Network.AnchoredFragment (AnchoredFragment, headBlockNo)
import qualified Ouroboros.Network.AnchoredFragment as AF
import Ouroboros.Network.Block
import Ouroboros.Network.BlockFetch.ClientState (FetchRequest (..), PeersOrder (..), PeerFetchBlockInFlight (..), PeerFetchStatus (..), PeerFetchInFlight (..))
import Ouroboros.Network.BlockFetch.ConsensusInterface (FetchMode(FetchModeBulkSync))
import Ouroboros.Network.BlockFetch.DeltaQ (calculatePeerFetchInFlightLimits)
import Ouroboros.Network.BlockFetch.ConsensusInterface (ChainSelStarvation(..))

import Ouroboros.Network.BlockFetch.Decision.Deadline

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

fetchDecisionsBulkSyncM
  :: forall peer header block m extra.
     (Ord peer,
      HasHeader header,
      HeaderHash header ~ HeaderHash block, MonadMonotonicTime m)
  => FetchDecisionPolicy header
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
fetchDecisionsBulkSyncM
  fetchDecisionPolicy@FetchDecisionPolicy {bulkSyncGracePeriod}
  currentChain
  fetchedBlocks
  fetchedMaxSlotNo
  chainSelStarvation
  ( peersOrder0,
    writePeersOrder,
    demoteCSJDynamoAndIgnoreInflightBlocks
    )
  candidatesAndPeers = do
    peersOrder <-
      peersOrder0
        -- Align the peers order with the actual peers; this consists in removing
        -- all peers from the peers order that are not in the actual peers list and
        -- adding at the end of the peers order all the actual peers that were not
        -- there before.
        & alignPeersOrderWithActualPeers
          (map (\(_, (_, _, _, peer, _)) -> peer) candidatesAndPeers)
        -- If the chain selection has been starved recently, that is after the
        -- current peer started (and a grace period), then the current peer is
        -- bad. We push it at the end of the queue, demote it from CSJ dynamo,
        -- and ignore its in-flight blocks for the future.
        & checkLastChainSelStarvation

    -- Compute the actual block fetch decision. This contains only declines and
    -- at most one request. 'theDecision' is therefore a 'Maybe'.
    let (theDecision, declines) =
          fetchDecisionsBulkSync
            fetchDecisionPolicy
            currentChain
            fetchedBlocks
            fetchedMaxSlotNo
            peersOrder
            mCurrentPeer
            candidatesAndPeers

    -- If there were no blocks in flight, then this will be the first request,
    -- so we take a new current time.
    when (mCurrentPeer == Nothing) $ do
      peersOrderStart <- getMonotonicTime
      writePeersOrder $ peersOrder {peersOrderStart}

    pure $
      map (first Right) (maybeToList theDecision)
        ++ map (first Left) declines
    where
      alignPeersOrderWithActualPeers :: [peer] -> PeersOrder peer -> PeersOrder peer
      alignPeersOrderWithActualPeers
        actualPeers
        PeersOrder {peersOrderStart, peersOrderAll} =
          let peersOrderAll' =
                filter (`elem` actualPeers) peersOrderAll
                  ++ filter (\peer -> peer `notElem` peersOrderAll) actualPeers
           in PeersOrder
                { peersOrderAll = peersOrderAll',
                  peersOrderStart
                }

      checkLastChainSelStarvation :: PeersOrder peer -> m (PeersOrder peer)
      checkLastChainSelStarvation
        peersOrder@PeersOrder {peersOrderStart, peersOrderAll} = do
          lastStarvationTime <- case chainSelStarvation of
            ChainSelStarvationEndedAt time -> pure time
            ChainSelStarvationOngoing -> getMonotonicTime
          case mCurrentPeer of
            Just badPeer ->
                if lastStarvationTime >= addTime bulkSyncGracePeriod peersOrderStart
                  then do
                    demoteCSJDynamoAndIgnoreInflightBlocks badPeer
                    let peersOrder' =
                          PeersOrder
                            { peersOrderAll = filter (/= badPeer) peersOrderAll ++ [badPeer],
                              peersOrderStart
                            }
                    writePeersOrder peersOrder'
                    pure peersOrder'
                  else pure peersOrder
            Nothing -> pure peersOrder

      mCurrentPeer =
        let peersWithBlocksInFlightNonIgnored =
              filter
                ( \(_, inflight, _, _, _) ->
                    not $ Map.null $ Map.filter (\(PeerFetchBlockInFlight b) -> not b) $ peerFetchBlocksInFlight inflight
                )
                (map snd candidatesAndPeers)
         in case peersWithBlocksInFlightNonIgnored of
              (_, _, _, peer, _) : otherPeersWithBlocksInFlightNonIgnored ->
                assert (List.null otherPeersWithBlocksInFlightNonIgnored) $
                  Just peer
              _ -> Nothing

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
  -- | The current peer, if there is one.
  Maybe peer ->
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
  mCurrentPeer
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
    -- for the requests in flight that are ignored). NOTE: if not declined,
    -- @theFragments@ is guaranteed to be non-empty.
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
          mCurrentPeer
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
      . List.sortOn (Down . headBlockNo . fst)
    where
      -- Very ad-hoc helper.
      -- Write all of the declined peers, and find the candidate fragment
      -- if there is any.
      separateDeclinedAndStillInRace ::
        [(FetchDecision (ChainSuffix header), peerInfo)] ->
        WithDeclined peerInfo (Maybe (ChainSuffix header, [(ChainSuffix header, peerInfo)]))
      separateDeclinedAndStillInRace decisions = do
        let (declined, inRace) = partitionEithers
              [ bimap ((,p)) ((,p)) d | (d, p) <- decisions ]
        tell (List declined)
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
-- PRECONDITION: The given candidate fragments must not be empty.
selectThePeer ::
  forall header peer extra.
  ( HasHeader header,
    Eq peer
  ) =>
  FetchDecisionPolicy header ->
  PeersOrder peer ->
  -- | The current peer
  Maybe peer ->
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
  mCurrentPeer
  theFragments
  candidates = do
    -- Create a fetch request for the blocks in question. The request is made to
    -- fit in 20 mebibytes but ignores everything else. It is gross in that
    -- sense. It will only be used to choose the peer to fetch from, but we will
    -- later craft a more refined request for that peer. See [About the gross
    -- request] in the module documentation. Because @theFragments@ is not
    -- empty, @grossRequest@ will not be empty.
    let (grossRequest :: FetchDecision (FetchRequest header)) =
          selectBlocksUpToLimits
            blockFetchSize
            0 -- number of request in flight
            maxBound -- maximum number of requests in flight
            0 -- bytes in flight
            (20 * 1024 * 1024) -- maximum bytes in flight; 20 mebibyte
            . snd
            <$> theFragments

    -- If there is a current peer, then that is the one we choose. Otherwise, we
    -- can choose any peer, so we choose a “good” one.
    case mCurrentPeer of
      Just thePeer -> do
          let ((thePeerCandidate, thePeerInfo), otherPeers) = fromJust $ extractFirstElem (\(_, (_, _, _, peer, _)) -> peer == thePeer) candidates
          tell (List (map (first (const (FetchDeclineConcurrencyLimit FetchModeBulkSync 1))) otherPeers))
          -- REVIEW: This is maybe overkill to check that the whole gross request
          -- fits in the peer's candidate. Maybe just checking that there is one
          -- block is sufficient.
          case checkRequestInCandidate thePeerCandidate =<< grossRequest of
            Left reason -> tell (List [(reason, thePeerInfo)]) >> return Nothing
            Right () -> return $ Just (thePeerCandidate, thePeerInfo)

      Nothing -> do
        -- For each peer, check whether its candidate contains the gross request in
        -- its entirety, otherwise decline it. This will guarantee that the
        -- remaining peers can serve the refined request that we will craft later.
        peers <-
          filterM
            ( \(candidate, peer) ->
                case checkRequestInCandidate candidate =<< grossRequest of
                  Left reason -> tell (List [(reason, peer)]) >> pure False
                  Right () -> pure True
            )
            candidates

        -- Order the peers according to the peer order that we have been given, then
        -- separate between declined peers and the others. NOTE: The order in which
        -- we bind the lists in the comprehension is capital.
        let peersOrdered =
              [ (candidate, peerInfo)
                | peer' <- peersOrderAll peersOrder,
                  (candidate, peerInfo@(_, _, _, peer, _)) <- peers,
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

      peersWithBlocksInFlightNonIgnored =
        filter
          ( \(_, inflight, _, _, _) ->
              not $ Map.null $ Map.filter (\(PeerFetchBlockInFlight b) -> not b) $ peerFetchBlocksInFlight inflight
          )
          (map snd candidates)

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
          -- Keep blocks that are not already in-flight with this peer. NOTE: We
          -- already filtered most of them (and more), but now we also filter
          -- out then ones that are in-flight AND ignored.
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
            (Right trimmedFragments) -- FIXME: This is a hack to avoid having to change the signature of 'fetchRequestDecision'.
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

extractFirstElem :: (a -> Bool) -> [a] -> Maybe (a, [a])
extractFirstElem _ [] = Nothing
extractFirstElem p (x : xs)
  | p x = Just (x, xs)
  | otherwise = second (x :) <$> extractFirstElem p xs
