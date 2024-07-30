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
-- Definitions:
--
-- - Let @inflight :: peer -> Set blk@ be the outstanding blocks, those that
--   have been requested and are expected to arrive but have not yet.
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
-- 0. If @inflight(currentPeer)@ is non-empty and the block validation component
--    has idled at any point after @currentStart@ plus @gracePeriod@, then the
--    peer @currentPeer@ has failed to promptly serve @inflight(currentPeer)@,
--    and:
--
--   - If @currentPeer@ is the ChainSync Jumping dynamo, then it must
--     immediately be replaced as the dynamo.
--
--   - Stop considering the peer “current” and make them the worst according to
--     the @peersOrder@.
--
-- 1. Select @theCandidate :: AnchoredFragment (Header blk)@. This is the best
--    candidate header chain among the ChainSync clients (eg. best raw
--    tiebreaker among the longest).
--
-- 2. Select @thePeer :: peer@.
--
--    - Let @grossRequest@ be the oldest block on @theCandidate@ that has not
--      already been downloaded.
--
--    - If @grossRequest@ is empty, then terminate this iteration. Otherwise,
--      pick the best peer (according to @peersOrder@) offering the block in
--      @grossRequest@.
--
-- 3. Craft the actual request to @thePeer@ asking blocks of @theCandidate@:
--
--    - If the byte size of @inflight(thePeer)@ is below the low-water mark,
--      then terminate this iteration.
--
--    - Decide and send the actual next batch request, as influenced by exactly
--      which blocks are actually already currently in-flight with @thePeer@.
--
-- 4. If we went through the election of a new peer, replace @currentPeer@ and
--    put the new peer at the front of @peersOrder@. Also reset @currentStart@
--    if @inflights(thePeer)@ is empty.
--
-- Terminate this iteration.
--
-- About the influence of in-flight requests
-- -----------------------------------------
--
-- One can note that in-flight requests are ignored when finding a new peer, but
-- considered when crafting the actual request to a chosen peer. This is by
-- design. We explain the rationale here.
--
-- If a peer proves too slow, then we give up on it (see point 0. above), even
-- if it has requests in-flight. In subsequent selections of peers (point 2.),
-- the blocks in these requests will not be removed from @theCandidate@ as, as
-- far as we know, these requests might never return.
--
-- When crafting the actual request, we do need to consider the in-flight
-- requests of the peer, to avoid clogging our network. If some of these
-- in-flight requests date from when the peer was previously “current”, this
-- means that we cycled through all the peers that provided @theCandidate@ and
-- they all failed to serve our blocks promptly.
--
-- This is a degenerate case of the algorithm that might happen but only be
-- transient. Soon enough, @theCandidate@ should be honest (if the consensus
-- layer does its job correctly), and there should exist an honest peer ready to
-- serve @theCandidate@ promptly.
--
-- Interactions with ChainSync Jumping (CSJ)
-- -----------------------------------------
--
-- Because we always require our peers to be able to serve a gross request
-- with an old block, peers with longer chains have a better chance to pass
-- this criteria and to be selected as current peer. The CSJ dynamo, being
-- always ahead of jumpers, has therefore more chances to be selected as the
-- current peer. It is still possible for a jumper or a disengaged peer to be
-- selected.
--
-- If the current peer is the CSJ dynamo and it is a dishonest peer that retains
-- blocks, it will get multiple opportunities to do so since it will be selected
-- as the current peer more often. We therefore rotate the dynamo every time it
-- is the current peer and it fails to serve blocks promptly.
--
-- About the gross request
-- -----------------------
--
-- We want to select a peer that is able to serve us a batch of oldest blocks
-- of @theCandidate@. However, not every peer will be able to deliver these
-- batches as they might be on different chains. We therefore select a peer only
-- if its candidate fragment contains the block in the gross request. In this
-- way, we ensure that the peer can serve at least one block that we wish to
-- fetch.
--
-- If the peer cannot offer any more blocks after that, it will be rotated out
-- soon.
--
module Ouroboros.Network.BlockFetch.Decision.BulkSync (
  fetchDecisionsBulkSyncM
) where

import Control.Monad (guard)
import Control.Monad.Class.MonadTime.SI (MonadMonotonicTime (getMonotonicTime), addTime)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Control.Monad.Writer.Strict (Writer, runWriter, MonadWriter (tell))
import Control.Tracer (Tracer, traceWith)
import Data.Bifunctor (first, Bifunctor (..))
import Data.Function (on)
import qualified Data.List as List
import qualified Data.Set as Set
import Data.Maybe (maybeToList)

import Cardano.Prelude (partitionEithers)

import Ouroboros.Network.AnchoredFragment (AnchoredFragment, headBlockNo)
import qualified Ouroboros.Network.AnchoredFragment as AF
import Ouroboros.Network.Block
import Ouroboros.Network.BlockFetch.ClientState
         (FetchRequest (..), PeersOrder (..), peerFetchBlocksInFlight)
import Ouroboros.Network.BlockFetch.ConsensusInterface (FetchMode(FetchModeBulkSync))
import Ouroboros.Network.BlockFetch.DeltaQ (calculatePeerFetchInFlightLimits)
import Ouroboros.Network.BlockFetch.ConsensusInterface (ChainSelStarvation(..))

import Ouroboros.Network.BlockFetch.Decision.Deadline
import Ouroboros.Network.BlockFetch.Decision.Trace (TraceDecisionEvent (..))

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
  => Tracer m (TraceDecisionEvent peer header)
  -> FetchDecisionPolicy header
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
  tracer
  fetchDecisionPolicy@FetchDecisionPolicy {bulkSyncGracePeriod}
  currentChain
  fetchedBlocks
  fetchedMaxSlotNo
  chainSelStarvation
  ( peersOrder0,
    writePeersOrder,
    demoteCSJDynamo
    )
  candidatesAndPeers = do
    peersOrder1 <- checkLastChainSelStarvation peersOrder0

    let (peersOrder, orderedCandidatesAndPeers) =
          alignPeersOrderWithActualPeers
            (peerInfoPeer . snd)
            candidatesAndPeers
            peersOrder1

    -- Compute the actual block fetch decision. This contains only declines and
    -- at most one request. 'theDecision' is therefore a 'Maybe'.
    let (theDecision, declines) =
          fetchDecisionsBulkSync
            fetchDecisionPolicy
            currentChain
            fetchedBlocks
            fetchedMaxSlotNo
            orderedCandidatesAndPeers

        newCurrentPeer = peerInfoPeer . snd <$> theDecision

    case theDecision of
      Just (_, (_, inflight, _, _, _))
        | Set.null (peerFetchBlocksInFlight inflight)
       -- If there were no blocks in flight, then this will be the first request,
       -- so we take a new current time.
       -> do
          peersOrderStart <- getMonotonicTime
          writePeersOrder $ setCurrentPeer newCurrentPeer peersOrder
            { peersOrderStart }
        | newCurrentPeer /= peersOrderCurrent peersOrder0
       -- If the new current peer is not the old one, then we update the current
       -- peer
       ->
          writePeersOrder $ setCurrentPeer newCurrentPeer peersOrder
      _ -> pure ()

    pure $
      map (first Right) (maybeToList theDecision)
        ++ map (first Left) declines
    where
      -- Align the peers order with the actual peers; this consists in removing
      -- all peers from the peers order that are not in the actual peers list and
      -- adding at the end of the peers order all the actual peers that were not
      -- there before.
      alignPeersOrderWithActualPeers :: forall d.
        (d -> peer) -> [d] -> PeersOrder peer -> (PeersOrder peer, [d])
      alignPeersOrderWithActualPeers
        peerOf
        actualPeers
        PeersOrder {peersOrderStart, peersOrderCurrent, peersOrderAll} =
          let peersOrderAll' =
                [ d
                | p <- peersOrderAll
                , Just d <- [List.find ((p ==) . peerOf) actualPeers]
                ]
                ++ filter ((`notElem` peersOrderAll) . peerOf) actualPeers
              -- Set the current peer to Nothing if it is not at the front of
              -- the list.
              peersOrderCurrent' = do
                peer <- peersOrderCurrent
                guard (any ((peer ==) . peerOf) $ take 1 peersOrderAll')
                pure peer
           in (PeersOrder
                { peersOrderCurrent = peersOrderCurrent',
                  -- INVARIANT met: Current peer is at the front if it exists
                  peersOrderAll = map peerOf peersOrderAll',
                  peersOrderStart
                }
              , peersOrderAll'
              )

      -- If the chain selection has been starved recently, that is after the
      -- current peer started (and a grace period), then the current peer is
      -- bad. We push it at the end of the queue, demote it from CSJ dynamo,
      -- and ignore its in-flight blocks for the future.
      checkLastChainSelStarvation :: PeersOrder peer -> m (PeersOrder peer)
      checkLastChainSelStarvation
        peersOrder@PeersOrder {peersOrderStart, peersOrderCurrent, peersOrderAll} = do
          lastStarvationTime <- case chainSelStarvation of
            ChainSelStarvationEndedAt time -> pure time
            ChainSelStarvationOngoing -> getMonotonicTime
          case peersOrderCurrent of
            Just peer
              | lastStarvationTime >= addTime bulkSyncGracePeriod peersOrderStart -> do
                  traceWith tracer (PeerStarvedUs peer)
                  demoteCSJDynamo peer
                  pure PeersOrder
                         {
                           peersOrderCurrent = Nothing,
                           -- INVARIANT met: there is no current peer
                           peersOrderAll = drop 1 peersOrderAll ++ [peer],
                           peersOrderStart
                         }
            _ -> pure peersOrder

      setCurrentPeer :: Maybe peer -> PeersOrder peer -> PeersOrder peer
      setCurrentPeer Nothing peersOrder = peersOrder {peersOrderCurrent = Nothing}
      setCurrentPeer (Just peer) peersOrder =
        case break ((peer ==)) (peersOrderAll peersOrder) of
          (xs, p : ys) ->
            peersOrder
              { peersOrderCurrent = Just p,
                -- INVARIANT met: Current peer is at the front
                peersOrderAll = p : xs ++ ys
              }
          (_, []) -> peersOrder {peersOrderCurrent = Nothing}

-- | Given a list of candidate fragments and their associated peers, choose what
-- to sync from who in the bulk sync mode.
fetchDecisionsBulkSync :: forall header block peer extra.
  ( HasHeader header,
    HeaderHash header ~ HeaderHash block
  ) =>
  FetchDecisionPolicy header ->
  -- | The current chain, anchored at the immutable tip.
  AnchoredFragment header ->
  (Point block -> Bool) ->
  MaxSlotNo ->
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
    -- have already been downloaded. NOTE: if not declined, @theFragments@ is
    -- guaranteed to be non-empty.
    theFragments :: CandidateFragments header
      <- MaybeT $ dropAlreadyFetchedBlocks candidatesAndPeers' theCandidate

    -- Step 3: Select the peer to sync from. This eliminates peers that cannot
    -- serve a reasonable batch of the candidate, then chooses the peer to sync
    -- from, then again declines the others.
    ( thePeerCandidate :: ChainSuffix header,
      thePeer :: PeerInfo header peer extra
      ) <-
      MaybeT $ selectThePeer theFragments candidatesAndPeers'

    -- Step 4: Fetch the candidate from the selected peer, potentially declining
    -- it (eg. if the peer is already too busy).
    MaybeT $
      makeFetchRequest
        fetchDecisionPolicy
        theFragments
        thePeer
        thePeerCandidate
    where
      combineWithDeclined :: forall peerInfo a.
        MaybeT (WithDeclined peerInfo) (a, peerInfo) ->
        ( Maybe (a, peerInfo),
          [(FetchDecline, peerInfo)]
        )
      combineWithDeclined = second listConcatToList . runWithDeclined . runMaybeT

      dropAlreadyFetchedBlocks :: forall peerInfo.
        [(ChainSuffix header, peerInfo)] ->
        ChainSuffix header ->
        WithDeclined peerInfo (Maybe (CandidateFragments header))
      dropAlreadyFetchedBlocks candidatesAndPeers' theCandidate =
        case dropAlreadyFetched fetchedBlocks fetchedMaxSlotNo theCandidate of
          Left reason -> do
            tell (List [(reason, peerInfo) | (_, peerInfo) <- candidatesAndPeers'])
            pure Nothing
          Right theFragments -> pure (Just theFragments)

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
    where
      -- Write all of the declined peers, and find the longest candidate
      -- fragment if there is any.
      separateDeclinedAndStillInRace ::
        [(FetchDecision (ChainSuffix header), peerInfo)] ->
        WithDeclined peerInfo (Maybe (ChainSuffix header, [(ChainSuffix header, peerInfo)]))
      separateDeclinedAndStillInRace decisions = do
        let (declined, inRace) = partitionEithers
              [ bimap ((,p)) ((,p)) d | (d, p) <- decisions ]
        tell (List declined)
        case inRace of
          [] -> pure Nothing
          _ : _ -> do
            let chainSfx = fst $
                  List.maximumBy (compare `on` (headBlockNo . getChainSuffix . fst))  inRace
            pure $ Just (chainSfx, inRace)

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
  HasHeader header =>
  -- | The candidate fragment that we have selected to sync from, as suffix of
  -- the immutable tip.
  CandidateFragments header ->
  -- | Association list of candidate fragments (as suffixes of the immutable
  -- tip) and their associated peers.
  [(ChainSuffix header, PeerInfo header peer extra)] ->
  WithDeclined
    (PeerInfo header peer extra)
    (Maybe (ChainSuffix header, PeerInfo header peer extra))
selectThePeer
  theFragments
  candidates = do
    -- Create a fetch request for the blocks in question. The request has exactly
    -- 1 block. It will only be used to choose the peer to fetch from, but we will
    -- later craft a more refined request for that peer. See [About the gross
    -- request] in the module documentation. Because @theFragments@ is not
    -- empty, @grossRequest@ will not be empty.
    let firstBlock = FetchRequest . map (AF.takeOldest 1) . take 1 . filter (not . AF.null)
        (grossRequest :: FetchRequest header) = firstBlock $ snd theFragments

    -- Return the first peer that can serve the gross request and decline
    -- the other peers.
    go grossRequest candidates
  where
    go grossRequest (c@(candidate, peerInfo) : xs) = do
      if requestHeadInCandidate candidate grossRequest then do
        tell $ List
          [(FetchDeclineConcurrencyLimit FetchModeBulkSync 1, pInfo)
          | (_, pInfo) <- xs
          ]
        pure (Just c)
      else do
        tell $ List [(FetchDeclineAlreadyFetched, peerInfo)]
        go grossRequest xs
    go _grossRequest [] = pure Nothing


    requestHeadInCandidate :: ChainSuffix header -> FetchRequest header -> Bool
    requestHeadInCandidate candidate request =
      case fetchRequestFragments request of
        fragments@(_:_)
          | AF.withinFragmentBounds
              (AF.headPoint $ last fragments)
              (getChainSuffix candidate)
          ->
            True
        _ ->
            False

-- | Given a candidate and a peer to sync from, create a request for that
-- specific peer. We might take the 'FetchDecision' to decline the request, but
-- only for “good” reasons, eg. if the peer is already too busy.
makeFetchRequest ::
  ( HasHeader header
  ) =>
  FetchDecisionPolicy header ->
  -- | The candidate fragment that we have selected to sync from, as suffix of
  -- the immutable tip.
  CandidateFragments header ->
  -- | The peer that we have selected to sync from.
  PeerInfo header peer extra ->
  -- | Its candidate fragment as suffix of the immutable tip.
  ChainSuffix header ->
  WithDeclined
    (PeerInfo header peer extra)
    (Maybe (FetchRequest header, PeerInfo header peer extra))
makeFetchRequest
  fetchDecisionPolicy
  theFragments
  thePeer@(status, inflight, gsvs, _, _)
  thePeerCandidate =
    let theDecision = do
          -- Drop blocks that are already in-flight with this peer.
          fragments <- dropAlreadyInFlightWithPeer inflight theFragments

          -- Trim the fragments to the peer's candidate, keeping only blocks that
          -- they may actually serve.
          trimmedFragments <- trimFragmentsToCandidate thePeerCandidate (snd fragments)

          -- Try to create a request for those fragments.
          fetchRequestDecision
            fetchDecisionPolicy
            FetchModeBulkSync
            0 -- bypass all concurrency limits.
            (calculatePeerFetchInFlightLimits gsvs)
            inflight
            status
            (Right trimmedFragments)
     in case theDecision of
          Left reason -> tell (List [(reason, thePeer)]) >> pure Nothing
          Right theRequest -> pure $ Just (theRequest, thePeer)
    where
      trimFragmentsToCandidate candidate fragments =
        let trimmedFragments =
              [ prefix
              | fragment <- fragments
              , Just (_, prefix, _, _) <- [AF.intersect (getChainSuffix candidate) fragment]
              , not (AF.null prefix)
              ]
         in if null trimmedFragments
              then Left FetchDeclineAlreadyFetched
              else Right trimmedFragments
