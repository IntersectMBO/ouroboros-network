{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module Ouroboros.Network.BlockFetch.Decision.Genesis
  ( -- * Genesis decision logic
    --
    -- | This module contains the part of the block fetch decisions process that is
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

    -- * About the influence of in-flight requests
    --
    -- | One can note that in-flight requests are ignored when finding a new peer, but
    -- considered when crafting the actual request to a chosen peer. This is by
    -- design. We explain the rationale here.
    --
    -- If a peer proves too slow, then we give up on it (see point 0. above), even
    -- if it has requests in-flight. In subsequent selections of peers (point 2.),
    -- the blocks in these requests will not be removed from @theCandidate@ as, as
    -- far as we know, these requests might not return (until the connection to that
    -- peer is terminated by the mini protocol timeout).
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

    -- * Interactions with ChainSync Jumping (CSJ)
    --
    -- | Because we always require our peers to be able to serve a gross request
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

    -- * About the gross request
    --
    -- | We want to select a peer that is able to serve us a batch of oldest blocks
    -- of @theCandidate@. However, not every peer will be able to deliver these
    -- batches as they might be on different chains. We therefore select a peer only
    -- if its candidate fragment contains the block in the gross request. In this
    -- way, we ensure that the peer can serve at least one block that we wish to
    -- fetch.
    --
    -- If the peer cannot offer any more blocks after that, it will be rotated out
    -- soon.
    fetchDecisionsGenesisM
  ) where

import Control.Exception (assert)
import Control.Monad (guard)
import Control.Monad.Class.MonadTime.SI (MonadMonotonicTime (getMonotonicTime),
           addTime)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Control.Monad.Trans.Writer.CPS (Writer, runWriter, tell)
import Control.Tracer (Tracer, traceWith)
import Data.Bifunctor (Bifunctor (..), first)
import Data.DList (DList)
import Data.DList qualified as DList
import Data.Foldable (find, toList)
import Data.List qualified as List
import Data.Maybe (maybeToList)
import Data.Sequence (Seq (..), (<|), (><), (|>))
import Data.Sequence qualified as Sequence
import Data.Set qualified as Set

import Cardano.Prelude (partitionEithers)

import Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import Ouroboros.Network.AnchoredFragment qualified as AF
import Ouroboros.Network.Block
import Ouroboros.Network.BlockFetch.ClientState (FetchRequest (..),
           PeerFetchInFlight (..), PeersOrder (..))
import Ouroboros.Network.BlockFetch.ConsensusInterface (ChainSelStarvation (..),
           FetchMode (..))
import Ouroboros.Network.BlockFetch.DeltaQ (calculatePeerFetchInFlightLimits)

import Cardano.Slotting.Slot (WithOrigin)
import Ouroboros.Network.BlockFetch.Decision
import Ouroboros.Network.BlockFetch.Decision.Trace (TraceDecisionEvent (..))


type WithDeclined peer = Writer (DList (FetchDecline, peer))

runWithDeclined :: WithDeclined peer a -> (a, DList (FetchDecline, peer))
runWithDeclined = runWriter

fetchDecisionsGenesisM
  :: forall peer header block m extra.
     (Ord peer,
      HasHeader header,
      HeaderHash header ~ HeaderHash block, MonadMonotonicTime m)
  => Tracer m (TraceDecisionEvent peer header)
  -> FetchDecisionPolicy header
  -> AnchoredFragment header
  -> (Point block -> Bool)
     -- ^ Whether the block has been fetched (only if recent, i.e. within @k@).
  -> MaxSlotNo
  -> ChainSelStarvation
  -> ( PeersOrder peer
     , PeersOrder peer -> m ()
     , peer -> m ()
     )
  -> [(AnchoredFragment header, PeerInfo header peer extra)]
  -> m [(FetchDecision (FetchRequest header), PeerInfo header peer extra)]
fetchDecisionsGenesisM
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
            (Sequence.fromList candidatesAndPeers)
            peersOrder1

    -- Compute the actual block fetch decision. This contains only declines and
    -- at most one request. 'theDecision' is therefore a 'Maybe'.
    let (theDecision, declines) =
          fetchDecisionsGenesis
            fetchDecisionPolicy
            currentChain
            fetchedBlocks
            fetchedMaxSlotNo
            (toList orderedCandidatesAndPeers)

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
      alignPeersOrderWithActualPeers
        :: forall d.
           (d -> peer)
        -> Seq d
        -> PeersOrder peer
        -> (PeersOrder peer, Seq d)
      alignPeersOrderWithActualPeers
        peerOf
        actualPeers
        PeersOrder {peersOrderStart, peersOrderCurrent, peersOrderAll} =
          let peersOrderAll' :: Seq d
              peersOrderAll' =
                    foldr (\p ds ->
                            case find ((p ==) . peerOf) actualPeers of
                              Just d  -> d <| ds
                              Nothing -> ds
                          )
                          Sequence.empty
                          peersOrderAll
                 >< Sequence.filter ((`notElem` peersOrderAll) . peerOf) actualPeers
              -- Set the current peer to Nothing if it is not at the front of
              -- the list.
              peersOrderCurrent' :: Maybe peer
              peersOrderCurrent' = do
                peer <- peersOrderCurrent
                guard $ case peersOrderAll' of
                  d Sequence.:<| _ -> peerOf d == peer
                  Sequence.Empty   -> False
                pure peer
           in (PeersOrder
                { peersOrderCurrent = peersOrderCurrent',
                  -- INVARIANT met: Current peer is at the front if it exists
                  peersOrderAll = fmap peerOf peersOrderAll',
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
            ChainSelStarvationOngoing      -> getMonotonicTime
          case peersOrderCurrent of
            Just peer
              | lastStarvationTime >= addTime bulkSyncGracePeriod peersOrderStart -> do
                  traceWith tracer (PeerStarvedUs peer)
                  demoteCSJDynamo peer
                  pure PeersOrder
                         {
                           peersOrderCurrent = Nothing,
                           -- INVARIANT met: there is no current peer
                           peersOrderAll = Sequence.drop 1 peersOrderAll |> peer,
                           peersOrderStart
                         }
            _ -> pure peersOrder

      setCurrentPeer :: Maybe peer -> PeersOrder peer -> PeersOrder peer
      setCurrentPeer Nothing peersOrder = peersOrder {peersOrderCurrent = Nothing}
      setCurrentPeer (Just peer) peersOrder =
        case Sequence.breakl (peer ==) (peersOrderAll peersOrder) of
          (xs, p :<| ys) ->
            peersOrder
              { peersOrderCurrent = Just p,
                -- INVARIANT met: Current peer is at the front
                peersOrderAll = p <| xs >< ys
              }
          (_, Empty) -> peersOrder {peersOrderCurrent = Nothing}

-- | Given a list of candidate fragments and their associated peers, choose what
-- to sync from who in the bulk sync mode.
fetchDecisionsGenesis
  :: forall header block peer extra.
     ( HasHeader header
     , HeaderHash header ~ HeaderHash block
     )
  => FetchDecisionPolicy header
  -> AnchoredFragment header
     -- ^ The current chain, anchored at the immutable tip.
  -> (Point block -> Bool)
     -- ^ Whether the block has been fetched (only if recent, i.e. within @k@).
  -> MaxSlotNo
  -> [(AnchoredFragment header, PeerInfo header peer extra)]
     -- ^ Association list of the candidate fragments and their associated peers.
     -- The candidate fragments are anchored in the current chain (not necessarily
     -- at the tip; and not necessarily forking off immediately).
  -> ( Maybe (FetchRequest header, PeerInfo header peer extra),
       [(FetchDecline, PeerInfo header peer extra)]
     )
     -- ^ Association list of the requests and their associated peers. There is at
     -- most one accepted request; everything else is declined. Morally, this is a
     -- map from peers to @'FetchDecision' ('FetchRequest' header)@ with at most
     -- one @'FetchRequest' header@.
fetchDecisionsGenesis
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
      combineWithDeclined
        :: forall peerInfo a.
           MaybeT (WithDeclined peerInfo) (a, peerInfo)
        -> ( Maybe (a, peerInfo),
             [(FetchDecline, peerInfo)]
           )
      combineWithDeclined = second DList.toList . runWithDeclined . runMaybeT

      dropAlreadyFetchedBlocks
        :: forall peerInfo.
           [(ChainSuffix header, peerInfo)]
        -> ChainSuffix header
        -> WithDeclined peerInfo (Maybe (CandidateFragments header))
      dropAlreadyFetchedBlocks candidatesAndPeers' theCandidate =
        case dropAlreadyFetched fetchedBlocks fetchedMaxSlotNo theCandidate of
          Left reason -> do
            tell (DList.fromList [(reason, peerInfo) | (_, peerInfo) <- candidatesAndPeers'])
            pure Nothing
          Right theFragments -> pure (Just theFragments)

-- | Find the fragments of the chain suffix that we still need to fetch because
-- they are covering blocks that have not yet been fetched.
--
-- Typically this is a single fragment forming a suffix of the chain, but in
-- the general case we can get a bunch of discontiguous chain fragments.
--
-- See also 'dropAlreadyInFlightWithPeer'.
-- Similar to 'filterNotAlreadyFetched'.
dropAlreadyFetched
  :: (HasHeader header, HeaderHash header ~ HeaderHash block)
  => (Point block -> Bool)
     -- ^ Whether the block has been fetched (only if recent, i.e. within @k@).
  -> MaxSlotNo
  -> ChainSuffix header
  -> FetchDecision (CandidateFragments header)
dropAlreadyFetched alreadyDownloaded fetchedMaxSlotNo candidate =
  if null fragments
    then Left FetchDeclineAlreadyFetched
    else Right (candidate, fragments)
  where
    fragments = filterWithMaxSlotNo notAlreadyFetched fetchedMaxSlotNo (getChainSuffix candidate)
    notAlreadyFetched = not . alreadyDownloaded . castPoint . blockPoint

-- | Given a list of candidate fragments and their associated peers, select the
-- candidate to sync from. Return this fragment, the list of peers that are
-- still in race to serve it, and the list of peers that are already being
-- declined.
selectTheCandidate
  :: forall header peerInfo.
     HasHeader header
  => FetchDecisionPolicy header
  -> AnchoredFragment header
     -- ^ The current chain.
  -> [(AnchoredFragment header, peerInfo)]
     -- ^ The candidate fragments and their associated peers.
  -> WithDeclined
       peerInfo
       (Maybe (ChainSuffix header, [(ChainSuffix header, peerInfo)]))
     -- ^ The pair of: (a) a list of peers that we have decided are not right,
     -- eg. because they presented us with a chain forking too deep, and (b) the
     -- selected candidate that we choose to sync from and a list of peers that
     -- are still in the race to serve that candidate.
selectTheCandidate
  FetchDecisionPolicy {compareCandidateChains, plausibleCandidateChain}
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
      separateDeclinedAndStillInRace
        :: [(FetchDecision (ChainSuffix header), peerInfo)]
        -> WithDeclined peerInfo (Maybe (ChainSuffix header, [(ChainSuffix header, peerInfo)]))
      separateDeclinedAndStillInRace decisions = do
        let (declined, inRace) = partitionEithers
              [ bimap (,p) (,p) d | (d, p) <- decisions ]
        tell (DList.fromList declined)
        case inRace of
          [] -> pure Nothing
          _ : _ -> do
            let maxChainOn f c0 c1 = case compareCandidateChains (f c0) (f c1) of
                  LT -> c1
                  _  -> c0
                -- maximumBy yields the last element in case of a tie while we
                -- prefer the first one
                chainSfx = fst $
                  List.foldl1' (maxChainOn (getChainSuffix . fst)) inRace
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
selectThePeer
  :: forall header peer extra.
     HasHeader header
  => CandidateFragments header
  -> [(ChainSuffix header, PeerInfo header peer extra)]
     -- ^ The candidate fragment that we have selected to sync from, as suffix
     -- of the immutable tip.
  -> WithDeclined
       (PeerInfo header peer extra)
       (Maybe (ChainSuffix header, PeerInfo header peer extra))
     -- ^ Association list of candidate fragments (as suffixes of the immutable
     -- tip) and their associated peers.
selectThePeer
  theFragments
  candidates = do
    -- Create a fetch request for the blocks in question. The request has exactly
    -- 1 block. It will only be used to choose the peer to fetch from, but we will
    -- later craft a more refined request for that peer. See [About the gross
    -- request] in the module documentation. Because @theFragments@ is not
    -- empty, and does not contain empty fragments, @grossRequest@ will not be empty.
    let firstBlock :: [AF.AnchoredSeq (WithOrigin SlotNo) (AF.Anchor header) header]
                   -> [AF.AnchoredSeq (WithOrigin SlotNo) (AF.Anchor header) header]
        firstBlock = map (AF.takeOldest 1) . take 1 . filter (not . AF.null)

        fetchRequestFragments :: [AF.AnchoredSeq (WithOrigin SlotNo) (AF.Anchor header) header]
        fetchRequestFragments = firstBlock $ snd theFragments

        grossRequest :: FetchRequest header
        grossRequest = assert (all (not . AF.null) fetchRequestFragments)
                     $ FetchRequest { fetchRequestFragments }

    -- Return the first peer that can serve the gross request and decline
    -- the other peers.
    go grossRequest candidates
  where
    go grossRequest (c@(candidate, peerInfo) : xs) = do
      if grossRequest `requestHeadInCandidate` candidate then do
        tell $ DList.fromList
          [(FetchDeclineConcurrencyLimit FetchModeGenesis 1, pInfo)
          | (_, pInfo) <- xs
          ]
        pure (Just c)
      else do
        tell $ DList.fromList [(FetchDeclineAlreadyFetched, peerInfo)]
        go grossRequest xs
    go _grossRequest [] = pure Nothing


    requestHeadInCandidate :: FetchRequest header -> ChainSuffix header -> Bool
    requestHeadInCandidate request candidate =
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
makeFetchRequest
  :: HasHeader header
  => FetchDecisionPolicy header
  -> CandidateFragments header
     -- ^ The candidate fragment that we have selected to sync from, as suffix of
     -- the immutable tip.
  -> PeerInfo header peer extra
     -- ^ The peer that we have selected to sync from.
  -> ChainSuffix header
     -- ^ Its candidate fragment as suffix of the immutable tip.
  -> WithDeclined
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
          trimmedFragments <- snd fragments `trimFragmentsToCandidate` thePeerCandidate

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
          Left reason -> tell (DList.fromList [(reason, thePeer)]) >> pure Nothing
          Right theRequest -> pure $ Just (theRequest, thePeer)
    where
      trimFragmentsToCandidate fragments candidate =
        let trimmedFragments =
              [ prefix
              | fragment <- fragments
              , Just (_, prefix, _, _) <- [AF.intersect (getChainSuffix candidate) fragment]
              , not (AF.null prefix)
              ]
         in if null trimmedFragments
              then Left FetchDeclineAlreadyFetched
              else Right trimmedFragments

-- | Find the fragments of the chain suffix that we still need to fetch because
-- they are covering blocks that are not currently in the process of being
-- fetched from this peer.
--
-- Typically this is a single fragment forming a suffix of the chain, but in
-- the general case we can get a bunch of discontiguous chain fragments.
--
-- See also 'dropAlreadyFetched'
-- Similar to 'filterNotAlreadyInFlightWithPeer'.
dropAlreadyInFlightWithPeer ::
  (HasHeader header) =>
  PeerFetchInFlight header ->
  CandidateFragments header ->
  FetchDecision (CandidateFragments header)
dropAlreadyInFlightWithPeer inflight (candidate, chainfragments) =
  if null fragments
    then Left FetchDeclineInFlightThisPeer
    else Right (candidate, fragments)
  where
    fragments = concatMap (filterWithMaxSlotNo notAlreadyInFlight (peerFetchMaxSlotNo inflight)) chainfragments
    notAlreadyInFlight b = blockPoint b `Set.notMember` peerFetchBlocksInFlight inflight
