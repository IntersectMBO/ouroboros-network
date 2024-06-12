{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module contains the part of the block fetch decisions process that is
-- common to both the bulk sync and deadline modes.
module Ouroboros.Network.BlockFetch.Decision.Common (
    FetchDecisionPolicy (..)
  , PeerInfo
  , FetchDecision
  , FetchDecline (..)
  , ChainSuffix (..)
  , filterNotAlreadyFetched
  , filterNotAlreadyInFlightWithPeer
  , (?!)
  , CandidateFragments
  , filterWithMaxSlotNo
  , filterPlausibleCandidates
  , selectForkSuffixes
  , filterNotAlreadyInFlightWithPeer'
  , filterNotAlreadyFetched'
  , fetchRequestDecision
) where

import GHC.Stack (HasCallStack)
import Control.Exception (assert)
import Control.Monad (guard)
import Control.Monad.Class.MonadTime.SI (DiffTime)
import qualified Data.Set as Set

import Ouroboros.Network.AnchoredFragment (AnchoredFragment, AnchoredSeq (..))
import qualified Ouroboros.Network.AnchoredFragment as AF
import Ouroboros.Network.Block (HasHeader, HeaderHash, Point, MaxSlotNo (..), castPoint, blockPoint, blockSlot)
import Ouroboros.Network.BlockFetch.ClientState (PeerFetchInFlight (..), PeerFetchStatus (..), FetchRequest (..))
import Ouroboros.Network.BlockFetch.ConsensusInterface (FetchMode (..))
import Ouroboros.Network.DeltaQ ( PeerGSV )
import Ouroboros.Network.SizeInBytes ( SizeInBytes )
import Ouroboros.Network.BlockFetch.DeltaQ (PeerFetchInFlightLimits (..))

data FetchDecisionPolicy header = FetchDecisionPolicy {
       maxInFlightReqsPerPeer  :: Word,  -- A protocol constant.

       maxConcurrencyBulkSync  :: Word,
       maxConcurrencyDeadline  :: Word,
       decisionLoopInterval    :: DiffTime,
       peerSalt                :: Int,

       plausibleCandidateChain :: HasCallStack
                               => AnchoredFragment header
                               -> AnchoredFragment header -> Bool,

       compareCandidateChains  :: HasCallStack
                               => AnchoredFragment header
                               -> AnchoredFragment header
                               -> Ordering,

       blockFetchSize          :: header -> SizeInBytes
     }

type PeerInfo header peer extra =
       ( PeerFetchStatus header,
         PeerFetchInFlight header,
         PeerGSV,
         peer,
         extra
       )

-- | Throughout the decision making process we accumulate reasons to decline
-- to fetch any blocks. This type is used to wrap intermediate and final
-- results.
--
type FetchDecision result = Either FetchDecline result

-- | All the various reasons we can decide not to fetch blocks from a peer.
--
-- It is worth highlighting which of these reasons result from competition
-- among upstream peers.
--
-- * 'FetchDeclineInFlightOtherPeer': decline this peer because all the
--   unfetched blocks of its candidate chain have already been requested from
--   other peers. This reason reflects the least-consequential competition
--   among peers: the competition that determines merely which upstream peer to
--   burden with the request (eg the one with the best
--   'Ouroboros.Network.BlockFetch.DeltaQ.DeltaQ' metrics). The consequences
--   are relatively minor because the unfetched blocks on this peer's candidate
--   chain will be requested regardless; it's merely a question of "From who?".
--   (One exception: if an adversarial peer wins this competition such that the
--   blocks are only requested from them, then it may be possible that this
--   decision determines whether the blocks are ever /received/. But that
--   depends on details of timeouts, a longer competing chain being soon
--   received within those timeouts, and so on.)
--
-- * 'FetchDeclineChainNotPlausible': decline this peer because the node has
--   already fetched, validated, and selected a chain better than its candidate
--   chain from other peers (or from the node's own block forge). Because the
--   node's current selection is influenced by what blocks other peers have
--   recently served (or it recently minted), this reason reflects that peers
--   /indirectly/ compete by serving as long of a chain as possible and as
--   promptly as possible. When the tips of the peers' selections are all
--   within their respective forecast horizons (see
--   'Ouroboros.Consensus.Ledger.SupportsProtocol.ledgerViewForecastAt'), then
--   the length of their candidate chains will typically be the length of their
--   selections, since the ChainSync is free to race ahead (in contrast, the
--   BlockFetch pipeline depth is bounded such that it will, for a syncing
--   node, not be able to request all blocks between the selection and the end
--   of the forecast window). But if one or more of their tips is beyond the
--   horizon, then the relative length of the candidate chains is more
--   complicated, influenced by both the relative density of the chains'
--   suffixes and the relative age of the chains' intersection with the node's
--   selection (since each peer's forecast horizon is a fixed number of slots
--   after the candidate's successor of that intersection).
--
-- * 'FetchDeclineConcurrencyLimit': decline this peer while the node has
--   already fully allocated the artificially scarce 'maxConcurrentFetchPeers'
--   resource amongst its other peers. This reason reflects the
--   least-fundamental competition: it's the only way a node would decline a
--   candidate chain C that it would immediately switch to if C had somehow
--   already been fetched (and any better current candidates hadn't). It is
--   possible that this peer's candidate fragment is better than the candidate
--   fragments of other peers, but that should only happen ephemerally (eg for
--   a brief while immediately after first connecting to this peer).
--
-- * 'FetchDeclineChainIntersectionTooDeep': decline this peer because the node's
--   selection has more than @K@ blocks that are not on this peer's candidate
--   chain. Typically, this reason occurs after the node has been declined---ie
--   lost the above competitions---for a long enough duration. This decision
--   only arises if the BlockFetch decision logic wins a harmless race against
--   the ChainSync client once the node's selection gets longer, since
--   'Ouroboros.Consensus.MiniProtocol.ChainSync.Client.ForkTooDeep'
--   disconnects from such a peer.
--
data FetchDecline =
     -- | This peer's candidate chain is not longer than our chain. For more
     -- details see
     -- 'Ouroboros.Consensus.MiniProtocol.BlockFetch.ClientInterface.mkBlockFetchConsensusInterface'
     -- which implements 'plausibleCandidateChain'.
     --
     FetchDeclineChainNotPlausible

     -- | Switching to this peer's candidate chain would require rolling back
     -- more than @K@ blocks.
     --
   | FetchDeclineChainIntersectionTooDeep

     -- | Every block on this peer's candidate chain has already been fetched.
     --
   | FetchDeclineAlreadyFetched

     -- | This peer's candidate chain has already been requested from this
     -- peer.
     --
   | FetchDeclineInFlightThisPeer

     -- | Some blocks on this peer's candidate chain have not yet been fetched,
     -- but all of those have already been requested from other peers.
     --
   | FetchDeclineInFlightOtherPeer

     -- | This peer's BlockFetch client is shutting down, see
     -- 'PeerFetchStatusShutdown'.
     --
   | FetchDeclinePeerShutdown

     -- | Blockfetch is starting up and waiting on corresponding Chainsync.
   | FetchDeclinePeerStarting


   -- The reasons above this comment are fundamental and/or obvious. On the
   -- other hand, the reasons below are heuristic.


     -- | This peer is in a potentially-temporary state in which it has not
     -- responded to us within a certain expected time limit, see
     -- 'PeerFetchStatusAberrant'.
     --
   | FetchDeclinePeerSlow

     -- | This peer is not under the 'maxInFlightReqsPerPeer' limit.
     --
     -- The argument is the 'maxInFlightReqsPerPeer' constant.
     --
   | FetchDeclineReqsInFlightLimit  !Word

     -- | This peer is not under the 'inFlightBytesHighWatermark' bytes limit.
     --
     -- The arguments are:
     --
     -- * number of bytes currently in flight for that peer
     -- * the configured 'inFlightBytesLowWatermark' constant
     -- * the configured 'inFlightBytesHighWatermark' constant
     --
   | FetchDeclineBytesInFlightLimit !SizeInBytes !SizeInBytes !SizeInBytes

     -- | This peer is not under the 'inFlightBytesLowWatermark'.
     --
     -- The arguments are:
     --
     -- * number of bytes currently in flight for that peer
     -- * the configured 'inFlightBytesLowWatermark' constant
     -- * the configured 'inFlightBytesHighWatermark' constant
     --
   | FetchDeclinePeerBusy           !SizeInBytes !SizeInBytes !SizeInBytes

     -- | The node is not under the 'maxConcurrentFetchPeers' limit.
     --
     -- The arguments are:
     --
     -- * the current 'FetchMode'
     -- * the corresponding configured limit constant, either
     --   'maxConcurrencyBulkSync' or 'maxConcurrencyDeadline'
     --
   | FetchDeclineConcurrencyLimit   !FetchMode !Word
  deriving (Eq, Show)

-- | The combination of a 'ChainSuffix' and a list of discontiguous
-- 'AnchoredFragment's:
--
-- * When comparing two 'CandidateFragments' as candidate chains, we use the
--   'ChainSuffix'.
--
-- * To track which blocks of that candidate still have to be downloaded, we
--   use a list of discontiguous 'AnchoredFragment's.
--
type CandidateFragments header = (ChainSuffix header, [AnchoredFragment header])

{-
Of course we would at most need to download the blocks in a candidate chain
that are not already in the current chain. So we must find those intersections.

Before we do that, lets define how we represent a suffix of a chain. We do this
very simply as a chain fragment: exactly those blocks contained in the suffix.
A chain fragment is of course not a chain, but has many similar invariants.

We will later also need to represent chain ranges when we send block fetch
requests. We do this using a pair of points: the first and last blocks in the
range.  While we can represent an empty chain fragment, we cannot represent an
empty fetch range, but this is ok since we never request empty ranges.

 Chain fragment
    ┌───┐
    │ ◉ │ Start of range, inclusive
    ├───┤
    │   │
    ├───┤
    │   │
    ├───┤
    │   │
    ├───┤
    │ ◉ │ End of range, inclusive.
    └───┘
-}

-- | A chain suffix, obtained by intersecting a candidate chain with the
-- current chain.
--
-- The anchor point of a 'ChainSuffix' will be a point within the bounds of
-- the current chain ('AF.withinFragmentBounds'), indicating that it forks off
-- in the last @K@ blocks.
--
-- A 'ChainSuffix' must be non-empty, as an empty suffix, i.e. the candidate
-- chain is equal to the current chain, would not be a plausible candidate.
newtype ChainSuffix header =
    ChainSuffix { getChainSuffix :: AnchoredFragment header }

{-
We have the node's /current/ or /adopted/ chain. This is the node's chain in
the sense specified by the Ouroboros algorithm. It is a fully verified chain
with block bodies and a ledger state.

    ┆   ┆
    ├───┤
    │   │
    ├───┤
    │   │
    ├───┤
    │   │
    ├───┤
    │   │
 ───┴───┴─── current chain length (block number)

With chain selection we are interested in /candidate/ chains. We have these
candidate chains in the form of chains of verified headers, but without bodies.

The consensus layer gives us the current set of candidate chains from our peers
and we have the task of selecting which block bodies to download, and then
passing those block bodes back to the consensus layer. The consensus layer will
try to validate them and decide if it wants to update its current chain.

    ┆   ┆     ┆   ┆     ┆   ┆     ┆   ┆     ┆   ┆
    ├───┤     ├───┤     ├───┤     ├───┤     ├───┤
    │   │     │   │     │   │     │   │     │   │
    ├───┤     ├───┤     ├───┤     ├───┤     ├───┤
    │   │     │   │     │   │     │   │     │   │
    ├───┤     ├───┤     ├───┤     ├───┤     ├───┤
    │   │     │   │     │   │     │   │     │   │
    ├───┤     ├───┤     ├───┤     ├───┤     └───┘
    │   │     │   │     │   │     │   │
 ───┴───┴─────┼───┼─────┼───┼─────┼───┼───────────── current chain length
              │   │     │   │     │   │
  current     ├───┤     ├───┤     └───┘
  (blocks)    │   │     │   │
              └───┘     └───┘
                A         B         C         D
             candidates
             (headers)

In this example we have four candidate chains, with all but chain D strictly
longer than our current chain.

In general there are many candidate chains. We make a distinction between a
candidate chain and the peer from which it is available. It is often the
case that the same chain is available from multiple peers. We will try to be
clear about when we are referring to chains or the combination of a chain and
the peer from which it is available.

For the sake of the example let us assume we have the four chains above
available from the following peers.

peer    1         2         3         4         5         6         7
      ┆   ┆     ┆   ┆     ┆   ┆     ┆   ┆     ┆   ┆     ┆   ┆     ┆   ┆
      ├───┤     ├───┤     ├───┤     ├───┤     ├───┤     ├───┤     ├───┤
      │   │     │   │     │   │     │   │     │   │     │   │     │   │
      ├───┤     ├───┤     ├───┤     ├───┤     └───┘     ├───┤     ├───┤
      │   │     │   │     │   │     │   │               │   │     │   │
    ──┼───┼─────┼───┼─────┼───┼─────┼───┼───────────────┼───┼─────┼───┼──
      │   │     │   │     │   │     │   │               │   │     │   │
      └───┘     ├───┤     ├───┤     ├───┤               ├───┤     ├───┤
                │   │     │   │     │   │               │   │     │   │
                └───┘     └───┘     └───┘               └───┘     └───┘
chain   C         A         B         A         D         B         A

This is the form in which we are informed about candidate chains from the
consensus layer, the combination of a chain and the peer it is from. This
makes sense, since these things change independently.

We will process the chains in this form, keeping the peer/chain combination all
the way through. Although there could in principle be some opportunistic saving
by sharing when multiple peers provide the same chain, taking advantage of this
adds complexity and does nothing to improve our worst case costs.

We are only interested in candidate chains that are strictly longer than our
current chain. So our first task is to filter down to this set.
-}

-- | Keep only those candidate chains that are preferred over the current
-- chain. Typically, this means that their length is longer than the length of
-- the current chain.
--
filterPlausibleCandidates
  :: (AnchoredFragment block -> AnchoredFragment header -> Bool)
  -> AnchoredFragment block  -- ^ The current chain
  -> [(AnchoredFragment header, peerinfo)]
  -> [(FetchDecision (AnchoredFragment header), peerinfo)]
filterPlausibleCandidates plausibleCandidateChain currentChain chains =
    [ (chain', peer)
    | (chain,  peer) <- chains
    , let chain' = do
            guard (plausibleCandidateChain currentChain chain)
              ?! FetchDeclineChainNotPlausible
            return chain
    ]

{-
We define the /fetch range/ as the suffix of the fork range that has not yet
had its blocks downloaded and block content checked against the headers.

    ┆   ┆
    ├───┤
    │   │
    ├───┤               ┌───┐
    │   │    already    │   │
    ├───┤    fetched    ├───┤
    │   │    blocks     │   │
    ├───┤               ├───┤
    │   │               │░◉░│  ◄  fetch range
 ───┴───┴─────┬───┬─────┼───┼───
              │░◉░│ ◄   │░░░│
              └───┘     ├───┤
                        │░◉░│  ◄
                        └───┘

In earlier versions of this scheme we maintained and relied on the invariant
that the ranges of fetched blocks are backwards closed. This meant we never had
discontinuous ranges of fetched or not-yet-fetched blocks. This invariant does
simplify things somewhat by keeping the ranges continuous however it precludes
fetching ranges of blocks from different peers in parallel.

We do not maintain any such invariant and so we have to deal with there being
gaps in the ranges we have already fetched or are yet to fetch. To keep the
tracking simple we do not track the ranges themselves, rather we track the set
of individual blocks without their relationship to each other.

-}

-- | Find the fragments of the chain suffix that we still need to fetch, these
-- are the fragments covering blocks that have not yet been fetched and are
-- not currently in the process of being fetched from this peer.
--
-- Typically this is a single fragment forming a suffix of the chain, but in
-- the general case we can get a bunch of discontiguous chain fragments.
filterNotAlreadyFetched ::
  (HasHeader header, HeaderHash header ~ HeaderHash block) =>
  (Point block -> Bool) ->
  MaxSlotNo ->
  ChainSuffix header ->
  FetchDecision (CandidateFragments header)
filterNotAlreadyFetched alreadyDownloaded fetchedMaxSlotNo candidate =
  if null fragments
    then Left FetchDeclineAlreadyFetched
    else Right (candidate, fragments)
  where
    fragments = filterWithMaxSlotNo notAlreadyFetched fetchedMaxSlotNo (getChainSuffix candidate)
    notAlreadyFetched = not . alreadyDownloaded . castPoint . blockPoint

filterNotAlreadyFetched' ::
  (HasHeader header, HeaderHash header ~ HeaderHash block) =>
  (Point block -> Bool) ->
  MaxSlotNo ->
  [(FetchDecision (ChainSuffix header), peerinfo)] ->
  [(FetchDecision (CandidateFragments header), peerinfo)]
filterNotAlreadyFetched' alreadyDownloaded fetchedMaxSlotNo =
  map
    ( \(mcandidate, peer) ->
        ((filterNotAlreadyFetched alreadyDownloaded fetchedMaxSlotNo =<< mcandidate), peer)
    )

filterNotAlreadyInFlightWithPeer ::
  (HasHeader header) =>
  PeerFetchInFlight header ->
  CandidateFragments header ->
  FetchDecision (CandidateFragments header)
filterNotAlreadyInFlightWithPeer inflight (candidate, chainfragments) =
  if null fragments
    then Left FetchDeclineInFlightThisPeer
    else Right (candidate, fragments)
  where
    fragments = concatMap (filterWithMaxSlotNo notAlreadyInFlight (peerFetchMaxSlotNo inflight)) chainfragments
    notAlreadyInFlight b = blockPoint b `Set.notMember` peerFetchBlocksInFlight inflight

filterNotAlreadyInFlightWithPeer' ::
  (HasHeader header) =>
  [(FetchDecision (CandidateFragments header), PeerFetchInFlight header, peerinfo)] ->
  [(FetchDecision (CandidateFragments header), peerinfo)]
filterNotAlreadyInFlightWithPeer' =
  map
    ( \(mcandidatefragments, inflight, peer) ->
        ((filterNotAlreadyInFlightWithPeer inflight =<< mcandidatefragments), peer)
    )

-- | The \"oh noes?!\" operator.
--
-- In the case of an error, the operator provides a specific error value.
--
(?!) :: Maybe a -> e -> Either e a
Just x  ?! _ = Right x
Nothing ?! e = Left  e

-- | Filter a fragment. This is an optimised variant that will behave the same
-- as 'AnchoredFragment.filter' if the following precondition is satisfied:
--
-- PRECONDITION: for all @hdr@ in the chain fragment: if @blockSlot hdr >
-- maxSlotNo@ then the predicate should not hold for any header after @hdr@ in
-- the chain fragment.
--
-- For example, when filtering out already downloaded blocks from the
-- fragment, it does not make sense to keep filtering after having encountered
-- the highest slot number the ChainDB has seen so far: blocks with a greater
-- slot number cannot have been downloaded yet. When the candidate fragments
-- get far ahead of the current chain, e.g., @2k@ headers, this optimisation
-- avoids the linear cost of filtering these headers when we know in advance
-- they will all remain in the final fragment. In case the given slot number
-- is 'NoSlotNo', no filtering takes place, as there should be no matches
-- because we haven't downloaded any blocks yet.
--
-- For example, when filtering out blocks already in-flight for the given
-- peer, the given @maxSlotNo@ can correspond to the block with the highest
-- slot number that so far has been in-flight for the given peer. When no
-- blocks have been in-flight yet, @maxSlotNo@ can be 'NoSlotNo', in which
-- case no filtering needs to take place, which makes sense, as there are no
-- blocks to filter out. Note that this is conservative: if a block is for
-- some reason multiple times in-flight (maybe it has to be redownloaded) and
-- the block's slot number matches the @maxSlotNo@, it will now be filtered
-- (while the filtering might previously have stopped before encountering the
-- block in question). This is fine, as the filter will now include the block,
-- because according to the filtering predicate, the block is not in-flight.
filterWithMaxSlotNo
  :: forall header. HasHeader header
  => (header -> Bool)
  -> MaxSlotNo  -- ^ @maxSlotNo@
  -> AnchoredFragment header
  -> [AnchoredFragment header]
filterWithMaxSlotNo p maxSlotNo =
    AF.filterWithStop p ((> maxSlotNo) . MaxSlotNo . blockSlot)

-- | Find the chain suffix for a candidate chain, with respect to the
-- current chain.
--
chainForkSuffix
  :: (HasHeader header, HasHeader block,
      HeaderHash header ~ HeaderHash block)
  => AnchoredFragment block  -- ^ Current chain.
  -> AnchoredFragment header -- ^ Candidate chain
  -> Maybe (ChainSuffix header)
chainForkSuffix current candidate =
    case AF.intersect current candidate of
      Nothing                         -> Nothing
      Just (_, _, _, candidateSuffix) ->
        -- If the suffix is empty, it means the candidate chain was equal to
        -- the current chain and didn't fork off. Such a candidate chain is
        -- not a plausible candidate, so it must have been filtered out.
        assert (not (AF.null candidateSuffix)) $
        Just (ChainSuffix candidateSuffix)

selectForkSuffixes
  :: (HasHeader header, HasHeader block,
      HeaderHash header ~ HeaderHash block)
  => AnchoredFragment block
  -> [(FetchDecision (AnchoredFragment header), peerinfo)]
  -> [(FetchDecision (ChainSuffix      header), peerinfo)]
selectForkSuffixes current chains =
    [ (mchain', peer)
    | (mchain,  peer) <- chains
    , let mchain' = do
            chain <- mchain
            chainForkSuffix current chain ?! FetchDeclineChainIntersectionTooDeep
    ]

-- |
--
-- This function _does not_ check if the peer is likely to have the blocks in
-- the ranges, it only compute a request that respect what the peer's current
-- status indicates on their ability to fulfill it.
fetchRequestDecision
  :: HasHeader header
  => FetchDecisionPolicy header
  -> FetchMode
  -> Word
     -- ^ Number of concurrent fetch peers. Can be set to @0@ to bypass
     -- concurrency limits.
  -> PeerFetchInFlightLimits
  -> PeerFetchInFlight header
  -> PeerFetchStatus header
  -> FetchDecision [AnchoredFragment header]
  -> FetchDecision (FetchRequest  header)

fetchRequestDecision _ _ _ _ _ _ (Left decline)
  = Left decline

fetchRequestDecision _ _ _ _ _ PeerFetchStatusShutdown _
  = Left FetchDeclinePeerShutdown

fetchRequestDecision _ _ _ _ _ PeerFetchStatusStarting _
  = Left FetchDeclinePeerStarting

fetchRequestDecision _ _ _ _ _ PeerFetchStatusAberrant _
  = Left FetchDeclinePeerSlow

fetchRequestDecision FetchDecisionPolicy {
                       maxConcurrencyDeadline,
                       maxInFlightReqsPerPeer,
                       blockFetchSize
                     }
                     fetchMode
                     nConcurrentFetchPeers
                     PeerFetchInFlightLimits {
                       inFlightBytesLowWatermark,
                       inFlightBytesHighWatermark
                     }
                     PeerFetchInFlight {
                       peerFetchReqsInFlight,
                       peerFetchBytesInFlight
                     }
                     peerFetchStatus
                     (Right fetchFragments)

  | peerFetchReqsInFlight >= maxInFlightReqsPerPeer
  = Left $ FetchDeclineReqsInFlightLimit
             maxInFlightReqsPerPeer

  | peerFetchBytesInFlight >= inFlightBytesHighWatermark
  = Left $ FetchDeclineBytesInFlightLimit -- FIXME: this one should be maybe not too bad.
             peerFetchBytesInFlight
             inFlightBytesLowWatermark
             inFlightBytesHighWatermark

    -- This covers the case when we could still fit in more reqs or bytes, but
    -- we want to let it drop below a low water mark before sending more so we
    -- get a bit more batching behaviour, rather than lots of 1-block reqs.
  | peerFetchStatus == PeerFetchStatusBusy
  = Left $ FetchDeclinePeerBusy -- FIXME: also not too bad
             peerFetchBytesInFlight
             inFlightBytesLowWatermark
             inFlightBytesHighWatermark

    -- Refuse any blockrequest if we're above the concurrency limit.
  | let maxConcurrentFetchPeers = case fetchMode of
                                    FetchModeBulkSync -> 1 -- FIXME: maxConcurrencyBulkSync has to be removed from the interface
                                    FetchModeDeadline -> maxConcurrencyDeadline
  , nConcurrentFetchPeers > maxConcurrentFetchPeers
  = Left $ FetchDeclineConcurrencyLimit
             fetchMode maxConcurrentFetchPeers

    -- If we're at the concurrency limit refuse any additional peers.
  | peerFetchReqsInFlight == 0
  , let maxConcurrentFetchPeers = case fetchMode of
                                    FetchModeBulkSync -> 1 -- FIXME: maxConcurrencyBulkSync has to be removed from the interface
                                    FetchModeDeadline -> maxConcurrencyDeadline
  , nConcurrentFetchPeers == maxConcurrentFetchPeers
  = Left $ FetchDeclineConcurrencyLimit
             fetchMode maxConcurrentFetchPeers

    -- We've checked our request limit and our byte limit. We are then
    -- guaranteed to get at least one non-empty request range.
  | otherwise
  = assert (peerFetchReqsInFlight < maxInFlightReqsPerPeer) $
    assert (not (null fetchFragments)) $

    Right $ selectBlocksUpToLimits
              blockFetchSize
              peerFetchReqsInFlight
              maxInFlightReqsPerPeer
              peerFetchBytesInFlight
              inFlightBytesHighWatermark
              fetchFragments

-- |
--
-- Precondition: The result will be non-empty if
--
-- Property: result is non-empty if preconditions satisfied
--
selectBlocksUpToLimits
  :: forall header. HasHeader header
  => (header -> SizeInBytes) -- ^ Block body size
  -> Word -- ^ Current number of requests in flight
  -> Word -- ^ Maximum number of requests in flight allowed
  -> SizeInBytes -- ^ Current number of bytes in flight
  -> SizeInBytes -- ^ Maximum number of bytes in flight allowed
  -> [AnchoredFragment header]
  -> FetchRequest header
selectBlocksUpToLimits blockFetchSize nreqs0 maxreqs nbytes0 maxbytes fragments =
    assert (nreqs0 < maxreqs && nbytes0 < maxbytes && not (null fragments)) $
    -- The case that we are already over our limits has to be checked earlier,
    -- outside of this function. From here on however we check for limits.

    let fragments' = goFrags nreqs0 nbytes0 fragments in
    assert (all (not . AF.null) fragments') $
    FetchRequest fragments'
  where
    goFrags :: Word
            -> SizeInBytes
            -> [AnchoredFragment header] -> [AnchoredFragment header]
    goFrags _     _      []     = []
    goFrags nreqs nbytes (c:cs)
      | nreqs+1  > maxreqs      = []
      | otherwise               = goFrag (nreqs+1) nbytes (Empty (AF.anchor c)) c cs
      -- Each time we have to pick from a new discontiguous chain fragment then
      -- that will become a new request, which contributes to our in-flight
      -- request count. We never break the maxreqs limit.

    goFrag :: Word
           -> SizeInBytes
           -> AnchoredFragment header
           -> AnchoredFragment header
           -> [AnchoredFragment header] -> [AnchoredFragment header]
    goFrag nreqs nbytes c' (Empty _) cs = c' : goFrags nreqs nbytes cs
    goFrag nreqs nbytes c' (b :< c)  cs
      | nbytes' >= maxbytes             = [c' :> b]
      | otherwise                       = goFrag nreqs nbytes' (c' :> b) c cs
      where
        nbytes' = nbytes + blockFetchSize b
      -- Note that we always pick the one last block that crosses the maxbytes
      -- limit. This cover the case where we otherwise wouldn't even be able to
      -- request a single block, as it's too large.
