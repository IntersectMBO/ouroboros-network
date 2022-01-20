{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

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
  , filterNotAlreadyInFlightWithOtherPeers
  , fetchRequestDecisions
  ) where

import qualified Data.Set as Set

import           Data.Function (on)
import           Data.Hashable
import           Data.List (foldl', groupBy, sortBy, transpose)
import           Data.Maybe (mapMaybe)
import           Data.Set (Set)
import           GHC.Stack (HasCallStack)

import           Control.Exception (assert)
import           Control.Monad (guard)
import           Control.Monad.Class.MonadTime (DiffTime)

import           Ouroboros.Network.AnchoredFragment (AnchoredFragment,
                     AnchoredSeq (..))
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block
import           Ouroboros.Network.Point (withOriginToMaybe)

import           Ouroboros.Network.BlockFetch.ClientState (FetchRequest (..),
                     PeerFetchInFlight (..), PeerFetchStatus (..))
import           Ouroboros.Network.BlockFetch.DeltaQ
                     (PeerFetchInFlightLimits (..), PeerGSV (..), SizeInBytes,
                     calculatePeerFetchInFlightLimits, comparePeerGSV,
                     comparePeerGSV',
                     estimateExpectedResponseDuration,
                     estimateResponseDeadlineProbability)


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


data FetchMode =
       -- | Use this mode when we are catching up on the chain but are stil
       -- well behind. In this mode the fetch logic will optimise for
       -- throughput rather than latency.
       --
       FetchModeBulkSync

       -- | Use this mode for block-producing nodes that have a known deadline
       -- to produce a block and need to get the best chain before that. In
       -- this mode the fetch logic will optimise for picking the best chain
       -- within the given deadline.
     | FetchModeDeadline

       -- TODO: add an additional mode for in-between: when we are a core node
       -- following the chain but do not have an imminent deadline, or are a
       -- relay forwarding chains within the network.
       --
       -- This is a mixed mode because we have to combine the distribution of
       -- time to next block under praos, with the distribution of latency of
       -- our peers, and also the consensus preference.

  deriving (Eq, Show)


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
data FetchDecline =
     FetchDeclineChainNotPlausible
   | FetchDeclineChainNoIntersection
   | FetchDeclineAlreadyFetched
   | FetchDeclineInFlightThisPeer
   | FetchDeclineInFlightOtherPeer
   | FetchDeclinePeerShutdown
   | FetchDeclinePeerSlow
   | FetchDeclineReqsInFlightLimit  !Word
   | FetchDeclineBytesInFlightLimit !SizeInBytes !SizeInBytes !SizeInBytes
   | FetchDeclinePeerBusy           !SizeInBytes !SizeInBytes !SizeInBytes
   | FetchDeclineConcurrencyLimit   !FetchMode !Word
  deriving (Eq, Show)


-- | The \"oh noes?!\" operator.
--
-- In the case of an error, the operator provides a specific error value.
--
(?!) :: Maybe a -> e -> Either e a
Just x  ?! _ = Right x
Nothing ?! e = Left  e

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


fetchDecisions
  :: (Ord peer,
      Hashable peer,
      HasHeader header,
      HeaderHash header ~ HeaderHash block)
  => FetchDecisionPolicy header
  -> FetchMode
  -> AnchoredFragment header
  -> (Point block -> Bool)
  -> MaxSlotNo
  -> [(AnchoredFragment header, PeerInfo header peer extra)]
  -> [(FetchDecision (FetchRequest header), PeerInfo header peer extra)]
fetchDecisions fetchDecisionPolicy@FetchDecisionPolicy {
                 plausibleCandidateChain,
                 compareCandidateChains,
                 blockFetchSize,
                 peerSalt
               }
               fetchMode
               currentChain
               fetchedBlocks
               fetchedMaxSlotNo =

    -- Finally, make a decision for each (chain, peer) pair.
    fetchRequestDecisions
      fetchDecisionPolicy
      fetchMode
  . map swizzleSIG

    -- Filter to keep blocks that are not already in-flight with other peers.
  . filterNotAlreadyInFlightWithOtherPeers
      fetchMode
  . map swizzleSI

    -- Reorder chains based on consensus policy and network timing data.
  . prioritisePeerChains
      fetchMode
      peerSalt
      compareCandidateChains
      blockFetchSize
  . map swizzleIG

    -- Filter to keep blocks that are not already in-flight for this peer.
  . filterNotAlreadyInFlightWithPeer
  . map swizzleI

    -- Filter to keep blocks that have not already been downloaded.
  . filterNotAlreadyFetched
      fetchedBlocks
      fetchedMaxSlotNo

    -- Select the suffix up to the intersection with the current chain.
  . selectForkSuffixes
      currentChain

    -- First, filter to keep chains the consensus layer tells us are plausible.
  . filterPlausibleCandidates
      plausibleCandidateChain
      currentChain
  where
    -- Data swizzling functions to get the right info into each stage.
    swizzleI   (c, p@(_,     inflight,_,_,      _)) = (c,         inflight,       p)
    swizzleIG  (c, p@(_,     inflight,gsvs,peer,_)) = (c,         inflight, gsvs, peer, p)
    swizzleSI  (c, p@(status,inflight,_,_,      _)) = (c, status, inflight,       p)
    swizzleSIG (c, p@(status,inflight,gsvs,peer,_)) = (c, status, inflight, gsvs, peer, p)

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
In the example, this leaves us with only the candidate chains: A, B and C, but
still paired up with the various peers.


peer    1         2         3         4                   6         7
      ┆   ┆     ┆   ┆     ┆   ┆     ┆   ┆               ┆   ┆     ┆   ┆
      ├───┤     ├───┤     ├───┤     ├───┤               ├───┤     ├───┤
      │   │     │   │     │   │     │   │               │   │     │   │
      ├───┤     ├───┤     ├───┤     ├───┤               ├───┤     ├───┤
      │   │     │   │     │   │     │   │               │   │     │   │
    ──┼───┼─────┼───┼─────┼───┼─────┼───┼───────────────┼───┼─────┼───┼──
      │   │     │   │     │   │     │   │               │   │     │   │
      └───┘     ├───┤     ├───┤     ├───┤               ├───┤     ├───┤
                │   │     │   │     │   │               │   │     │   │
                └───┘     └───┘     └───┘               └───┘     └───┘
chain   C         A         B         A                   B         A
-}


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
-- the currrent chain ('AF.withinFragmentBounds'), indicating that it forks off
-- in the last @K@ blocks.
--
-- A 'ChainSuffix' must be non-empty, as an empty suffix, i.e. the candidate
-- chain is equal to the current chain, would not be a plausible candidate.
newtype ChainSuffix header =
    ChainSuffix { getChainSuffix :: AnchoredFragment header }

{-
We define the /chain suffix/ as the suffix of the candidate chain up until (but
not including) where it intersects the current chain.


   current    peer 1    peer 2

    ┆   ┆
    ├───┤
    │  ◀┿━━━━━━━━━━━━━━━━━┓
    ├───┤               ┌─╂─┐
    │   │               │ ◉ │
    ├───┤               ├───┤
    │   │               │   │
    ├───┤               ├───┤
    │  ◀┿━━━━━━━┓       │   │
 ───┴───┴─────┬─╂─┬─────┼───┼───
              │ ◉ │     │   │
              └───┘     ├───┤
                        │ ◉ │
                        └───┘
                C         A

In this example we found that C was a strict extension of the current chain
and chain A was a short fork.

Note that it's possible that we don't find any intersection within the last K
blocks. This means the candidate forks by more than K and so we are not
interested in this candidate at all.
-}

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
            chainForkSuffix current chain ?! FetchDeclineChainNoIntersection
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
--
filterNotAlreadyFetched
  :: (HasHeader header, HeaderHash header ~ HeaderHash block)
  => (Point block -> Bool)
  -> MaxSlotNo
  -> [(FetchDecision (ChainSuffix        header), peerinfo)]
  -> [(FetchDecision (CandidateFragments header), peerinfo)]
filterNotAlreadyFetched alreadyDownloaded fetchedMaxSlotNo chains =
    [ (mcandidates, peer)
    | (mcandidate,  peer) <- chains
    , let mcandidates = do
            candidate <- mcandidate
            let fragments = filterWithMaxSlotNo
                              notAlreadyFetched
                              fetchedMaxSlotNo
                              (getChainSuffix candidate)
            guard (not (null fragments)) ?! FetchDeclineAlreadyFetched
            return (candidate, fragments)
    ]
  where
    notAlreadyFetched = not . alreadyDownloaded . castPoint . blockPoint


filterNotAlreadyInFlightWithPeer
  :: HasHeader header
  => [(FetchDecision (CandidateFragments header), PeerFetchInFlight header,
                                                  peerinfo)]
  -> [(FetchDecision (CandidateFragments header), peerinfo)]
filterNotAlreadyInFlightWithPeer chains =
    [ (mcandidatefragments',          peer)
    | (mcandidatefragments, inflight, peer) <- chains
    , let mcandidatefragments' = do
            (candidate, chainfragments) <- mcandidatefragments
            let fragments = concatMap (filterWithMaxSlotNo
                                         (notAlreadyInFlight inflight)
                                         (peerFetchMaxSlotNo inflight))
                                      chainfragments
            guard (not (null fragments)) ?! FetchDeclineInFlightThisPeer
            return (candidate, fragments)
    ]
  where
    notAlreadyInFlight inflight b =
      blockPoint b `Set.notMember` peerFetchBlocksInFlight inflight


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
  => FetchMode
  -> [( FetchDecision [AnchoredFragment header]
      , PeerFetchStatus header
      , PeerFetchInFlight header
      , peerinfo )]
  -> [(FetchDecision [AnchoredFragment header], peerinfo)]

filterNotAlreadyInFlightWithOtherPeers FetchModeDeadline chains =
    [ (mchainfragments,       peer)
    | (mchainfragments, _, _, peer) <- chains ]

filterNotAlreadyInFlightWithOtherPeers FetchModeBulkSync chains =
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
            PeerFetchStatusAberrant -> Set.empty
            _other                  -> peerFetchBlocksInFlight inflight
        | (_, status, inflight, _) <- chains ]

    -- The highest slot number that is or has been in flight for any peer.
    maxSlotNoInFlightWithOtherPeers = foldl' max NoMaxSlotNo
      [ peerFetchMaxSlotNo inflight | (_, _, inflight, _) <- chains ]

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

prioritisePeerChains
  :: forall extra header peer.
   ( HasHeader header
   , Hashable peer
   , Ord peer
   )
  => FetchMode
  -> Int
  -> (AnchoredFragment header -> AnchoredFragment header -> Ordering)
  -> (header -> SizeInBytes)
  -> [(FetchDecision (CandidateFragments header), PeerFetchInFlight header,
                                                  PeerGSV,
                                                  peer,
                                                  extra )]
  -> [(FetchDecision [AnchoredFragment header],   extra)]
prioritisePeerChains FetchModeDeadline salt compareCandidateChains blockFetchSize =
    map (\(decision, peer) ->
            (fmap (\(_,_,fragment) -> fragment) decision, peer))
  . concatMap ( concat
              . transpose
              . groupBy (equatingFst
                          (equatingRight
                            ((==) `on` chainHeadPoint)))
              . sortBy  (comparingFst
                          (comparingRight
                            (compare `on` chainHeadPoint)))
              )
  . groupBy (equatingFst
              (equatingRight
                (equatingPair
                   -- compare on probability band first, then preferred chain
                   (==)
                   (equateCandidateChains `on` getChainSuffix)
                 `on`
                   (\(band, chain, _fragments) -> (band, chain)))))
  . sortBy  (descendingOrder
              (comparingFst
                (comparingRight
                  (comparingPair
                     -- compare on probability band first, then preferred chain
                     compare
                     (compareCandidateChains `on` getChainSuffix)
                   `on`
                      (\(band, chain, _fragments) -> (band, chain))))))
  . map annotateProbabilityBand
  . sortBy (\(_,_,a,ap,_) (_,_,b,bp,_) ->
      comparePeerGSV' salt (a,ap) (b,bp))
  where
    annotateProbabilityBand (Left decline, _, _, _, peer) = (Left decline, peer)
    annotateProbabilityBand (Right (chain,fragments), inflight, gsvs, _, peer) =
        (Right (band, chain, fragments), peer)
      where
        band = probabilityBand $
                 estimateResponseDeadlineProbability
                   gsvs
                   (peerFetchBytesInFlight inflight)
                   (totalFetchSize blockFetchSize fragments)
                   deadline

    deadline = 2 -- seconds -- TODO: get this from external info

    equateCandidateChains chain1 chain2
      | EQ <- compareCandidateChains chain1 chain2 = True
      | otherwise                                  = False

    chainHeadPoint (_,ChainSuffix c,_) = AF.headPoint c

prioritisePeerChains FetchModeBulkSync salt compareCandidateChains blockFetchSize =
    map (\(decision, peer) ->
            (fmap (\(_, _, fragment) -> fragment) decision, peer))
  . sortBy (comparingFst
             (comparingRight
               (comparingPair
                  -- compare on preferred chain first, then duration
                  (compareCandidateChains `on` getChainSuffix)
                  compare
                `on`
                  (\(duration, chain, _fragments) -> (chain, duration)))))
  . map annotateDuration
  . sortBy (\(_,_,a,ap,_) (_,_,b,bp,_) ->
      comparePeerGSV' salt (a,ap) (b,bp))
  where
    annotateDuration (Left decline, _, _, _, peer) = (Left decline, peer)
    annotateDuration (Right (chain,fragments), inflight, gsvs, _, peer) =
        (Right (duration, chain, fragments), peer)
      where
        -- TODO: consider if we should put this into bands rather than just
        -- taking the full value.
        duration = estimateExpectedResponseDuration
                     gsvs
                     (peerFetchBytesInFlight inflight)
                     (totalFetchSize blockFetchSize fragments)

totalFetchSize :: (header -> SizeInBytes)
               -> [AnchoredFragment header]
               -> SizeInBytes
totalFetchSize blockFetchSize fragments =
  sum [ blockFetchSize header
      | fragment <- fragments
      , header   <- AF.toOldestFirst fragment ]

type Comparing a = a -> a -> Ordering
type Equating  a = a -> a -> Bool

descendingOrder :: Comparing a -> Comparing a
descendingOrder cmp = flip cmp

comparingPair :: Comparing a -> Comparing b -> Comparing (a, b)
comparingPair cmpA cmpB (a1, b1) (a2, b2) = cmpA a1 a2 <> cmpB b1 b2

equatingPair :: Equating a -> Equating b -> Equating (a, b)
equatingPair eqA eqB (a1, b1) (a2, b2) = eqA a1 a2 && eqB b1 b2

comparingEither :: Comparing a -> Comparing b -> Comparing (Either a b)
comparingEither _ _    (Left  _) (Right _) = LT
comparingEither cmpA _ (Left  x) (Left  y) = cmpA x y
comparingEither _ cmpB (Right x) (Right y) = cmpB x y
comparingEither _ _    (Right _) (Left  _) = GT

equatingEither :: Equating a -> Equating b -> Equating (Either a b)
equatingEither _ _   (Left  _) (Right _) = False
equatingEither eqA _ (Left  x) (Left  y) = eqA x y
equatingEither _ eqB (Right x) (Right y) = eqB x y
equatingEither _ _   (Right _) (Left  _) = False

comparingFst :: Comparing a -> Comparing (a, b)
comparingFst cmp = cmp `on` fst

equatingFst :: Equating a -> Equating (a, b)
equatingFst eq = eq `on` fst

comparingRight :: Comparing b -> Comparing (Either a b)
comparingRight = comparingEither mempty

equatingRight :: Equating b -> Equating (Either a b)
equatingRight = equatingEither (\_ _ -> True)

-- | Given the probability of the download completing within the deadline,
-- classify that into one of three broad bands: high, medium and low.
--
-- The bands are
--
-- * high:    98% -- 100%
-- * medium:  75% --  98%
-- * low:      0% --  75%
--
probabilityBand :: Double -> ProbabilityBand
probabilityBand p
  | p > 0.98  = ProbabilityHigh
  | p > 0.75  = ProbabilityModerate
  | otherwise = ProbabilityLow
 -- TODO: for hysteresis, increase probability if we're already using this peer

data ProbabilityBand = ProbabilityLow
                     | ProbabilityModerate
                     | ProbabilityHigh
  deriving (Eq, Ord, Show)


{-
In the second phase we walk over the prioritised fetch suffixes for each peer
and make a decision about whether we should initiate any new fetch requests.

This decision is based on a number of factors:

 * Is the fetch suffix empty? If so, there's nothing to do.
 * Do we already have block fetch requests in flight with this peer?
 * If so are we under the maximum number of in-flight blocks for this peer?
 * Is this peer still performing within expectations or has it missed any soft
   time outs?
 * Has the peer missed any hard timeouts or otherwise been disconnected.
 * Are we at our soft or hard limit of the number of peers we are prepared to
   fetch blocks from concurrently?

We look at each peer chain fetch suffix one by one. Of course decisions we
make earlier can affect decisions later, in particular the number of peers we
fetch from concurrently can increase if we fetch from a new peer, and we must
obviously take that into account when considering later peer chains.
-}


fetchRequestDecisions
  :: forall extra header peer.
      ( Hashable peer
      , HasHeader header
      , Ord peer
      )
  => FetchDecisionPolicy header
  -> FetchMode
  -> [( FetchDecision [AnchoredFragment header]
      , PeerFetchStatus header
      , PeerFetchInFlight header
      , PeerGSV
      , peer
      , extra)]
  -> [(FetchDecision (FetchRequest header), extra)]
fetchRequestDecisions fetchDecisionPolicy fetchMode chains =
    go nConcurrentFetchPeers0 Set.empty NoMaxSlotNo chains
  where
    go :: Word
       -> Set (Point header)
       -> MaxSlotNo
       -> [(Either FetchDecline [AnchoredFragment header],
            PeerFetchStatus header, PeerFetchInFlight header, PeerGSV, peer, extra)]
       -> [(FetchDecision (FetchRequest header), extra)]
    go !_ !_ !_ [] = []
    go !nConcurrentFetchPeers !blocksFetchedThisRound !maxSlotNoFetchedThisRound
       ((mchainfragments, status, inflight, gsvs, peer, extra) : cps) =

        (decision, extra)
      : go nConcurrentFetchPeers' blocksFetchedThisRound'
           maxSlotNoFetchedThisRound' cps
      where
        decision = fetchRequestDecision
                     fetchDecisionPolicy
                     fetchMode
                     -- Permitt the prefered peers to by pass any concurrency limits.
                     (if elem peer nPreferedPeers then 0
                                                  else nConcurrentFetchPeers)
                     (calculatePeerFetchInFlightLimits gsvs)
                     inflight
                     status
                     mchainfragments'

        mchainfragments' =
          case fetchMode of
            FetchModeDeadline -> mchainfragments
            FetchModeBulkSync -> do
                chainfragments <- mchainfragments
                let fragments =
                      concatMap (filterWithMaxSlotNo
                                   notFetchedThisRound
                                   maxSlotNoFetchedThisRound)
                                chainfragments
                guard (not (null fragments)) ?! FetchDeclineInFlightOtherPeer
                return fragments
              where
                notFetchedThisRound h =
                  blockPoint h `Set.notMember` blocksFetchedThisRound

        nConcurrentFetchPeers'
          -- increment if it was idle, and now will not be
          | peerFetchReqsInFlight inflight == 0
          , Right{} <- decision = nConcurrentFetchPeers + 1
          | otherwise           = nConcurrentFetchPeers

        -- This is only for avoiding duplication between fetch requests in this
        -- round of decisions. Avoiding duplication with blocks that are already
        -- in flight is handled by filterNotAlreadyInFlightWithOtherPeers
        (blocksFetchedThisRound', maxSlotNoFetchedThisRound') =
          case decision of
            Left _                         ->
              (blocksFetchedThisRound, maxSlotNoFetchedThisRound)
            Right (FetchRequest fragments) ->
              (blocksFetchedThisRound `Set.union` blocksFetchedThisDecision,
               maxSlotNoFetchedThisRound `max` maxSlotNoFetchedThisDecision)
              where
                maxSlotNoFetchedThisDecision =
                  foldl' max NoMaxSlotNo $ map MaxSlotNo $
                  mapMaybe (withOriginToMaybe . AF.headSlot) fragments

                blocksFetchedThisDecision =
                  Set.fromList
                    [ blockPoint header
                    | fragment <- fragments
                    , header   <- AF.toOldestFirst fragment ]

    nConcurrentFetchPeers0 = fromIntegral $ Set.size nActivePeers

    -- Set of peers with outstanding bytes.
    nActivePeers :: Set peer
    nActivePeers =
        Set.fromList
      . map snd
      . filter (\(inFlight, _) -> inFlight > 0)
      . map (\(_, _, PeerFetchInFlight{peerFetchReqsInFlight}, _, p, _) ->
                       (peerFetchReqsInFlight, p))
      $ chains

    -- Order the peers based on current PeerGSV. The top performing peers will be
    -- permitted to go active even if we're above the desired maxConcurrentFetchPeers
    -- which will cause us to switch smoothly from a slower to faster peers.
    -- When switching from slow to faster peers we will be over the configured limit, but
    -- PeerGSV is expected to be updated rather infrequently so the set of prefered peers should
    -- be stable during 10s of second.
    nPreferedPeers :: [peer]
    nPreferedPeers =
        map snd
      . take (fromIntegral maxConcurrentFetchPeers)
      . sortBy (\a b -> comparePeerGSV nActivePeers (peerSalt fetchDecisionPolicy) a b)
      . map (\(_, _, _, gsv, p, _) -> (gsv, p))
      $ chains

    maxConcurrentFetchPeers :: Word
    maxConcurrentFetchPeers =
      case fetchMode of
           FetchModeBulkSync -> maxConcurrencyBulkSync fetchDecisionPolicy
           FetchModeDeadline -> maxConcurrencyDeadline fetchDecisionPolicy


fetchRequestDecision
  :: HasHeader header
  => FetchDecisionPolicy header
  -> FetchMode
  -> Word
  -> PeerFetchInFlightLimits
  -> PeerFetchInFlight header
  -> PeerFetchStatus header
  -> FetchDecision [AnchoredFragment header]
  -> FetchDecision (FetchRequest  header)

fetchRequestDecision _ _ _ _ _ _ (Left decline)
  = Left decline

fetchRequestDecision _ _ _ _ _ PeerFetchStatusShutdown _
  = Left FetchDeclinePeerShutdown

fetchRequestDecision _ _ _ _ _ PeerFetchStatusAberrant _
  = Left FetchDeclinePeerSlow

fetchRequestDecision FetchDecisionPolicy {
                       maxConcurrencyBulkSync,
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
  = Left $ FetchDeclineBytesInFlightLimit
             peerFetchBytesInFlight
             inFlightBytesLowWatermark
             inFlightBytesHighWatermark

    -- This covers the case when we could still fit in more reqs or bytes, but
    -- we want to let it drop below a low water mark before sending more so we
    -- get a bit more batching behaviour, rather than lots of 1-block reqs.
  | peerFetchStatus == PeerFetchStatusBusy
  = Left $ FetchDeclinePeerBusy
             peerFetchBytesInFlight
             inFlightBytesLowWatermark
             inFlightBytesHighWatermark

    -- Refuse any blockrequest if we're above the concurrency limit.
  | let maxConcurrentFetchPeers = case fetchMode of
                                    FetchModeBulkSync -> maxConcurrencyBulkSync
                                    FetchModeDeadline -> maxConcurrencyDeadline
  , nConcurrentFetchPeers > maxConcurrentFetchPeers
  = Left $ FetchDeclineConcurrencyLimit
             fetchMode maxConcurrentFetchPeers

    -- If we're at the concurrency limit refuse any additional peers.
  | peerFetchReqsInFlight == 0
  , let maxConcurrentFetchPeers = case fetchMode of
                                    FetchModeBulkSync -> maxConcurrencyBulkSync
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
      | otherwise               = goFrag (nreqs+1) nbytes (AF.Empty (AF.anchor c)) c cs
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
