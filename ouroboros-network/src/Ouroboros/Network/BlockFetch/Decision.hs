{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE BangPatterns               #-}

module Ouroboros.Network.BlockFetch.Decision (
    -- * Deciding what to fetch
    fetchDecisions,
    FetchDecisionPolicy(..),
    PeerInfo,
    FetchDecision,
    FetchDecline(..),

    -- ** Components of the decision-making process
    filterPlausibleCandidates,
    selectForkSuffixes,
    filterNotAlreadyFetched,
    filterNotAlreadyInFlightWithPeer,
    prioritisePeerChains,
    filterNotAlreadyInFlightWithOtherPeers,
    fetchRequestDecisions,
  ) where

import           Data.Maybe
import qualified Data.Set as Set

import           Control.Exception (assert)

import           Ouroboros.Network.Block
import           Ouroboros.Network.ChainFragment (ChainFragment(..))
import qualified Ouroboros.Network.ChainFragment as ChainFragment

import           Ouroboros.Network.BlockFetch.Types
                   ( FetchRequest(..)
                   , PeerFetchInFlight(..)
                   , PeerFetchStatus(..)
                   , SizeInBytes
                   )
import           Ouroboros.Network.BlockFetch.DeltaQ
                   ( PeerGSV(..), PeerFetchInFlightLimits(..)
                   , calculatePeerFetchInFlightLimits )


data FetchDecisionPolicy header block = FetchDecisionPolicy {
       maxConcurrentFetchPeers :: Word,
       maxInFlightReqsPerPeer  :: Word,  -- A protocol constant.

       plausibleCandidateChain :: ChainFragment block
                               -> ChainFragment header -> Bool,

       compareCandidateChains  :: ChainFragment header
                               -> ChainFragment header
                               -> Ordering,

       blockFetchSize          :: header -> SizeInBytes
     }


type PeerInfo header extra =
       ( PeerFetchStatus,
         PeerFetchInFlight header,
         PeerGSV,
         extra
       )

type FetchDecision header = Either FetchDecline (FetchRequest header)

-- | All the various reasons we can decide not to fetch blocks from a peer.
--
data FetchDecline =
     FetchDeclineNothingToDo
   | FetchDeclinePeerShutdown
   | FetchDeclinePeerSlow
   | FetchDeclineReqsInFlightLimit  !Word
   | FetchDeclineBytesInFlightLimit !SizeInBytes
   | FetchDeclinePeerBusy           !SizeInBytes !SizeInBytes !SizeInBytes
   | FetchDeclineConcurrencyLimit   !Word
  deriving (Eq, Show)


fetchDecisions
  :: (HasHeader header, HasHeader block,
      HeaderHash header ~ HeaderHash block)
  => FetchDecisionPolicy header block
  -> ChainFragment block
  -> (Point block -> Bool)
  -> [(ChainFragment header, PeerInfo header extra)]
  -> [(FetchDecision header, PeerInfo header extra)]
fetchDecisions fetchDecisionPolicy@FetchDecisionPolicy {
                 plausibleCandidateChain,
                 compareCandidateChains,
                 blockFetchSize
               }
               currentChain
               fetchedBlocks =

    -- Finally, make a decision for each (chain, peer) pair.
    fetchRequestDecisions
      fetchDecisionPolicy
  . map swizzleSIG

    -- Filter to keep blocks that are not already in-flight with other peers.
  . filterNotAlreadyInFlightWithOtherPeers
      True -- TODO: supply properly
  . map swizzleSI

    -- Reorder chains based on consensus policy and network timing data.
  . prioritisePeerChains
      compareCandidateChains
      blockFetchSize
  . map swizzleIG

    -- Filter to keep blocks that are not already in-flight for this peer.
  . filterNotAlreadyInFlightWithPeer
  . map swizzleI

    -- Filter to keep blocks that have not already been downloaded.
  . filterNotAlreadyFetched
      fetchedBlocks

    -- Select the suffix up to the intersection with the current chain.
  . selectForkSuffixes
      currentChain

    -- First, filter to keep chains the consensus layer tells us are plausible.
  . filterPlausibleCandidates
      plausibleCandidateChain
      currentChain
  where
    -- Data swizzling functions to get the right info into each stage.
    swizzleI   (c, p@(_,     inflight,_,   _)) = (c,         inflight,       p)
    swizzleIG  (c, p@(_,     inflight,gsvs,_)) = (c,         inflight, gsvs, p)
    swizzleSI  (c, p@(status,inflight,_,   _)) = (c, status, inflight,       p)
    swizzleSIG (c, p@(status,inflight,gsvs,_)) = (c, status, inflight, gsvs, p)

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


-- | Keep only those candidate chains that are strictly longer than a given
-- length (typically the length of the current adopted chain).
--
filterPlausibleCandidates
  :: HasHeader header
  => (ChainFragment block -> ChainFragment header -> Bool)
  -> ChainFragment block
  -> [(ChainFragment header, peerinfo)]
  -> [(ChainFragment header, peerinfo)]
filterPlausibleCandidates plausibleCandidateChain currentChain =
    filter (\(c, _) -> plausibleCandidateChain currentChain c)

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

-- | A chain and a selected suffix. It is represented as a range between a
-- given point (exclusive) and the end of the chain (inclusive).
--
-- So for example the empty suffix has the point as the chain head.
--
--type ChainSuffix header = ChainFragment header

{-
We define the /fork range/ as the suffix of the candidate chain up until (but
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

-- | Find the fork suffix range for a candidate chain, with respect to the
-- current chain.
--
chainForkSuffix
  :: (HasHeader header, HasHeader block,
      HeaderHash header ~ HeaderHash block)
  => ChainFragment block  -- ^ Current chain.
  -> ChainFragment header -- ^ Candidate chain
  -> Maybe (ChainFragment header)
chainForkSuffix current candidate =
    case ChainFragment.intersectChainFragments current candidate of
      Nothing                         -> Nothing
      Just (_, _, _, candidateSuffix) -> Just candidateSuffix

selectForkSuffixes
  :: (HasHeader header, HasHeader block,
      HeaderHash header ~ HeaderHash block)
  => ChainFragment block
  -> [(ChainFragment header, peerinfo)]
  -> [(ChainFragment header, peerinfo)]
selectForkSuffixes current chains =
    catMaybes [ (,) <$> chainForkSuffix current chain <*> pure peer
              | (chain, peer) <- chains ]


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

-- | Find the fragments of the chain that we still need to fetch, these are the
-- fragments covering blocks that have not yet been fetched and are not
-- currently in the process of being fetched from this peer.
--
-- Typically this is a single fragment forming a suffix of the chain, but in
-- the general case we can get a bunch of discontiguous chain fragments.
--
filterNotAlreadyFetched
  :: (HasHeader header, HeaderHash header ~ HeaderHash block)
  => (Point block -> Bool)
  -> [ (ChainFragment header,  peerinfo)]
  -> [([ChainFragment header], peerinfo)]
filterNotAlreadyFetched alreadyDownloaded chains =
    [ (chainfragments, peer)
    | (chainsuffix,    peer) <- chains
    , let chainfragments =
            ChainFragment.filter notAlreadyFetched chainsuffix
    ]
  where
    notAlreadyFetched = not . alreadyDownloaded . castPoint . blockPoint


filterNotAlreadyInFlightWithPeer
  :: HasHeader header
  => [([ChainFragment header], PeerFetchInFlight header, peerinfo)]
  -> [([ChainFragment header],                           peerinfo)]
filterNotAlreadyInFlightWithPeer chains =
    [ (chainfragments',          peer)
    | (chainfragments, inflight, peer) <- chains
    , let chainfragments' =
            concatMap (ChainFragment.filter (notAlreadyInFlight inflight))
                      chainfragments
    ]
  where
    notAlreadyInFlight inflight b =
      blockPoint b `Set.notMember` peerFetchBlocksInFlight inflight


-- data FetchMode = TODO

-- One last step of filtering, but this time across peers, rather than
-- individually for each peer. If we're following the parallel fetch
-- mode then we filter out blocks that are already in-flight with other
-- peers.
filterNotAlreadyInFlightWithOtherPeers
  :: HasHeader header
  => Bool -- TODO make an enum
  -> [([ChainFragment header], PeerFetchStatus,
                               PeerFetchInFlight header,
                               peer)]
  -> [([ChainFragment header], peer)]

filterNotAlreadyInFlightWithOtherPeers parallelFetchMode chains
  | not parallelFetchMode
  = [ (mchainfragments,       peer)
    | (mchainfragments, _, _, peer) <- chains ]

filterNotAlreadyInFlightWithOtherPeers _ chains =
    go Set.empty chains
  where
    go !_ [] = []
    go !blocksInFlightWithOtherPeers
       ((chainfragments, status, inflight, peer) : cps) =

        (chainfragments', peer)
      : go blocksInFlightWithOtherPeers' cps
      where
        chainfragments' =
          concatMap (ChainFragment.filter notAlreadyInFlight) chainfragments

        notAlreadyInFlight b =
          blockPoint b `Set.notMember` blocksInFlightWithOtherPeers

        blocksInFlightWithOtherPeers' = case status of
          PeerFetchStatusShutdown -> blocksInFlightWithOtherPeers
          PeerFetchStatusAberrant -> blocksInFlightWithOtherPeers
          _other                  -> blocksInFlightWithOtherPeers
                         `Set.union` peerFetchBlocksInFlight inflight


prioritisePeerChains
  :: HasHeader header
  => (ChainFragment header ->
     ChainFragment header -> Ordering)
  -> (header -> SizeInBytes)
  -> [([ChainFragment header], PeerFetchInFlight header,
                               PeerGSV,
                               peer)]
  -> [([ChainFragment header], peer)]
prioritisePeerChains _compareCandidateChains _blockFetchSize =
    map (\(c,_,_,p) -> (c,p))

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
  :: HasHeader header
  => FetchDecisionPolicy header block
  -> [([ChainFragment header], PeerFetchStatus,
                               PeerFetchInFlight header,
                               PeerGSV,
                               peer)]
  -> [ (FetchDecision header,  peer)]
fetchRequestDecisions fetchDecisionPolicy chains =
    go nConcurrentFetchPeers0 chains
  where
    go !_ [] = []
    go !nConcurrentFetchPeers
       ((chain, status, inflight, gsvs, peer) : cps) =

        (decision, peer)
      : go nConcurrentFetchPeers' cps
      where
        decision = fetchRequestDecision
                     fetchDecisionPolicy
                     nConcurrentFetchPeers
                     (calculatePeerFetchInFlightLimits gsvs)
                     inflight
                     status
                     chain

        nConcurrentFetchPeers'
          -- increment if it was idle, and now will not be
          | peerFetchReqsInFlight inflight == 0
          , Right{} <- decision = nConcurrentFetchPeers + 1
          | otherwise           = nConcurrentFetchPeers

    nConcurrentFetchPeers0 =
        fromIntegral
      . length
      . filter (> 0)
      . map (\(_, _, PeerFetchInFlight{peerFetchReqsInFlight}, _, _) ->
                       peerFetchReqsInFlight)
      $ chains


fetchRequestDecision
  :: HasHeader header
  => FetchDecisionPolicy header block
  -> Word
  -> PeerFetchInFlightLimits
  -> PeerFetchInFlight header
  -> PeerFetchStatus
  -> [ChainFragment header]
  -> FetchDecision header

fetchRequestDecision _ _ _ _ _ [] = Left FetchDeclineNothingToDo

fetchRequestDecision _ _ _ _ PeerFetchStatusShutdown _
                                  = Left FetchDeclinePeerShutdown

fetchRequestDecision _ _ _ _ PeerFetchStatusAberrant _
                                  = Left FetchDeclinePeerSlow

fetchRequestDecision FetchDecisionPolicy {
                       maxConcurrentFetchPeers,
                       maxInFlightReqsPerPeer,
                       blockFetchSize
                     }
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
                     fetchFragments

  | peerFetchReqsInFlight >= maxInFlightReqsPerPeer
  = Left $ FetchDeclineReqsInFlightLimit
             maxInFlightReqsPerPeer

  | peerFetchBytesInFlight >= inFlightBytesHighWatermark
  = Left $ FetchDeclineBytesInFlightLimit
             inFlightBytesHighWatermark

    -- This covers the case when we could still fit in more reqs or bytes, but
    -- we want to let it drop below a low water mark before sending more so we
    -- get a bit more batching behaviour, rather than lots of 1-block reqs.
  | peerFetchStatus == PeerFetchStatusBusy
  = Left $ FetchDeclinePeerBusy
             peerFetchBytesInFlight
             inFlightBytesLowWatermark
             inFlightBytesHighWatermark

  | peerFetchReqsInFlight == 0
  , nConcurrentFetchPeers >= maxConcurrentFetchPeers
  = Left $ FetchDeclineConcurrencyLimit
             maxConcurrentFetchPeers

    -- We've checked our request limit and our byte limit. We are then
    -- guaranteed to get at least one non-empty request range.
  | otherwise
  = assert (peerFetchReqsInFlight < maxInFlightReqsPerPeer) $

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
  :: HasHeader header
  => (header -> SizeInBytes) -- ^ Block body size
  -> Word -- ^ Current number of requests in flight
  -> Word -- ^ Maximum number of requests in flight allowed
  -> SizeInBytes -- ^ Current number of bytes in flight
  -> SizeInBytes -- ^ Maximum number of bytes in flight allowed
  -> [ChainFragment header]
  -> FetchRequest header
selectBlocksUpToLimits blockFetchSize nreqs0 maxreqs nbytes0 maxbytes fragments =
    assert (nreqs0 < maxreqs && nbytes0 < maxbytes && not (null fragments)) $
    -- The case that we are already over our limits has to be checked earlier,
    -- outside of this function. From here on however we check for limits.

    FetchRequest
      [ assert (not (ChainFragment.null fragment)) $
        ChainFragment.toOldestFirst fragment
      | fragment <- goFrags nreqs0 nbytes0 fragments ]
  where
    goFrags _     _      []     = []
    goFrags nreqs nbytes (c:cs)
      | nreqs+1  > maxreqs      = []
      | otherwise               = goFrag (nreqs+1) nbytes Empty c cs
      -- Each time we have to pick from a new discontiguous chain fragment then
      -- that will become a new request, which contributes to our in-flight
      -- request count. We never break the maxreqs limit.

    goFrag nreqs nbytes c' Empty    cs = c' : goFrags nreqs nbytes cs
    goFrag nreqs nbytes c' (b :< c) cs
      | nbytes' >= maxbytes            = [c' :> b]
      | otherwise                      = goFrag nreqs nbytes' (c' :> b) c cs
      where
        nbytes' = nbytes + blockFetchSize b
      -- Note that we always pick the one last block that crosses the maxbytes
      -- limit. This cover the case where we otherwise wouldn't even be able to
      -- request a single block, as it's too large.

