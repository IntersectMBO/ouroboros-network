{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE BangPatterns               #-}

module Ouroboros.Network.BlockFetch.Decision (
    fetchDecisions,
    fetchRequestDecisions,
    prioritisePeerChains,
    chainsFetchFragments,
    chainsForkSuffix,
    filterLongerCandidateChains,
    PeerInfo,
    FetchDecision,
    FetchDecline(..),
    FetchDecisionPolicy(..),
  ) where

import           Data.Maybe
import qualified Data.Set as Set
import           Data.Set (Set)

import           Control.Monad.Class.MonadTimer
import           Control.Exception (assert)

import           Ouroboros.Network.Block
import           Ouroboros.Network.Chain (Point, castPoint, blockPoint)
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


type PeerInfo header time extra =
       ( PeerFetchStatus,
         PeerFetchInFlight header,
         PeerGSV time,
         extra
       )

fetchDecisions :: (HasHeader header, HasHeader block,
                   HeaderHash header ~ HeaderHash block,
                   TimeMeasure time)
               => FetchDecisionPolicy header block
               -> ChainFragment block
               -> (Point block -> Bool)
               -> [(ChainFragment header, PeerInfo header time extra)]
               -> [(FetchDecision header, PeerInfo header time extra)]
fetchDecisions fetchDecisionPolicy@FetchDecisionPolicy {
                 plausibleCandidateChain,
                 compareCandidateChains,
                 blockFetchSize
               }
               currentChain
               fetchedBlocks =

    fetchRequestDecisions
      fetchDecisionPolicy
      True -- TODO: supply properly
  . map (\(c, p@(status,inflight,gsvs,_)) -> (c, status, inflight, gsvs, p))

  . prioritisePeerChains
      compareCandidateChains
      blockFetchSize
  . map (\(c, p@(_,inflight,gsvs,_)) -> (c, inflight, gsvs, p))

  . chainsFetchFragments
      fetchedBlocks
  . map (\(c, p@(_,inflight,_,_)) -> (c, peerFetchBlocksInFlight inflight, p))

  . chainsForkSuffix
      currentChain

  . filterLongerCandidateChains
      plausibleCandidateChain
      currentChain


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
filterLongerCandidateChains :: HasHeader header
                            => (ChainFragment block ->
                                ChainFragment header -> Bool)
                            -> ChainFragment block
                            -> [(ChainFragment header, peerinfo)]
                            -> [(ChainFragment header, peerinfo)]
filterLongerCandidateChains plausibleCandidateChain currentChain =
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
chainForkSuffix :: (HasHeader header, HasHeader block,
                    HeaderHash header ~ HeaderHash block)
                => ChainFragment block  -- ^ Current chain.
                -> ChainFragment header -- ^ Candidate chain
                -> Maybe (ChainFragment header)
chainForkSuffix current candidate =
    case ChainFragment.intersectChainFragments current candidate of
      Nothing                         -> Nothing
      Just (_, _, _, candidateSuffix) -> Just candidateSuffix

chainsForkSuffix :: (HasHeader header, HasHeader block,
                     HeaderHash header ~ HeaderHash block)
                 => ChainFragment block
                 -> [(ChainFragment header, peerinfo)]
                 -> [(ChainFragment header, peerinfo)]
chainsForkSuffix current chains =
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
chainFetchFragments :: forall header block.
                       (HasHeader header, HeaderHash header ~ HeaderHash block)
                    => (Point block  -> Bool)
                    -> (Point header -> Bool)
                    -> ChainFragment header
                    -> [ChainFragment header]
chainFetchFragments alreadyDownloaded alreadyInFlight =
    ChainFragment.filter notAlreadyDownloadedOrAlreadyInFlight
  where
    notAlreadyDownloadedOrAlreadyInFlight :: header -> Bool
    notAlreadyDownloadedOrAlreadyInFlight p =
        not (alreadyDownloaded (castPoint (blockPoint p))
          || alreadyInFlight   (blockPoint p))


chainsFetchFragments :: (HasHeader header,
                         HeaderHash header ~ HeaderHash block)
                     => (Point block -> Bool)
                     -> [ (ChainFragment header,  Set (Point header),
                                                  peerinfo)]
                     -> [([ChainFragment header], peerinfo)]
chainsFetchFragments alreadyDownloaded chains =
    [ (chainfragments, peer)
    | (chainsuffix, alreadyInFlight, peer) <- chains
    , let chainfragments = chainFetchFragments
                             alreadyDownloaded
                             (`Set.member` alreadyInFlight)
                             chainsuffix
    ]


{-
At this point we have all of the blocks we may be interested in downloading
from any peer. Decisions about downloading now depend on what we know about the
peers, such as the recent performance history and what requests we have
in-flight with that peer.

We split this into two phases.

 1. In the first phase we prioritise the chain/peer pairs based on the chain
    fetch suffix and estimated peer performance and status, but with very
    limited consideration of what is already in-flight. In particular we do not
    cut the fetch suffix down to exclude those that are already in flight.

 2. In the second phase we go through the chain/peer pairs in order and now
    based on what blocks are already in-flight we decide if it is time to
    initiate any new block fetch requests. The second phase is where we avoid
    asking the same peer for the same blocks again, and we apply our policies
    on whether to ask for the same block from multiple peers for redundancy, or
    to stripe block requests across multiple peers to maximise download speed
    without overloading individual peers.

The limited consideration of what is in-flight is actually a rather cunning
simplification of how to estimate the response time of block fetch requests.
One very simplistic approach would be to ignore what blocks are already
in-flight and estimate the response time based on the GSV just for the new
requests. This would of course ignore the fact that the remote peer may still
be busy sending us replies for requests we asked for previously. So it would
significantly underestimate the response times. A much more sophisticated
approach would be to try and track the estimated state of the remote peer's
queue of requests, to be able to estimate when that queue will empty and thus
the first moment at which new requests could begin to be serviced. This would
be complex and is perhaps attempting to be too precise, when there are other
confounding factors it cannot take into account. Our simpler approximation is
as follows. We track the size of responses that are still in-flight: that is
requests for responses of that size have been sent, but responses have not yet
been received. For streamed responses of multiple blocks, this is the size of
remaining blocks that have not yet been received. Then to estimate the response
time of a new request, we calculate it as if we were asking for all the
existing in-flight and new responses now from an idle state. This is of course
an overestimate of the response time, but bounded by G, the one-way minimum
latency. Given our use case this degree of overestimate is fine. We will be
using these estimated response times to compare between different peers and to
set timeouts for when a peer is considered to be responding too slowly. And
given that typical values of G might be as high as 0.3 sec, but our slot times
are 2 -- 20 seconds, then this is acceptable.

The first phase is simply a re-ordering of the chain/peer pairs. For now we
can leave this as the identify and defer discussion on the policy.

Slight change of plan to the above:

 * We will include temporary peer performance problems in the ranking,
   giving them a d-Q of _|_ so they can still be picked, but are always
   in the category of expecting not to make any deadline.

 * Reminder: why do we say it's ok to ignore what is in-flight when calculating
   the ETA of new requests? We're treating it as if we ask for all the in-flight
   plus new stuff now, which is only ever a half round-trip out (G). If we
   naively took into account in-flight then we'd assume the peer had an empty
   queue to deal with our new request. To do it in a more sophisticated way would
   require estimating the queue on the peer's end.

 * The downward closed property is perhaps not that big of a deal. We can track
   the set of in-flight blocks per-peer. This can be updated atomically when
   blocks bodies are validated and added to the fetched block heap. This means
   we can take a strategy where we go through peers in order accumulating the
   non-aberrant peers in-flight blocks and subtracting them from the ones we
   choose to fetch. This should give rise to distributing blocks between peers.
   As soon as one becomes aberrant then it drops to the bottom of the order
   (and we can also skip it for this accumulation) and we would fill in gaps
   using other peers.

 * The cost of managing in-flight blocks as individual blocks is probably not
   that big a deal, there will only ever be a few hundred of them in-flight
   at once.

 * We can auto-calibrate the max-in flight bytes per-peer to 2.5 -- 3x the
   bandwidth latency product (based on GSV). Assume symmetric G: then in G time
   we can send a certain amount (the product), and by the time the leading edge
   arrives the trailing edge would be leaving. So if we got it perfect, we'd
   need 2x to not leave any gaps. And then we should just go for a bit more to
   ensure the pipes stay full without gaps.

 * Need to add (block -> Size) function to fetch logic inputs
-}

prioritisePeerChains :: HasHeader header
                     => (ChainFragment header ->
                         ChainFragment header -> Ordering)
                     -> (header -> SizeInBytes)
                     -> [([ChainFragment header], PeerFetchInFlight header,
                                                  PeerGSV time,
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


type FetchDecision header = Either FetchDecline (FetchRequest header)




data FetchDecline =

     -- | All the blocks have been fetched or are in-flight.
     FetchDeclineNothingToDo
   | FetchDeclinePeerShutdown
   | FetchDeclinePeerSlow
   | FetchDeclineReqsInFlightLimit  !Word
   | FetchDeclineBytesInFlightLimit !SizeInBytes
   | FetchDeclinePeerBusy           !SizeInBytes !SizeInBytes !SizeInBytes
   | FetchDeclineConcurrencyLimit   !Word
  deriving Show



-- data FetchMode = TODO

fetchRequestDecisions :: (HasHeader header, TimeMeasure time)
                      => FetchDecisionPolicy header block
                      -> Bool -- TODO make an enum
                      -> [([ChainFragment header], PeerFetchStatus,
                                                   PeerFetchInFlight header,
                                                   PeerGSV time,
                                                   peer)]
                      -> [ (FetchDecision header,  peer)]
fetchRequestDecisions fetchDecisionPolicy parallelFetchMode chains =
    go nConcurrentFetchPeers0 Set.empty chains
  where
    go !_ _ [] = []
    go !nConcurrentFetchPeers !blocksInFlightOtherPeers
       ((chain, status, inflight, gsvs, peer) : cps) =

        (decision, peer)
      : go nConcurrentFetchPeers' blocksInFlightOtherPeers' cps
      where
        decision = fetchRequestDecision
                     fetchDecisionPolicy
                     nConcurrentFetchPeers
                     (calculatePeerFetchInFlightLimits gsvs)
                     inflight
                     status
                     chain'

        chain' | parallelFetchMode
               = concatMap (ChainFragment.filter alreadyInFlight) chain

               | otherwise
               = chain
        alreadyInFlight h = blockPoint h `Set.member` blocksInFlightOtherPeers

        nConcurrentFetchPeers'
          -- increment if it was idle, and now will not be
          | peerFetchReqsInFlight inflight == 0
          , Right{} <- decision = nConcurrentFetchPeers + 1
          | otherwise           = nConcurrentFetchPeers

        blocksInFlightOtherPeers' = blocksInFlightOtherPeers
                        `Set.union` peerFetchBlocksInFlight inflight

    nConcurrentFetchPeers0 =
        fromIntegral
      . length
      . filter (> 0)
      . map (\(_, _, PeerFetchInFlight{peerFetchReqsInFlight}, _, _) ->
                       peerFetchReqsInFlight)
      $ chains


fetchRequestDecision :: HasHeader header
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
selectBlocksUpToLimits :: HasHeader header
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


{--

-- | Given a set of chains, and which peer they are from, select an order of
-- preference for downloading from them.
--
selectChainInstances :: forall header peer.
                        (HasHeader header, Ord peer)
                     => [(Chain header, peer)] -- ^ Candidate header chains
                     -> BlockNo                -- ^ Current chain length
                     -> (peer -> DeltaQ)
                     -> Duration               -- ^ Deadline, from now.
                     -> [(Chain header, peer)]
selectChainInstances candidateChains 

-- We start with a bunch of candidate chains, and where they come from.
-- In general we'll have the same chain from multiple peers.
--
-- peer   1   2   3   4   5   6   7
--       +-+ +-+ +-+ +-+ +-+ +-+ +-+
--      _|_|_|_|_|_|_|_|_|D|_| |_|_|__ current chain length
--       |_| |C| |_| |_| +-+ |_| |_|
-- chain |A| +-+ |B| |A|     |B| |A|
--       +-+     +-+ +-+     +-+ +-+
--
-- So in this example we have two longest chains, A and B. Chain A is available
-- from three peers, and chain B is available from two. We could also be
-- interested in chain C, which is longer than the current adopted chain, but
-- is not the longest candidate chain, since it may turn out that we cannot
-- download all of A or B.
--
-- We may still be interested in the shorter chains such as C, or even ones 
-- that are not longer than our current one such as D, if they share blocks
-- with a chain that we are interested in, as that provides an alternative
-- source from which to download some of the blocks.

-- First we select the chains that are strictly longer than the current one.
--
-- In the example that's chains A, B, C (but not D)

  let chainsOfInterest = [ (chain, peer)
                         | (chain, peer) <- candidateChains
                         , headBlockNo chain > currentBlockNo ]

-- If there are no chains we're interested in downloading we can just wait for
-- longer candidates to arrive.
  check (not (null chainsOfInterest))

-- TODO: if our own chain is of equal or better length then we don't have to
-- download anything, but we may actually still be interested if we've got
-- some interesting fork of equal length that we may want to pre-emptively
-- download and cache.

-- Set up a mapping, of chains of interest to the set of peers from which they
-- are available
--
-- In the example above that would be:
--
-- A -> {1,4,7}
-- B -> {3,6}
-- C -> {2}

  let chainSources :: Map (Point header) peer
      chainSources = Map.fromListWith (++)
                       [ (headPoint chain, [peer])
                       | (chain, peer) <- chainsOfInterest ]

-- If we're in a deadline situation then we are not interested in choices that
-- are not likely to arrive within our deadline. So our first choices are
-- chains that are the longest and their ETA (based on DeltaQ models) is within
-- the deadline. Our second choices are chains that are not the longest, but
-- are longer than the current chain and their ETA is within the deadline.
-- Finally, last choice is chains with an ETA outside of the deadline.
--
-- For our running example, the sake of argument suppose we know that peers
-- 6 and 7 have terrible performance (perhaps they're a long way away, or are
-- overloaded), then we would divide the peers into three groups, and within
-- each group organise them by chain and then perhaps pseudorandomly weighted
-- by some measure to spread out load.
--
--
-- Chain   A   B   C
--        ___________
--
-- Peers   1   3        Longest chains, ETA within deadline
--         4
--        ___________
--
--                 2    Longer than current chains, ETA within deadline
--        ___________
--
--         7   6        All other chains, ETA not within deadline
--
--
-- Now within the major groups we want to fall back to alternative chains
-- before falling back to alternative sources for the same chain, since this
-- policy mitigates some potential attacks.
--
-- So the overall order of preference is to transpose and concatenate, giving
-- the peer preference order of:
--
-- 1 3 4 2 7 6
--
--
-}

-- Invariant: no gaps in downloaded chains, so no splitting between peers


{-
We start with a bunch of candidate chains, and where they come from.
In general we'll have the same chain from multiple peers.

peer    1     2     3     4     5     6     7
      ┌───┐ ┌───┐ ┌───┐ ┌───┐ ┌───┐ ┌───┐ ┌───┐
      │   │ │   │ │   │ │   │ │   │ │   │ │   │ 
    ──┼───┼─┼───┼─┼───┼─┼───┼─┴───┴─┼───┼─┼───┼──   current chain length
      │   │ │   │ │   │ │   │       │   │ │   │
      ├───┤ └───┘ ├───┤ ├───┤       ├───┤ ├───┤
      │   │       │   │ │   │       │   │ │   │
      └───┘       └───┘ └───┘       └───┘ └───┘
chain   A     C     B     A     D     B     A



-- So in this example we have two longest chains, A and B. Chain A is available
-- from three peers, and chain B is available from two. We could also be
-- interested in chain C, which is longer than the current adopted chain, but
-- is not the longest candidate chain, since it may turn out that we cannot
-- download all of A or B.
--
-- We may still be interested in the shorter chains such as C, or even ones 
-- that are not longer than our current one such as D, if they share blocks
-- with a chain that we are interested in, as that provides an alternative
-- source from which to download some of the blocks.



    ┆   ┆
    ├───┤
    │  ◀┿━━┓
    ├───┤  ┃  ┌───┐
    │   │  ┗━━┿   │
    ├───┤     ├───┤
    │   │     │   │
    ├───┤     ├───┤
    │   │     │   │
 ───┴───┴─────┼───┼────
              │   │
              └───┘


    ┆   ┆     ┆   ┆     ┆   ┆     ┆   ┆
    ├───┤     ├───┤     ├───┤     ├───┤
    │   │     │   │     │   │     │   │
    ├───┤     ├───┤     ├───┤     ├───┤
    │   │     │   │     │   │     │   │
    ├───┤     ├───┤     ├───┤     ├───┤
    │   │     │   │     │   │     │   │
    ├───┤     ├───┤     ├───┤     ├───┤
    │   │     │   │     │   │     │   │
 ───┴───┴─────┼───┼─────┼───┼─────┼───┼───
   current/   │   │     │   │     │   │
   adopted    └───┘     ├───┤     ├───┤
 (full blocks)          │   │     │   │
                        └───┘     └───┘
                A         B   candidates (headers only)

Fetch range: the suffix of the fork range that has not yet been downloaded
In flight range: the range(s) of the fork range that have outstanding/in-flight
                 download requests (there can be multiple from different peers)
Request range: a suffix range for a particular peer, within the fetch range

-}

