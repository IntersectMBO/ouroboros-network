{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Ouroboros.Network.BlockFetch.Tests (
    unit1,
    unit2,
    unit3,

    demoBlockServer,
    demo2,
    demo2Example,

  ) where

import           Data.Maybe
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Set (Set)

import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadTimer
import           Control.Exception (throw)

import           Ouroboros.Network.Block
import           Ouroboros.Network.Chain (Point, castPoint, blockPoint)
import           Ouroboros.Network.ChainFragment (ChainFragment(..))
import qualified Ouroboros.Network.ChainFragment as ChainFragment

import           Ouroboros.Network.Protocol.BlockFetch.Type
import           Ouroboros.Network.Protocol.BlockFetch.Server
import           Ouroboros.Network.Protocol.BlockFetch.Codec
import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Driver
import           Ouroboros.Network.Channel

import           Ouroboros.Network.DeltaQ
import           Ouroboros.Network.BlockFetch.Types
import           Ouroboros.Network.BlockFetch.Client
import           Ouroboros.Network.BlockFetch.Decision

import           Ouroboros.Network.Testing.ConcreteBlock
import           Control.Monad.IOSim
--import           Test.QuickCheck
--import           Test.Chain
--import           Debug.Trace




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

exCurChain :: ChainFragment Block
exCurChain =
  mkChainFragmentSimple ["cur1", "cur2", "cur3", "cur4"]

exPeerChainA :: ChainFragment BlockHeader
exPeerChainA =
  mkChainFragmentHeadersSimple ["cur1", "A1", "A2", "A3", "A4", "A5"]

exPeerChainB :: ChainFragment BlockHeader
exPeerChainB =
  mkChainFragmentHeadersSimple ["cur1", "cur2", "cur3", "cur4", "B1", "B2"]

exPeerChainC :: ChainFragment BlockHeader
exPeerChainC =
  mkChainFragmentHeadersSimple ["cur1", "cur2", "cur3", "cur4", "C1"]

exPeerChainD :: ChainFragment BlockHeader
exPeerChainD =
  mkChainFragmentHeadersSimple ["cur1", "cur2", "cur3"]

exPeerChains :: [(ChainFragment BlockHeader, String)]
exPeerChains =
  [ (exPeerChainC, "Peer1")
  , (exPeerChainA, "Peer2")
  , (exPeerChainB, "Peer3")
  , (exPeerChainA, "Peer4")
  , (exPeerChainD, "Peer5")
  , (exPeerChainB, "Peer6")
  , (exPeerChainA, "Peer7")
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

exPeerChains2 :: [(ChainFragment BlockHeader, String)]
exPeerChains2 = filterLongerCandidateChains
                  (\currentChain c ->
                        ChainFragment.headBlockNo c
                      > ChainFragment.headBlockNo currentChain)
                  exCurChain
                  exPeerChains

unit1 :: Bool
unit1 = exPeerChains2
     == [ (exPeerChainC, "Peer1")
        , (exPeerChainA, "Peer2")
        , (exPeerChainB, "Peer3")
        , (exPeerChainA, "Peer4")
          -- filtered out Peer5
        , (exPeerChainB, "Peer6")
        , (exPeerChainA, "Peer7")
        ]

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


exPeerChains3 :: [(ChainFragment BlockHeader, String)]
exPeerChains3 = chainsForkSuffix
                  exCurChain
                  exPeerChains2

unit2 :: Bool
unit2 = exPeerChains3
     == [ (exPeerChainC', "Peer1")
        , (exPeerChainA', "Peer2")
        , (exPeerChainB', "Peer3")
        , (exPeerChainA', "Peer4")
        , (exPeerChainB', "Peer6")
        , (exPeerChainA', "Peer7")
        ]
  where
    exPeerChainA' = ChainFragment.takeNewest 5 exPeerChainA
    exPeerChainB' = ChainFragment.takeNewest 2 exPeerChainB
    exPeerChainC' = ChainFragment.takeNewest 1 exPeerChainC

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


exPeerChains4 :: [([ChainFragment BlockHeader], String)]
exPeerChains4 =
    chainsFetchFragments
      alreadyDownloaded
      [ (chain, alreadyInFlight peer, peer) | (chain, peer) <- exPeerChains3 ]
  where
    alreadyDownloaded :: Point Block -> Bool
    alreadyDownloaded = const False
    alreadyInFlight :: String -> Set (Point BlockHeader)
    alreadyInFlight "Peer2" = Set.fromList [pointA1, pointA2]
    alreadyInFlight "Peer4" = Set.fromList [         pointA2]
    alreadyInFlight _       = Set.empty
    pointA1 = grabPoint exPeerChainA 2 "A1"
    pointA2 = grabPoint exPeerChainA 3 "A2"
    grabPoint chain slot expectedBody =
      case ChainFragment.lookupBySlot chain slot of
        Just b
          | headerBodyHash b == hashBody (BlockBody expectedBody)
                      -> blockPoint b
          | otherwise -> error ("grabPoint: expected hash of " ++ show expectedBody)
        _             -> error ("grabPoint: no block at slot " ++ show slot)

unit3 :: Bool
unit3 = [ map (map headerBodyHash . ChainFragment.toOldestFirst) cf
        | (cf, _) <- exPeerChains4 ]
     == 
        map (map (map (hashBody . BlockBody)))
        [ [["C1"]]
        , [["A3", "A4", "A5"]]
        , [["B1", "B2"]]
        , [["A1"], ["A3", "A4", "A5"]] -- two fragments
        , [["B1", "B2"]]
        , [["A1", "A2", "A3", "A4", "A5"]]
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




{-
exPeerFetchDecisions :: [(FetchDecision BlockHeader, String)]
exPeerFetchDecisions =
    fetchRequestDecisions
      exampleFetchClientPolicy
      fetchPolicyParams
      [  | (chain, peer) <- exPeerChains4 ]
  where
    fetchPolicyParams = FetchPolicyParams {
        maxInFlightReqsPerPeer   = 2,
        maxConcurrentFetchPeers  = 4
      }

    peerFetchStates =
      Map.fromList
        [ ("Peer1", defaultPeerFetchState { peerFetchInFlight = initialPeerFetchInFlight { peerFetchReqsInFlight = 2 } } )
        , ("Peer2", defaultPeerFetchState { peerFetchInFlight = initialPeerFetchInFlight { peerFetchReqsInFlight = 2 } } )
        , ("Peer3", defaultPeerFetchState { peerFetchInFlight = initialPeerFetchInFlight { peerFetchReqsInFlight = 2 } } )
        , ("Peer4", defaultPeerFetchState)
        , ("Peer5", defaultPeerFetchState)
        , ("Peer6", defaultPeerFetchState)
        , ("Peer7", defaultPeerFetchState)
        ]
    initialPeerFetchInFlight = PeerFetchInFlight 0 0 Set.empty
    defaultPeerFetchState = PeerFetchState
                              PeerFetchStatusReady
                              initialPeerFetchInFlight
                              (error "TODO: defaultPeerFetchState1")
                              (error "TODO: defaultPeerFetchState2")
-}




data TestFetchedBlockHeap m block = TestFetchedBlockHeap {
       getTestFetchedBlocks  :: m (Point block -> Maybe block),
       addTestFetchedBlock   :: Point block -> block -> m ()
     }

mkTestFetchedBlockHeap :: (MonadSTM m, HasHeader block)
                       => m (TestFetchedBlockHeap m block)
mkTestFetchedBlockHeap = do
    v <- atomically (newTVar Map.empty)
    return TestFetchedBlockHeap {
      getTestFetchedBlocks = flip Map.lookup <$> atomically (readTVar v),
      addTestFetchedBlock  = \p b -> atomically (modifyTVar' v $ Map.insert p b)
    }

{-
demo1 :: ChainFragment Block
      -> ChainFragment BlockHeader
      -> ChainFragment BlockHeader
      -> IO Bool
demo1 currentChain candidateChain1 candidateChain2 = do
    currentChainVar    <- newTVarM currentChain
    fetchedBlocksVar   <- newTVarM Set.empty
    candidateChain1Var <- newTVarM candidateChain1
    candidateChain2Var <- newTVarM candidateChain2
    let candidateChainVars =
          Map.fromList [("peer1", candidateChain1Var)
                       ,("peer2", candidateChain2Var)]
        peerStates =
          Map.fromList [("peer1", PeerFetchState PeerFetchStatusReady (PeerFetchInFlight 0 0 Set.empty) undefined undefined)
                       ,("peer2", PeerFetchState PeerFetchStatusReady (PeerFetchInFlight 0 0 Set.empty) undefined undefined)]
        peerGSVs =
          Map.fromList [("peer1", GSV 20000 0 (Distribution 0))
                       ,("peer2", GSV 15000 0 (Distribution 0))]

    let fetchTriggerVariables :: FetchTriggerVariables String BlockHeader Block IO
        fetchTriggerVariables = FetchTriggerVariables {
          readStateCurrentChain    = readTVar currentChainVar,
          readStateCandidateChains = traverse readTVar candidateChainVars,
          readStatePeerStatus      = return (Map.map peerFetchStatus peerStates)
        }
        fetchNonTriggerVariables :: FetchNonTriggerVariables String BlockHeader Block IO
        fetchNonTriggerVariables = FetchNonTriggerVariables {
          readStateFetchedBlocks = flip Set.member <$> readTVar fetchedBlocksVar,
          readStatePeerStates    = return peerStates
        }
        fetchStateFingerprint = initialFetchStateFingerprint

    fetchStateFingerprint' <-
      fetchLogicIteration
        consensusFunctions
        fetchPolicyParams
        fetchTriggerVariables
        fetchNonTriggerVariables
        fetchStateFingerprint

    return (fetchStateFingerprint' /= fetchStateFingerprint)
  where
    fetchPolicyParams = FetchPolicyParams {
        maxInFlightReqsPerPeer   = 10,
        maxConcurrentFetchPeers  = 1
      }
    consensusFunctions = undefined
-}

-- Tests needed for peer logic:

-- test of pure logic:
--   that it makes a decision for each peer
--   peers in == peers out
--   decisions look kosher
--   does decide to fetch when there are some available below conc limits
--   never violates limits
--   we prefer faster peers
-- tests of STM bits
--   that we do trigger on certain changes
--   that we do not trigger on others
--   we don't spin busy waiting endlessly, can go idle
-- test of fetch proxy
-- overall integration with fetch proxy:
--   n changing candidate chains
--   that we do download candidate blocks
--   we prefer faster peers
-- no "getting stuck", no matter what fetch peers do we make some progress
--   so long as we have at least one working peer
-- streaming property, do we keep request buffers full enough?
-- attack resistance properties
--   unit tests to do the "right" thing in various cases

{-
--
-- For testing: simple CandidateChains impl
--

-- | A collection of chains that supports concurrent modification and change
-- detection via STM.
--
newtype CandidateChains m peer header =
        CandidateChains (TVar m (Map peer (TVar m (ChainFragment header))))

registerCandidateChain :: (MonadSTM m, Ord peer)
                       => CandidateChains m peer header
                       -> peer
                       -> TVar m (ChainFragment header)
                       -> STM m ()
registerCandidateChain (CandidateChains chainsVar) peerid chainVar =
    modifyTVar' chainsVar (Map.insert peerid chainVar)

unregisterCandidateChain :: (MonadSTM m, Ord peer)
                         => CandidateChains m peer header
                         -> peer
                         -> STM m ()
unregisterCandidateChain (CandidateChains chainsVar) peerid =
    modifyTVar' chainsVar (Map.delete peerid)

readCandidateChains' :: MonadSTM m
                    => CandidateChains m peer header
                    -> STM m (Map peer (ChainFragment header))
readCandidateChains' (CandidateChains chainsVar) =
    traverse readTVar =<< readTVar chainsVar
-}



demo2Example :: IO ()
demo2Example =
  mapM_ print $
  selectTraceSay $ runSimTrace $
  demo2 $
  mkChainFragmentSimple ["foo", "bar"]

selectTraceSay :: Trace a -> [String]
selectTraceSay (Trace _ _ (EventSay msg) trace) = msg : selectTraceSay trace
selectTraceSay (Trace _ _ _              trace) = selectTraceSay trace
selectTraceSay (TraceMainReturn _ _ _)          = []
selectTraceSay (TraceMainException _ e _)       = throw (FailureException e)
selectTraceSay (TraceDeadlock _ _)              = throw FailureDeadlock

demo2 :: forall m. (MonadSTM m, MonadST m, MonadAsync m,
                    MonadCatch m, MonadTimer m, MonadSay m)
      => ChainFragment Block -> m ()
demo2 chain = do

    fetchClientInFlightVar <- atomically $ newTVar initialPeerFetchInFlight
    fetchClientStatusVar   <- atomically $ newTVar PeerFetchStatusReady
    fetchClientRequestVar  <- atomically $ newTFetchRequestVar

    blockHeap <- mkTestFetchedBlockHeap

    let server :: Peer (BlockFetch BlockHeader Block) AsServer BFIdle m ()
        server = blockFetchServerPeer $
                   demoBlockServer chain
        client = blockFetchClient
                   FetchClientPolicy {
                     blockFetchSize     = \_ -> 100,
                     blockMatchesHeader = \_ _ -> True,
                     addFetchedBlock    = addTestFetchedBlock blockHeap
                   }
                   FetchClientStateVars {
                     fetchClientStatusVar,
                     fetchClientInFlightVar,
                     fetchClientRequestVar
                   }
                   readPeerGSVs
        -- roughly 10ms ping time and 1MBit/s bandwidth
        -- leads to 2000 bytes in flight minimum
        readPeerGSVs = return PeerGSV{outboundGSV, inboundGSV}
        inboundGSV   = ballisticGSV 10000 10 (degenerateDistribution 0)
        outboundGSV  = inboundGSV

        -- Watch and log the client's status and in-flight vars
        observer = go PeerFetchStatusShutdown initialPeerFetchInFlight
          where
            go status inflight = do
              (status', inflight') <- atomically $ do
                status'   <- readTVar fetchClientStatusVar
                inflight' <- readTVar fetchClientInFlightVar
                check (status' /= status || inflight /= inflight')
                return (status', inflight')
              say (show status')
              say (show inflight')
              go status' inflight'

        -- Actually tell the fetch client to do anything
        testDriver = do
          -- test1, whole chain fragment
          let request = FetchRequest [map blockHeader (ChainFragment.toOldestFirst chain)]
          atomically $ writeTFetchRequestVar fetchClientRequestVar request
          say "set request"
          -- wait 'til the request is accepted
          atomically $ do
            inflight <- readTVar fetchClientInFlightVar
            check (peerFetchReqsInFlight inflight /= 0)
          say "request accepted"
          -- wait 'til the request is done
          atomically $ do
            inflight <- readTVar fetchClientInFlightVar
            check (peerFetchReqsInFlight inflight == 0)

          lookupBlock <- getTestFetchedBlocks blockHeap
          say $ "all isFetched: "
             ++ show (all (\b -> isJust (lookupBlock (blockPoint b)))
                          (ChainFragment.toOldestFirst chain))

          say "driver done"

    (clientChannel, serverChannel) <- createConnectedChannels
    let runServer = runPeer             codecBlockFetch serverChannel server
        runClient = runPipelinedPeer 10 codecBlockFetch clientChannel client
    serverAsync   <- async $ runServer
    clientAsync   <- async $ runClient
    observerAsync <- async $ observer
    driverAsync   <- async $ testDriver

    _ <- waitAnyCancel [serverAsync, clientAsync, observerAsync, driverAsync]
    return ()


demoBlockServer :: forall header block m.
                  (MonadSTM m, MonadTimer m, HasHeader block,
                   HeaderHash header ~ HeaderHash block)
                => ChainFragment block
                -> BlockFetchServer header block m ()
demoBlockServer chain =
    senderSide
  where
    senderSide :: BlockFetchServer header block m ()
    senderSide = BlockFetchServer receiveReq ()

    receiveReq :: ChainRange header
               -> m (BlockFetchBlockSender header block m ())
    receiveReq (ChainRange lpoint upoint) = do
      case ChainFragment.sliceRange chain
             (castPoint lpoint) (castPoint upoint) of
        Nothing     -> return $ SendMsgNoBlocks (return senderSide)
        Just chain' -> return $ SendMsgStartBatch (sendBlocks blocks)
          where blocks = ChainFragment.toOldestFirst chain'


    sendBlocks :: [block] -> m (BlockFetchSendBlocks header block m ())
    sendBlocks []     = return $ SendMsgBatchDone (return senderSide)
    sendBlocks (b:bs) = return $ SendMsgBlock b (sendBlocks bs)

