{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Ouroboros.Network.FetchLogic where

import           Data.Maybe
import           Data.Tuple (swap)
import           Data.Semigroup ((<>))
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import qualified Data.Dequeue as Q
import           Data.Void

import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadTimer
import           Control.Exception (assert)
import           System.Random (Random(..), StdGen)

import           Ouroboros.Network.Block
import           Ouroboros.Network.Chain (Chain, Point, castPoint, blockPoint)
import qualified Ouroboros.Network.Chain as Chain
import           Ouroboros.Network.ChainFragment (ChainFragment)
import qualified Ouroboros.Network.ChainFragment as ChainFragment

import           Ouroboros.Network.Protocol.BlockFetch.Server
--import           Ouroboros.Network.Protocol.BlockFetch.Client
import           Ouroboros.Network.Protocol.BlockFetch.Type (ChainRange(..))

--import           Ouroboros.Network.Testing.ConcreteBlock
--import           Test.QuickCheck
--import           Test.Chain


{-
Let's start with the big picture...

┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄
Key:  ┏━━━━━━━━━━━━┓  ╔═════════════╗  ┏━━━━━━━━━━━━━━┓   ╔════════════╗
      ┃ STM-based  ┃  ║active thread║  ┃state instance┃┓  ║ one thread ║╗
      ┃shared state┃  ║             ║  ┃   per peer   ┃┃  ║  per peer  ║║
      ┗━━━━━━━━━━━━┛  ╚═════════════╝  ┗━━━━━━━━━━━━━━┛┃  ╚════════════╝║
                                        ┗━━━━━━━━━━━━━━┛   ╚════════════╝
┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄
Notes:
 • Thread communication is via STM based state.
 • Outbound: threads update STM state.
 • Inbound: threads wait on STM state changing (using retry).
 • These are no queues: there is only the current state, not all change events.
┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄
  ╔═════════════╗     ┏━━━━━━━━━━━━━┓
  ║ Chain sync  ║╗    ┃   Ledger    ┃
  ║  protocol   ║║◀───┨   state     ┃◀───────────╮
  ║(client side)║║    ┃             ┃            │
  ╚══════╤══════╝║    ┗━━━━━━━━━━━━━┛            │
   ╚═════╪═══════╝                               │
         ▼                                       │
  ┏━━━━━━━━━━━━━┓     ┏━━━━━━━━━━━━━┓     ╔══════╧══════╗
  ┃  Candidate  ┃     ┃  Candidate  ┃     ║  Chain and  ║
  ┃  chains     ┃     ┃   chains    ┠────▶║   ledger    ║
  ┃  (headers)  ┃     ┃  (blocks)   ┃     ║  validation ║
  ┗━━━━━━┯━━━━━━┛     ┗━━━━━━━━━━━━━┛     ╚══════╤══════╝
         │                      ▲                │
         ╰─────────────────╮    │                │
░░░░░░░░░░░░░░░░░░░░░░░░░░░▼░░░░│░░░░░░          ▼
░░┏━━━━━━━━━━━━━┓░░░░░╔═════════╧═══╗░░   ┏━━━━━━━━━━━━━┓     ╔═════════════╗
░░┃    Block    ┃┓░░░░║    Block    ║░░   ┃   Current   ┃     ║ Block fetch ║╗
░░┃    fetch    ┃┃◀───╢    fetch    ║◀────┨    chain    ┠────▶║ protocol    ║║
░░┃   requests  ┃┃░░░░║    logic    ║░░   ┃  (blocks)   ┃     ║(server side)║║
░░┗━━━━━━┯━━━━━━┛┃░░░░╚═════════════╝░░   ┠─────────────┨     ╚═════════════╝║
░░░┗━━━━━┿━━━━━━━┛░░░░░░░░░░░▲░░░░░░░░░   ┃  Tentative  ┃      ╚═════════════╝
░░░░░░░░░▼░░░░░░░░░░         │            ┃    chain    ┠──╮
░░╔═════════════╗░░░  ┏━━━━━━┷━━━━━━┓     ┃  (headers)  ┃  │  ╔═════════════╗
░░║ block fetch ║╗░░  ┃    Set of   ┃     ┗━━━━━━━━━━━━━┛  │  ║ Chain sync  ║╗
░░║  protocol   ╟╫───▶┃  downloaded ┃                      ╰─▶║ protocol    ║║
░░║(client side)║║░░  ┃    blocks   ┃                         ║(server side)║║
░░╚═════════════╝║░░  ┗━━━━━━━━━━━━━┛                         ╚═════════════╝║
░░░╚═════════════╝░░                                           ╚═════════════╝
░░░░░░░░░░░░░░░░░░░░
┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄

We will consider the block fetch logic and the policy (but not mechanism)
for the block fetch protocol client together as one unit of functionality.
This is the shaded area in the diagram above.

Looking at the diagram we see that these two threads interact with each other
and other threads via the following shared state

 ═════════════════════════════╤════════════════╤═════════════════
   State                      │  Interactions  │  {In,Ex}ternal
 ─────────────────────────────┼────────────────┼─────────────────
   Candidate chains (headers) │  Read          │  External
   Current chain (blocks)     │  Read          │  External
   Set of downloaded blocks   │  Read & Write  │  External
   Block fetch requests       │  Read & Write  │  Internal

The block fetch requests state is private between the block fetch logic
and the block fetch protocol client, so it is implemented here.

The other state is managed by the consensus layer and is considered external
here. So here we define interfaces for interacting with the external state.
These have to be provided when instantiating the block fetch logic.
-}

data FetchLogicExternalState peer header block m =
     FetchLogicExternalState {

       -- | Read the K-suffixes of the candidate chains.
       --
       -- They must be already validated and contain the last @K@ headers
       -- (unless we're near the chain genesis of course).
       --
       readCandidateChains    :: Tr m (Map peer (ChainFragment header)),

       -- | Read the K-suffix of the current chain.
       --
       -- This must contain info on the last @K@ blocks (unless we're near
       -- the chain genesis of course).
       --
       readCurrentChain       :: Tr m (ChainFragment block),

       readFetchedBlocks      :: Tr m (Point block -> Bool)

       --TODO: adding and then registering blocks

       -- blockBodySize      :: header -> Word
       -- plausibleCandidate :: ChainFragment block -> ChainFragment header -> Bool
       -- compareCandidates  :: ChainFragment header -> ChainFragment header -> Ordering
     }

type FetchClientRegistry m = m ()

-- | Execute the block fetch logic. It monitors the current chain and candidate
-- chains. It decided which block bodies to fetch and manages the process of
-- fetching them, including making alternative decisions based on timeouts and
-- failures.
--
-- This runs forever and should be shutdown using mechanisms such as async.
--
fetchLogic :: forall peer header block m.
              (MonadSTM m, Ord peer,
               HasHeader header, HasHeader block,
               HeaderHash header ~ HeaderHash block,
               -- extra debug constraints:
               MonadSay m, Show peer, Show header, Show block)
           => FetchLogicExternalState peer header block m
           -> FetchClientRegistry m
           -> m Void
fetchLogic FetchLogicExternalState{..} clientRegistry = do
    peerStates  <- newTVarM Map.empty
    peerDeltaQs <- newTVarM Map.empty

    fetchLogicIterations
      fetchPolicyParams
      FetchTriggerVariables {
        readStateCurrentChain    = readCurrentChain,
        readStateCandidateChains = readCandidateChains,
        readStatePeerStates      = readTVar peerStates,
        readStateFetchedBlocks   = readFetchedBlocks
      }
      FetchNonTriggerVariables {
        readStatePeerDeltaQs = readTVar peerDeltaQs
      }
--      clientRegistry
  where
    -- For now, use a fixed policy.
    -- It's unclear for the moment if this will be fixed external config or
    -- if the in-fligh per-peer needs to be adaptive.
    fetchPolicyParams = FetchPolicyParams {
        maxInFlightBytesPerPeer  = 256 * 1024,
        maxConcurrentFetchPeers  = 1
      }



fetchLogicIterations :: (MonadSTM m, Ord peer,
                         HasHeader header, HasHeader block,
                         HeaderHash header ~ HeaderHash block)
                     => FetchPolicyParams
                     -> FetchTriggerVariables peer header block m
                     -> FetchNonTriggerVariables peer time m
                     -> m Void
fetchLogicIterations fetchPolicyParams
                     fetchTriggerVariables
                     fetchNonTriggerVariables =

    iterateForever initialFetchStateFingerprint $ \fingerprint -> do

      -- Run a single iteration of the fetch logic:
      --
      -- * wait for the state to change and make decisions for the new state
      -- * act on those decisions

      (fingerprint', decisions)
        <- fetchLogicIterationDecide
             fetchPolicyParams
             fetchTriggerVariables
             fetchNonTriggerVariables
             fingerprint

      -- Log the fetch decisions
      --TODO use Trace mechanism

      fetchLogicIterationAct decisions

      return fingerprint'


iterateForever :: Monad m => a -> (a -> m a) -> m Void
iterateForever x0 m = go x0 where go x = m x >>= go


-- | The first half of a single iteration of the fetch logic.
--
-- This involves:
--
-- * waiting for the state that the fetch decisions depend upon to change;
-- * taking a snapshot of the state;
-- * deciding for each peer if we will initiate a new fetch request
--
fetchLogicIterationDecide
  :: (MonadSTM m, Ord peer,
      HasHeader header, HasHeader block,
      HeaderHash header ~ HeaderHash block)
  => FetchPolicyParams
  -> FetchTriggerVariables peer header block m
  -> FetchNonTriggerVariables peer time m
  -> FetchStateFingerprint peer header
  -> m (FetchStateFingerprint peer header, [FetchDecision header peer])
fetchLogicIterationDecide fetchPolicyParams
                          fetchTriggerVariables
                          fetchNonTriggerVariables
                          fetchStateFingerprint = do

    -- Gather a snapshot of all the state we need.
    (fetchStateSnapshot,
     fetchStateFingerprint') <- atomically $
                                readStateVariables
                                  fetchTriggerVariables
                                  fetchNonTriggerVariables
                                  fetchStateFingerprint

    assert (fetchStateFingerprint' /= fetchStateFingerprint) $ return ()

    -- Make all the fetch decisions
    let fetchDecisions = fetchDecisionsForState
                           fetchPolicyParams
                           fetchStateFetchedBlocks
                           fetchStatePeerStates
                           fetchStatePeerDeltaQs
                           fetchStateCurrentChain
                           (map swap (Map.toList fetchStatePeerChains))
          where
            FetchStateSnapshot {
              fetchStateCurrentChain,
              fetchStatePeerChains,
              fetchStateFetchedBlocks,
              fetchStatePeerStates,
              fetchStatePeerDeltaQs
            } = fetchStateSnapshot

    return (fetchStateFingerprint', fetchDecisions)

-- | STM actions to read various state variables that the fetch logic depends
-- upon. Any change in these variables is a trigger to re-evaluate the decision
-- on what blocks to fetch.
--
-- Note that this is a \"level trigger\" not an \"edge trigger\": we do not
-- have to re-evaluate on every change, it is sufficient to re-evaluate at some
-- stage after one or more changes. This means it is ok to get somewhat behind,
-- and it is not necessary to determine exactly what changed, just that there
-- was some change.
--
data FetchTriggerVariables peer header block m = FetchTriggerVariables {
       readStateCurrentChain    :: Tr m (ChainFragment block),
       readStateCandidateChains :: Tr m (Map peer (ChainFragment header)),
       readStatePeerStates      :: Tr m (Map peer PeerFetchState),
       readStateFetchedBlocks   :: Tr m (Point block -> Bool)
     }

-- | STM actions to read various state variables that the fetch logic uses.
-- While the decisions do make use of the values of these variables, it is not
-- necessary to re-evaluate when these variables change.
--
data FetchNonTriggerVariables peer time m = FetchNonTriggerVariables {
       readStatePeerDeltaQs :: Tr m (Map peer (GSV time))
     }

data FetchStateSnapshot peer header block time = FetchStateSnapshot {
       fetchStateCurrentChain  :: ChainFragment block,
       fetchStatePeerChains    :: Map peer (ChainFragment header),
       fetchStateFetchedBlocks :: Point block -> Bool,
       fetchStatePeerStates    :: Map peer PeerFetchState,
       fetchStatePeerDeltaQs   :: Map peer (GSV time)
     }

data FetchStateFingerprint peer header =
     FetchStateFingerprint
       (Maybe BlockNo)
       (Map peer (Maybe (Point header)))
       (Map peer PeerFetchStatus) -- note, we don't include the full PeerFetchState
       --TODO: trigger for the fetched block set
  deriving Eq

initialFetchStateFingerprint :: FetchStateFingerprint peer header
initialFetchStateFingerprint =
    FetchStateFingerprint
      Nothing
      Map.empty
      Map.empty


readStateVariables :: (MonadSTM m, Eq peer, Eq (Point header),
                       HasHeader header, HasHeader block)
                   => FetchTriggerVariables peer header block m
                   -> FetchNonTriggerVariables peer time m
                   -> FetchStateFingerprint peer header
                   -> Tr m (FetchStateSnapshot peer header block time,
                            FetchStateFingerprint peer header)
readStateVariables FetchTriggerVariables{..}
                        FetchNonTriggerVariables{..}
                        fetchStateFingerprint = do

    -- Read all the trigger state variables
    fetchStateCurrentChain  <- readStateCurrentChain
    fetchStatePeerChains    <- readStateCandidateChains
    fetchStatePeerStates    <- readStatePeerStates
    fetchStateFetchedBlocks <- readStateFetchedBlocks

    -- Construct the change detection fingerprint
    let fetchStateFingerprint' =
          FetchStateFingerprint
            (ChainFragment.headBlockNo fetchStateCurrentChain)
            (Map.map ChainFragment.headPoint fetchStatePeerChains)
            (Map.map peerFetchStatus fetchStatePeerStates)

    -- Check the fingerprint changed, or block and wait until it does
    check (fetchStateFingerprint' /= fetchStateFingerprint)

    -- Now read all the non-trigger state variables
    fetchStatePeerDeltaQs <- readStatePeerDeltaQs

    -- Construct the overall snapshot of the state
    let fetchStateSnapshot = FetchStateSnapshot {
          fetchStateCurrentChain,
          fetchStatePeerChains,
          fetchStateFetchedBlocks,
          fetchStatePeerStates,
          fetchStatePeerDeltaQs
        }

    return (fetchStateSnapshot, fetchStateFingerprint')


fetchLogicIterationAct :: Monad m => [FetchDecision header peer] -> m ()
fetchLogicIterationAct _ = return ()

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
                            => Maybe BlockNo
                            -> [(ChainFragment header, peer)]
                            -> [(ChainFragment header, peer)]
filterLongerCandidateChains currentBlockNo =
    filter (\(c, _) -> ChainFragment.headBlockNo c > currentBlockNo)

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
type ChainSuffix header = ChainFragment header

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
                -> Maybe (ChainSuffix header)
chainForkSuffix current candidate =
    case ChainFragment.intersectChainFragments current candidate of
      Nothing                         -> Nothing
      Just (_, _, _, candidateSuffix) -> Just candidateSuffix

chainsForkSuffix :: (HasHeader header, HasHeader block,
                     HeaderHash header ~ HeaderHash block)
                 => ChainFragment block
                 -> [(ChainFragment header, peer)]
                 -> [(ChainSuffix   header, peer)]
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

-- | Find the fetch suffix range of blocks that have not been fetched.
--
-- We rely on the invariant that the set of already downloaded blocks are
-- downward closed.
--
chainFetchSuffix :: forall header block.
                    (HasHeader header, HeaderHash header ~ HeaderHash block)
                 => (Point block -> Bool)
                 -> ChainSuffix header
                 -> ChainSuffix header
chainFetchSuffix alreadyDownloaded =
    ChainFragment.takeWhileNewest (alreadyDownloaded . castPoint . blockPoint)

chainsFetchSuffix :: (HasHeader header, HeaderHash header ~ HeaderHash block)
                  => (Point block -> Bool)
                  -> [(ChainSuffix header, peer)]
                  -> [(ChainSuffix header, peer)]
chainsFetchSuffix alreadyDownloaded chains =
    [ (chainFetchSuffix alreadyDownloaded chainsuffix, peer)
    | (chainsuffix, peer) <- chains ]

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
                     => Map peer PeerFetchState
                     -> Map peer (GSV time)
                     -> [(ChainSuffix header, peer)]
                     -> [(ChainSuffix header, peer)]
prioritisePeerChains _peersStatus _peerDeltaQs = id

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


type FetchDecision header peer = Either (FetchDecline header peer)
                                        (FetchRequest header peer)

data FetchRequest header peer = FetchRequest !peer !(ChainRange header)
  deriving Show


chainSuffixRange :: HasHeader a => ChainFragment a -> Maybe (ChainRange a)
chainSuffixRange c =
    case (ChainFragment.last c, ChainFragment.head c) of
      (Just bl, Just bh) -> Just (ChainRange (blockPoint bl) (blockPoint bh))
      _                  -> Nothing

-- | A range on a chain identified by two points. It is exclusive on the
-- lower end and inclusive on the upper end.
--
--data ChainRange header = ChainRange !(Point header) !(Point header)
--  deriving Show

data FetchDecline header peer =
     FetchDeclineAllDownloaded  peer
   | FetchDeclineSlowPeer       peer
   | FetchDeclinePeerFailure    peer
   | FetchDeclineInFlightLimit  peer Word
   | FetchDeclineConcLimit      peer Word
  deriving Show

data FetchPolicyParams = FetchPolicyParams {
       maxInFlightBytesPerPeer  :: Word,
       maxConcurrentFetchPeers  :: Word
     }

data PeerFetchState = PeerFetchState {
       -- | The number of block fetch requests that are currently in-flight.
       -- This is the number of /requests/ not the number of blocks. Each
       -- request is for a range of blocks.
       --
       -- We track this because there is a fixed maximum number of outstanding
       -- requests that the protocol allows.
       --
       peerFetchReqsInFlight :: Word,

       -- | The sum of the byte count of blocks expected from all in-flight
       -- fetch requests. This is a close approximation of the amount of data
       -- we expect to receive, assuming no failures.
       --
       -- We track this because we pipeline fetch requests and we want to keep
       -- some but not too much data in flight at once.
       --
       peerFetchBytesInFlight :: Word,

       -- | The current status of communication with this peer. If the peer
       -- fetch responses hit a soft timeout then it enters an aberrant state.
       -- On a hard failure it is in the failure state, and it should be
       -- removed as a candidate shortly thereafter.
       --
       peerFetchStatus :: PeerFetchStatus
     }

data PeerFetchStatus = PeerFetchStatusNormal
                     | PeerFetchStatusAberrant
                     | PeerFetchStatusFailure
  deriving (Eq, Show)

fetchRequestDecisions :: (HasHeader header, Ord peer)
                      => FetchPolicyParams
                      -> Map peer PeerFetchState
                      -> [(ChainSuffix header, peer)]
                      -> [FetchDecision header peer]
fetchRequestDecisions fetchPolicyParams peerFetchStates =
    go nConcurrentFetchPeers0 []
  where
    go _nConcurrentFetchPeers decisions [] = decisions
    go  nConcurrentFetchPeers decisions ((chain, peer) : cps) =

      let decision = fetchRequestDecision
                       fetchPolicyParams
                       nConcurrentFetchPeers
                       peerFetchState
                       chain
                       peer

          peerFetchState = peerFetchStates Map.! peer

          nConcurrentFetchPeers'
            -- increment if it was idle, and now will not be
            | peerFetchBytesInFlight peerFetchState == 0
            , Right{} <- decision = nConcurrentFetchPeers + 1
            | otherwise           = nConcurrentFetchPeers

      in go nConcurrentFetchPeers' (decision:decisions) cps

    nConcurrentFetchPeers0 =
      fromIntegral $ Prelude.length
        [ () | s <- Map.elems peerFetchStates
             , peerFetchReqsInFlight s > 0 ]


fetchRequestDecision :: HasHeader header
                     => FetchPolicyParams
                     -> Word
                     -> PeerFetchState
                     -> ChainSuffix header
                     -> peer
                     -> FetchDecision header peer
fetchRequestDecision FetchPolicyParams {
                       maxInFlightBytesPerPeer,
                       maxConcurrentFetchPeers
                     }
                     nConcurrentFetchPeers
                     PeerFetchState{
                       peerFetchBytesInFlight,
                       peerFetchStatus
                     }
                     fetchSuffix peer
  | ChainFragment.null fetchSuffix
  = Left (FetchDeclineAllDownloaded peer)

  | PeerFetchStatusAberrant <- peerFetchStatus
  = Left (FetchDeclineSlowPeer peer)

  | PeerFetchStatusFailure <- peerFetchStatus
  = Left (FetchDeclinePeerFailure peer)

  | peerFetchBytesInFlight >= maxInFlightBytesPerPeer
  = Left (FetchDeclineInFlightLimit peer maxInFlightBytesPerPeer)

  | peerFetchBytesInFlight == 0
  , nConcurrentFetchPeers >= maxConcurrentFetchPeers
  = Left (FetchDeclineConcLimit peer maxConcurrentFetchPeers)

  | otherwise
  = Right (FetchRequest peer fetchReqRange)
  where
    --TODO: limit the request range to fit within maxInFlightBytesPerPeer
    --FIXME: separate empty/non-empty cases better:
    Just fetchReqRange = chainSuffixRange fetchSuffix
                     -- point prior to first block in fetch suffix
                     -- head point of fetch suffix
                     -- (blockPrevHash fetchSuffix) (ChainFragment.headPoint chain)


fetchDecisionsForState :: (HasHeader header, HasHeader block,
                           HeaderHash header ~ HeaderHash block, Ord peer)
                       => FetchPolicyParams
                       -> (Point block -> Bool)
                       -> Map peer PeerFetchState
                       -> Map peer (GSV time)
                       -> ChainFragment block
                       -> [(ChainFragment header, peer)]
                       -> [FetchDecision header peer]
fetchDecisionsForState fetchPolicyParams alreadyDownloaded
                       peersStatus peerDeltaQs
                       currentChain =
    fetchRequestDecisions fetchPolicyParams peersStatus
  . prioritisePeerChains peersStatus peerDeltaQs
  . chainsFetchSuffix alreadyDownloaded
  . chainsForkSuffix currentChain
  . filterLongerCandidateChains (ChainFragment.headBlockNo currentChain)


data FetchedBlockHeap m block = FetchedBlockHeap {
       getFetchedBlockLookup :: m (Point block -> Maybe block),
       addFetchedBlock       :: block -> m ()
     }

mkTestFetchedBlockHeap :: (MonadSTM m, HasHeader block)
                       => m (FetchedBlockHeap m block)
mkTestFetchedBlockHeap = do
    bhvar <- atomically (newTVar Map.empty)
    return FetchedBlockHeap {
      getFetchedBlockLookup = flip Map.lookup <$> atomically (readTVar bhvar),
      addFetchedBlock       = \b -> atomically $
                              modifyTVar' bhvar $ Map.insert (blockPoint b) b
    }

{-
demo1 :: Chain Block -> Chain BlockHeader -> Chain BlockHeader
      -> IO (Bool, [FetchDecision BlockHeader String])
demo1 currentChain candidateChain1 candidateChain2 = do
    currentChainVar    <- newTVarM currentChain
    fetchedBlocksVar   <- newTVarM Set.empty
    candidateChain1Var <- newTVarM candidateChain1
    candidateChain2Var <- newTVarM candidateChain2
    let candidateChainVars =
          Map.fromList [("peer1", candidateChain1Var)
                       ,("peer2", candidateChain2Var)]
        peerStates =
          Map.fromList [("peer1", PeerFetchState 0 0 PeerFetchStatusNormal)
                       ,("peer2", PeerFetchState 0 0 PeerFetchStatusNormal)]
        peerDeltaQs =
          Map.fromList [("peer1", GSV 20000 0 (Distribution 0))
                       ,("peer2", GSV 15000 0 (Distribution 0))]

    let fetchTriggerVariables :: FetchTriggerVariables String BlockHeader Block IO
        fetchTriggerVariables = FetchTriggerVariables {
          readStateCurrentChain    = readTVar currentChainVar,
          readStateCandidateChains = traverse readTVar candidateChainVars,
          readStatePeerStates      = return peerStates,
          readStateFetchedBlocks   = flip Set.member <$> readTVar fetchedBlocksVar
        }
        fetchNonTriggerVariables :: FetchNonTriggerVariables String Int IO
        fetchNonTriggerVariables = FetchNonTriggerVariables {
          readStatePeerDeltaQs = return peerDeltaQs
        }
        fetchStateFingerprint = initialFetchStateFingerprint

    (fetchStateFingerprint', fetchDecisions) <-
      fetchLogicIterationDecide
        fetchPolicyParams
        fetchTriggerVariables
        fetchNonTriggerVariables
        fetchStateFingerprint

    return (fetchStateFingerprint' /= fetchStateFingerprint, fetchDecisions)
  where
    fetchPolicyParams = FetchPolicyParams {
        maxInFlightBytesPerPeer  = 256 * 1024,
        maxConcurrentFetchPeers  = 1
      }
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
                       -> Tr m ()
registerCandidateChain (CandidateChains chainsVar) peerid chainVar =
    modifyTVar' chainsVar (Map.insert peerid chainVar)

unregisterCandidateChain :: (MonadSTM m, Ord peer)
                         => CandidateChains m peer header
                         -> peer
                         -> Tr m ()
unregisterCandidateChain (CandidateChains chainsVar) peerid =
    modifyTVar' chainsVar (Map.delete peerid)

readCandidateChains' :: MonadSTM m
                    => CandidateChains m peer header
                    -> Tr m (Map peer (ChainFragment header))
readCandidateChains' (CandidateChains chainsVar) =
    traverse readTVar =<< readTVar chainsVar

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






{-
data FetchRequestMsg  block = FetchRequestMsg (ChainRange block)
data FetchResponseMsg block = FetchResponseMsgBlock block
                            | FetchResponseMsgDone
                            | FetchResponseMsgFail


blockServer :: forall m block.
               (MonadTimer m, MonadSTM m)
            => StdGen
            -> Chain block
            -> TMVar m (FetchRequestMsg block)
            -> TMVar m (FetchResponseMsg block)
            -> m ()
blockServer prng0 chain inQ outQ =
    atomically (newTVar prng0) >>= forever . serve
  where
    serve prngVar = do
      FetchRequestMsg range <- recvMsg
      threadDelay . fromIntegral =<< randomUniform prngVar (1000, 100000)
      case Chain.selectBlockRange chain range of
        Just blocks -> do mapM_ (sendMsg . FetchResponseMsgBlock) blocks
                          sendMsg FetchResponseMsgDone
        Nothing     -> sendMsg FetchResponseMsgFail

    recvMsg :: m (FetchRequestMsg block)
    recvMsg = atomically $ takeTMVar inQ

    sendMsg :: FetchResponseMsg block -> m ()
    sendMsg = atomically . putTMVar outQ

    randomUniform :: TVar m StdGen -> (Int, Int) -> m Int
    randomUniform prngVar range = atomically $ do
      prng <- readTVar prngVar
      let (wait, prng') = randomR range prng
      writeTVar prngVar prng'
      return wait
-}


data Distribution n = Distribution n
-- This is a totally bogus representation. It's just a PoC.
-- This says that there's a single value that it takes with probability 1.

instance Num n => Semigroup (Distribution n) where
  (<>) = convolveDistribution

convolveDistribution :: Num n => Distribution n -> Distribution n -> Distribution n
convolveDistribution (Distribution d) (Distribution d') = Distribution (d+d')
-- Again, totally bogus.

shiftDistribution :: Num n => n -> Distribution n -> Distribution n
shiftDistribution n (Distribution d) = Distribution (n+d)
-- Again, totally bogus.

data GSV t = GSV (Duration t)                -- G as seconds
                 (Duration t)                -- S as seconds / octet
                 (Distribution (Duration t)) -- V as distribution

instance TimeMeasure t => Semigroup (GSV t) where
  GSV g1 s1 v1 <> GSV g2 s2 v2 = GSV (g1+g2) (s1+s2) (v1 <> v2)

newtype DeltaQ t = DeltaQ (Distribution (Duration t))

deriving instance TimeMeasure t => Semigroup (DeltaQ t)

type Size = Word

gsvLeadingEdgeArrive  :: TimeMeasure t => GSV t ->         DeltaQ t
gsvTrailingEdgeDepart :: TimeMeasure t => GSV t -> Size -> DeltaQ t  -- perhaps a bit dubious
gsvTrailingEdgeArrive :: TimeMeasure t => GSV t -> Size -> DeltaQ t

gsvLeadingEdgeArrive (GSV g _s v) =
  DeltaQ (shiftDistribution g v)

gsvTrailingEdgeDepart (GSV _g s v) bytes =
  DeltaQ (shiftDistribution (s * fromIntegral bytes) v)

gsvTrailingEdgeArrive (GSV g s v) bytes =
  DeltaQ (shiftDistribution (g + s * fromIntegral bytes) v)


estimateDetltaQ99thPercentile :: DeltaQ t -> Duration t
estimateDetltaQ99thPercentile (DeltaQ (Distribution t)) = t
-- Again, totally bogus.

estimateProbabilityMassBeforeDeadline :: TimeMeasure t
                                      => DeltaQ t -> Duration t -> Double
estimateProbabilityMassBeforeDeadline (DeltaQ (Distribution t)) d
  | t < d     = 1
  | otherwise = 0
  -- Again, totally bogus.


{-
blockArrivalShedule :: TimeMeasure time
                    => (block -> Size)
                    -> GSV
                    -> time
                    -> [block]
                    -> [(Point block, time)]
blockArrivalShedule blockSize deltaq now blocks =
    [ (blockPoint b, eta)
    | b <- blocks
    , let eta = estimateETA deltaq (blockSize b) `addTime` now
    ]
-}

{-
submitFetchReqs :: ChainRange block
                -> [(Point block, time)]
                -> FetchTrackingState
                -> TMVar m (FetchRequestMsg block)
                -> m ()
submitFetchReqs range etaschedule FetchTrackingState{..} outQ =
    atomically $ do
      putTMVar outQ (FetchRequestMsg range)
      mapM_ (writeTBQueue blocksInFlight) etaschedule
      modifyTVar' bytesInFlight (+ sum (map (blockSize . fst) etaschedule))
-}


--  If I were to send this request now, when would the leading and trailing
-- edges of the response come back?
--
-- The leading edge is useful for setting a timeout to give us early indication
-- that we're highly likely to miss our response time. The trailing edge is
-- the time we are ultimately interested in, and a timeout on that ensures we
-- are not waiting too long before re-evaluating our decisions.
--
    -- Obviously the response times depend not just on the outbound and inbound
    -- Delta Qs, and the request and response sizes, but it also depends on
    -- the requests that the remote end is already processing.
    --
    -- We could keep track of the detail of the queue on the remote side, but
    -- this quickly gets complicated. Instead we make the following simplifying
    -- and conservative assumption. We only keep track of the amount of data
    -- we have requested that we have not yet received, and we pretend that we
    -- are now asking for that plus the new data we're really asking for.
    --
    -- This over-estimates the response time, but by at most one round trip.
    -- To see that this is the case, consider the most extreme case of response
    -- data that it just arriving at the receiver. If instead the leading edge
    -- is back at the initial request, that is a full round trip away. Note
    -- that to limit this to one round trip we need to update the in-flight
    -- data as it arrives, not just at the trailing edge of block messages.

data FetchTrackingState header time = FetchTrackingState {
       fetchRequestsInFlight      :: Queue (FetchRequestBatch header time),
       fetchRequestsBytesInFlight :: Size
     }

type Queue = Q.BankersDequeue

data FetchRequestBatch header time =
     FetchRequestBatch
       !(ChainRange header)         -- the requested range
       !time                        -- leading edge timeout
       [(BlockInfo header, time)]   -- blocks and trailing edge timeouts

data BlockInfo header = BlockInfo !(Point header) !Size



estimateBlockFetchResponse :: TimeMeasure time
                           => GSV time
                           -> GSV time
                           -> FetchTrackingState header time
                           -> [BlockInfo header]
                           -> Duration time
estimateBlockFetchResponse outboundGSV inboundGSV
                           FetchTrackingState{fetchRequestsBytesInFlight}
                           blocks =
    estimateDetltaQ99thPercentile $
         gsvTrailingEdgeArrive outboundGSV reqSize
      <> gsvTrailingEdgeArrive inboundGSV  rspSize
  where
    reqSize = 100 -- not exact, but it's small
    rspSize = fetchRequestsBytesInFlight
            + sum [ size | BlockInfo _ size <- blocks ]

blockArrivalShedule :: TimeMeasure time
                    => GSV time
                    -> GSV time
                    -> FetchTrackingState header time
                    -> [BlockInfo header]
                    -> (Duration time, [Duration time])
blockArrivalShedule outboundGSV inboundGSV
                    FetchTrackingState{fetchRequestsBytesInFlight}
                    blocks =
    ( batchLeadingEdgeArrival, blockTrailingEdgesArrival )
  where
    reqSize = 100 -- not exact, but it's small

    batchLeadingEdgeArrival =
        estimateDetltaQ99thPercentile $
             gsvTrailingEdgeArrive outboundGSV reqSize
          <> gsvTrailingEdgeArrive inboundGSV  fetchRequestsBytesInFlight

    blockTrailingEdgesArrival =
      [ estimateDetltaQ99thPercentile $
             gsvTrailingEdgeArrive outboundGSV reqSize
          <> gsvTrailingEdgeArrive inboundGSV  rspSize
      | rspSize <- cumulativeSumFrom fetchRequestsBytesInFlight
                                     [ size | BlockInfo _ size <- blocks ]
      ]
{-
    batchTrailingEdgeDeparture =
        estimateDetltaQ99thPercentile $
             gsvTrailingEdgeArrive outboundGSV reqSize
          <> gsvTrailingEdgeDepart inboundGSV  rspSize
      where
        rspSize = fetchRequestsBytesInFlight + sum (map snd reqblocks)
-}

    cumulativeSumFrom n = tail . scanl (+) n

newFetchRequestBatch :: TimeMeasure time
                     => time
                     -> GSV time
                     -> GSV time
                     -> FetchTrackingState header time
                     -> ChainRange header
                     -> [BlockInfo header]
                     -> FetchRequestBatch header time
newFetchRequestBatch now outboundGSV inboundGSV fetchTrackingState fetchRange fetchBlocks =
    FetchRequestBatch
      fetchRange
      (addTime leadingEdge now)
      [ (block, addTime trailingEdge now)
      | (block, trailingEdge) <- zip fetchBlocks blockTrailingEdges ]
  where
    (leadingEdge, blockTrailingEdges) =
      blockArrivalShedule outboundGSV inboundGSV
                          fetchTrackingState
                          fetchBlocks

{-
updateForNewRequest :: DeltaQ
                    -> DeltaQ
                    -> time
                    -> BlockInfo
                    -> FetchTrackingState
                    -> FetchTrackingState
updateForNewRequest outboundDeltaQ inboundDeltaQ now RemoteIdle =
    RemoteActive {
      _ = 
      _ =
      _ = Q.empty
    }
  where
    -- Point in time where our request would arrive on the remote side.
    est1 = estimateTrailingEdgeArrive outboundDeltaQ 100

    -- Point in time where the response would 
    est2 = estimateTrailingEdgeArrive inboundDeltaQ (blockSize b)

    est3 = estimateTrailingEdgeDepart inboundDeltaQ (blockSize b)


updateForNewRequest RemoteActive{..} =

updateForBlockArrival :: Point
                      -> FetchTrackingState
                      -> FetchTrackingState
-- 
-}

demoBlockServer :: forall header block m.
                  (MonadSTM m, MonadTimer m, HasHeader block,
                   HeaderHash header ~ HeaderHash block)
                => StdGen
                -> Chain block
                -> BlockFetchServer header block m ()
demoBlockServer prng0 chain =
    senderSide prng0
  where
    senderSide :: StdGen -> BlockFetchServer header block m ()
    senderSide prng = BlockFetchServer (receiveReq prng) ()

    receiveReq :: StdGen -> ChainRange header -> m (BlockFetchBlockSender header block m ())
    receiveReq prng (ChainRange lpoint upoint) = do
      let wait :: Int
          (wait, prng') = randomR (1000, 100000) prng
      threadDelay (fromIntegral wait)
      case Chain.selectBlockRange chain (castPoint lpoint) (castPoint upoint) of
        Nothing     -> return $ SendMsgNoBlocks (return (senderSide prng'))
        Just blocks -> return $ SendMsgStartBatch (sendBlocks prng' blocks)


    sendBlocks :: StdGen -> [block] -> m (BlockFetchSendBlocks header block m ())
    sendBlocks prng []     = return $ SendMsgBatchDone (return (senderSide prng))
    sendBlocks prng (b:bs) = return $ SendMsgBlock b (sendBlocks prng bs)

