{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE BangPatterns               #-}

module Ouroboros.Network.FetchLogic (
  fetchLogic,
  FetchClientRegistry,
  bracketFetchClient,
  blockFetchClient
  ) where

import           Data.Maybe
import           Data.Semigroup ((<>))
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import qualified Data.Set as Set
import           Data.Set (Set)
--import qualified Data.Dequeue as Q
import           Data.Void

import           Control.Monad (when, unless)
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadTimer
import           Control.Exception (assert)
import           System.Random (Random(..), StdGen)

import           Ouroboros.Network.Block
import           Ouroboros.Network.Chain (Chain, Point, castPoint, blockPoint)
import qualified Ouroboros.Network.Chain as Chain
import           Ouroboros.Network.ChainFragment (ChainFragment(..))
import qualified Ouroboros.Network.ChainFragment as ChainFragment

import           Ouroboros.Network.Protocol.BlockFetch.Server
--import           Ouroboros.Network.Protocol.BlockFetch.Client
import           Ouroboros.Network.Protocol.BlockFetch.Type
import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Pipelined

import           Ouroboros.Network.Testing.ConcreteBlock
--import           Test.QuickCheck
--import           Test.Chain
--import           Debug.Trace


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


       --TODO: need to add blocks
     }


-- | The consensus layer functionality that the fetch logic depends upon.
--
-- These are provided as input to the fetch logic by the consensus layer.
--
data ConsensusFunctions header block =
     ConsensusFunctions {

       -- | Given the current chain, is the given chain plausible as a
       -- candidate chain. Classically for Ouroboros this would simply
       -- check if the candidate is strictly longer, but for Ouroboros
       -- with operational key certificates there are also cases where
       -- we would consider a chain of equal length to the current chain.
       --
       plausibleCandidateChain :: ChainFragment block
                               -> ChainFragment header -> Bool,

       -- | Compare two candidate chains and return a preference ordering.
       -- This is used as part of selecting which chains to prioritise for
       -- downloading block bodies.
       --
       compareCandidateChains  :: ChainFragment header
                               -> ChainFragment header
                               -> Ordering,

       -- | Much of the logic for deciding which blocks to download from which
       -- peer depends on making estimates based on recent performance metrics.
       -- These estimates of course depend on the amount of data we will be
       -- downloading.
       --
       blockFetchSize          :: header -> Word,

       -- | Given a block header, validate the supposed corresponding block
       -- body.
       --
       validateBlockBody       :: header -> block -> Bool
     }


-- | A registry for the threads that are executing the client side of the
-- 'BlockFetch' protocol to communicate with our peers.
--
-- The registry contains the shared variables we use to communicate with these
-- threads, both to track their status and to provide instructions.
--
-- The threads add\/remove themselves to\/from this registry when they start up
-- and shut down.
--
newtype FetchClientRegistry peer header m =
        FetchClientRegistry (TVar m (Map peer (FetchClientStateVars header m)))


-- | A set of variables shared between the block fetch logic thread and each
-- thread executing the client side of the block fetch protocol. That is, these
-- are the shared variables per peer. The 'FetchClientRegistry' contains the
-- mapping of these for all peers.
--
-- The variables are used for communicating from the protocol thread to the
-- decision making thread the status of things with that peer. And in the other
-- direction one shared variable is for providing new fetch requests.
--
data FetchClientStateVars header m =
     FetchClientStateVars {

       -- | The current status of communication with the peer. It is written
       -- by the protocol thread and monitored and read by the decision logic
       -- thread. Changes in this state trigger re-evaluation of fetch
       -- decisions.
       --
       fetchClientStatusVar   :: TVar m PeerFetchStatus,

       -- | The current number of requests in-flight and the amount of data
       -- in-flight with the peer. It is written by the protocol thread and
       -- read by the decision logic thread. This is used in fetch decisions
       -- but changes here do not trigger re-evaluation of fetch decisions.
       --
       fetchClientInFlightVar :: TVar m (PeerFetchInFlight header),

       -- | The shared variable used to communicate fetch requests to the thread
       -- running the block fetch protocol. Fetch requests are posted by the
       -- decision logic thread. The protocol thread accepts the requests and
       -- acts on them, updating the in-flight stats. While this is a 'TMVar',
       -- it is not used as a one-place queue: the requests can be updated
       -- before being accepted.
       --
       fetchClientRequestVar  :: TFetchRequestVar m header
     }

-- | The status of the block fetch communication with a peer. This is maintained
-- by fetch protocol threads and used in the block fetch decision making logic.
-- Changes in this status trigger re-evaluation of fetch decisions.
--
data PeerFetchStatus =
       -- | Communication with the peer has failed. This is a temporary status
       -- that may occur during the process of shutting down the thread that
       -- runs the block fetch protocol. The peer will promptly be removed from
       -- the peer registry and so will not be considered at all.
       --
       PeerFetchStatusFailure

       -- | The peer is in a potentially-temporary state in which it has not
       -- responded to us within a certain expected time limit. This is not
       -- a hard protocol timeout where the whole connection will be abandoned,
       -- it is simply a reply that has taken longer than expected. This status
       -- is used to trigger re-evaluating which peer to ask for blocks from,
       -- so that we can swiftly ask other peers for blocks if one unexpectedly
       -- responds too slowly
       --
       -- Peers in this state may later return to normal states if communication
       -- resumes, or they may eventually hit a hard timeout and fail.
       --
     | PeerFetchStatusAberrant

       -- | Communication with the peer is in a normal state, and the peer is
       -- considered too busy to accept new requests. Changing from this state
       -- to the ready state is used to trigger re-evaluating fetch decisions
       -- and may eventually result in new fetch requests. This state is used
       -- as part of a policy to batch new requests: instead of switching to
       -- the ready state the moment there is tiny bit of capacity available,
       -- the state is changed once the capacity reaches a certain threshold.
       --
     | PeerFetchStatusBusy

       -- | Communication with the peer is in a normal state, and the peer is
       -- considered ready to accept new requests.
       --
     | PeerFetchStatusReady
  deriving (Eq, Ord, Show)


-- | The number of requests in-flight and the amount of data in-flight with a
-- peer. This is maintained by fetch protocol threads and used in the block
-- fetch decision making logic.
--
data PeerFetchInFlight header = PeerFetchInFlight {
       -- | The number of block fetch requests that are currently in-flight.
       -- This is the number of /requests/ not the number of blocks. Each
       -- request is for a range of blocks.
       --
       -- We track this because there is a fixed maximum number of outstanding
       -- requests that the protocol allows.
       --
       peerFetchReqsInFlight :: !Word,

       -- | The sum of the byte count of blocks expected from all in-flight
       -- fetch requests. This is a close approximation of the amount of data
       -- we expect to receive, assuming no failures.
       --
       -- We track this because we pipeline fetch requests and we want to keep
       -- some but not too much data in flight at once.
       --
       peerFetchBytesInFlight :: !Word,

       -- | The points for the set of blocks that are currently in-flight.
       -- Note that since requests are for ranges of blocks this does not
       -- correspond to the number of requests in flight.
       --
       -- We track this because as part of the decision for which blocks to
       -- fetch from which peers we take into account what blocks are already
       -- in-flight with peers.
       --
       peerFetchBlocksInFlight :: Set (Point header)
     }


bracketFetchClient :: (MonadThrow m, MonadSTM m, Ord peer)
                   => FetchClientRegistry peer header m
                   -> peer
                   -> (FetchClientStateVars header m -> m a)
                   -> m a
bracketFetchClient (FetchClientRegistry registry) peer =
    bracket register unregister
  where
    register = atomically $ do
      fetchClientInFlightVar <- newTVar PeerFetchInFlight {
                                          peerFetchReqsInFlight   = 0,
                                          peerFetchBytesInFlight  = 0,
                                          peerFetchBlocksInFlight = Set.empty
                                        }
      fetchClientStatusVar   <- newTVar PeerFetchStatusReady
      fetchClientRequestVar  <- newTFetchRequestVar
      let stateVars = FetchClientStateVars {
                        fetchClientStatusVar,
                        fetchClientInFlightVar,
                        fetchClientRequestVar
                      }
      modifyTVar' registry (Map.insert peer stateVars)
      return stateVars

    unregister _ =
      atomically $ modifyTVar' registry (Map.delete peer)


-- | Execute the block fetch logic. It monitors the current chain and candidate
-- chains. It decided which block bodies to fetch and manages the process of
-- fetching them, including making alternative decisions based on timeouts and
-- failures.
--
-- This runs forever and should be shut down using mechanisms such as async.
--
fetchLogic :: forall peer header block m.
              (MonadSTM m, Ord peer,
               HasHeader header, HasHeader block,
               HeaderHash header ~ HeaderHash block,
               -- extra debug constraints:
               MonadSay m, Show peer, Show header, Show block)
           => FetchLogicExternalState peer header block m
           -> ConsensusFunctions header block
           -> FetchClientRegistry peer header m
           -> m Void
fetchLogic FetchLogicExternalState{..}
           consensusFunctions
           (FetchClientRegistry registry) = do

    -- TODO: get this from elsewhere
    peerDeltaQs <- newTVarM Map.empty

    fetchLogicIterations
      consensusFunctions
      fetchPolicyParams
      FetchTriggerVariables {
        readStateCurrentChain    = readCurrentChain,
        readStateCandidateChains = readCandidateChains,
        readStatePeerStatus      = readPeerStatus
      }
      FetchNonTriggerVariables {
        readStateFetchedBlocks = readFetchedBlocks,
        readStatePeerStates    = readPeerStates,
        readStatePeerDeltaQs   = readTVar peerDeltaQs,
        readStatePeerReqVars   = readPeerReqVars
      }
  where
    readPeerStatus :: Tr m (Map peer PeerFetchStatus)
    readPeerStatus =
      readTVar registry >>= traverse (readTVar . fetchClientStatusVar)

    readPeerStates :: Tr m (Map peer (PeerFetchStatus, PeerFetchInFlight header))
    readPeerStates =
      readTVar registry >>=
      traverse (\s -> (,) <$> readTVar (fetchClientStatusVar s)
                          <*> readTVar (fetchClientInFlightVar s))

    readPeerReqVars :: Tr m (Map peer (TFetchRequestVar m header))
    readPeerReqVars =
      readTVar registry >>= return . Map.map fetchClientRequestVar


    -- For now, use a fixed policy.
    -- It's unclear for the moment if this will be fixed external config
    fetchPolicyParams = FetchPolicyParams {
        maxInFlightReqsPerPeer   = 10,
        maxConcurrentFetchPeers  = 1
      }



fetchLogicIterations :: (MonadSTM m, Ord peer,
                         HasHeader header, HasHeader block,
                         HeaderHash header ~ HeaderHash block)
                     => ConsensusFunctions header block
                     -> FetchPolicyParams
                     -> FetchTriggerVariables peer header block m
                     -> FetchNonTriggerVariables peer header block m
                     -> m Void
fetchLogicIterations consensusFunctions
                     fetchPolicyParams
                     fetchTriggerVariables
                     fetchNonTriggerVariables =

    iterateForever initialFetchStateFingerprint $ \stateFingerprint ->

      -- Run a single iteration of the fetch logic:
      --
      -- * wait for the state to change and make decisions for the new state
      -- * act on those decisions

      fetchLogicIteration
        consensusFunctions
        fetchPolicyParams
        fetchTriggerVariables
        fetchNonTriggerVariables
        stateFingerprint


iterateForever :: Monad m => a -> (a -> m a) -> m Void
iterateForever x0 m = go x0 where go x = m x >>= go


-- | A single iteration of the fetch logic.
--
-- This involves:
--
-- * waiting for the state that the fetch decisions depend upon to change;
-- * taking a snapshot of the state;
-- * deciding for each peer if we will initiate a new fetch request
--
fetchLogicIteration
  :: (MonadSTM m, Ord peer,
      HasHeader header, HasHeader block,
      HeaderHash header ~ HeaderHash block)
  => ConsensusFunctions header block
  -> FetchPolicyParams
  -> FetchTriggerVariables peer header block m
  -> FetchNonTriggerVariables peer header block m
  -> FetchStateFingerprint peer header block
  -> m (FetchStateFingerprint peer header block)
fetchLogicIteration consensusFunctions
                    fetchPolicyParams
                    fetchTriggerVariables
                    fetchNonTriggerVariables
                    stateFingerprint = do

    -- Gather a snapshot of all the state we need.
    (stateSnapshot, stateFingerprint') <-
      atomically $
        readStateVariables
          fetchTriggerVariables
          fetchNonTriggerVariables
          stateFingerprint

    -- TODO: allow for boring PeerFetchStatusBusy transitions where we go round
    -- again rather than re-evaluating everything.
    assert (stateFingerprint' /= stateFingerprint) $ return ()

    -- TODO: log the difference in the fingerprint that caused us to wake up

    -- Make all the fetch decisions
    let decisions = fetchDecisionsForStateSnapshot
                      consensusFunctions
                      fetchPolicyParams
                      stateSnapshot

    -- Log the fetch decisions
    --TODO use Trace mechanism

    let swizzleReqVar (d,(_,_,_,rq)) = (d,rq)
    fetchLogicIterationAct (map swizzleReqVar decisions)

    return stateFingerprint'

-- | Do a bit of rearranging of data before calling 'fetchDecisions' to do the
-- real work.
--
fetchDecisionsForStateSnapshot
  :: (HasHeader header, HasHeader block, Ord peer,
      HeaderHash header ~ HeaderHash block)
  => ConsensusFunctions header block
  -> FetchPolicyParams
  -> FetchStateSnapshot peer header block m
  -> [( FetchDecision header,
        PeerInfo header (Time m) (TFetchRequestVar m header)
      )]

fetchDecisionsForStateSnapshot
    consensusFunctions
    fetchPolicyParams
    FetchStateSnapshot {
      fetchStateCurrentChain,
      fetchStatePeerChains,
      fetchStatePeerStates,
      fetchStatePeerDeltaQs,
      fetchStatePeerReqVars,
      fetchStateFetchedBlocks
    } =
    assert (                 Map.keysSet fetchStatePeerChains
            `Set.isSubsetOf` Map.keysSet fetchStatePeerStates) $

    assert (Map.keysSet fetchStatePeerStates
         == Map.keysSet fetchStatePeerDeltaQs) $

    assert (Map.keysSet fetchStatePeerStates
         == Map.keysSet fetchStatePeerReqVars) $

    fetchDecisions
      consensusFunctions
      fetchPolicyParams
      fetchStateCurrentChain
      fetchStateFetchedBlocks
      peerChainsAndPeerInfo
  where
    peerChainsAndPeerInfo =
      Map.elems $
      Map.intersectionWith swizzle
        (Map.intersectionWith (,) fetchStatePeerChains  fetchStatePeerStates)
        (Map.intersectionWith (,) fetchStatePeerDeltaQs fetchStatePeerReqVars)

    swizzle (chain, (status, inflight)) (deltaq, reqvar) =
      (chain, (status, inflight, deltaq, reqvar))

type PeerInfo header time extra =
       ( PeerFetchStatus,
         PeerFetchInFlight header,
         GSV time,
         extra
       )

fetchDecisions :: (HasHeader header, HasHeader block,
                   HeaderHash header ~ HeaderHash block)
               => ConsensusFunctions header block
               -> FetchPolicyParams
               -> ChainFragment block
               -> (Point block -> Bool)
               -> [(ChainFragment header, PeerInfo header time extra)]
               -> [(FetchDecision header, PeerInfo header time extra)]
fetchDecisions ConsensusFunctions {
                 plausibleCandidateChain,
                 compareCandidateChains,
                 blockFetchSize
               }
               fetchPolicyParams
               currentChain fetchedBlocks =

    fetchRequestDecisions
      blockFetchSize
      fetchPolicyParams
  . map (\(c, p@(status,inflight,_,_)) -> (c, status, inflight, p))

  . prioritisePeerChains
      compareCandidateChains
      blockFetchSize
  . map (\(c, p@(_,inflight,deltaq,_)) -> (c, inflight, deltaq, p))

  . chainsFetchFragments
      fetchedBlocks
  . map (\(c, p@(_,inflight,_,_)) -> (c, peerFetchBlocksInFlight inflight, p))

  . chainsForkSuffix
      currentChain

  . filterLongerCandidateChains
      plausibleCandidateChain
      currentChain


-- | Act on decisions to send new requests. In fact all we do here is update
-- request variables that are shared with the threads running the block fetch
-- protocol with each peer.
--
fetchLogicIterationAct :: MonadSTM m
                       => [(FetchDecision header, TFetchRequestVar m header)]
                       -> m ()
fetchLogicIterationAct decisions =
    sequence_
      [ atomically (writeTFetchRequestVar peerFetchRequestVar request)
      | (Right request, peerFetchRequestVar) <- decisions ]


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
       readStatePeerStatus      :: Tr m (Map peer PeerFetchStatus)
     }

-- | STM actions to read various state variables that the fetch logic uses.
-- While the decisions do make use of the values of these variables, it is not
-- necessary to re-evaluate when these variables change.
--
data FetchNonTriggerVariables peer header block m = FetchNonTriggerVariables {
       readStateFetchedBlocks :: Tr m (Point block -> Bool),
       readStatePeerStates    :: Tr m (Map peer (PeerFetchStatus, PeerFetchInFlight header)),
       readStatePeerDeltaQs   :: Tr m (Map peer (GSV (Time m))),
       readStatePeerReqVars   :: Tr m (Map peer (TFetchRequestVar m header))
     }


data FetchStateFingerprint peer header block =
     FetchStateFingerprint
       (Maybe (Point block))
       (Map peer (Maybe (Point header)))
       (Map peer PeerFetchStatus)
  deriving Eq

initialFetchStateFingerprint :: FetchStateFingerprint peer header block
initialFetchStateFingerprint =
    FetchStateFingerprint
      Nothing
      Map.empty
      Map.empty

-- |
--
-- Note that the domain of 'fetchStatePeerChains' is a subset of the domain
-- of 'fetchStatePeerStates' and 'fetchStatePeerReqVars'.
--
data FetchStateSnapshot peer header block m = FetchStateSnapshot {
       fetchStateCurrentChain  :: ChainFragment block,
       fetchStatePeerChains    :: Map peer (ChainFragment header),
       fetchStatePeerStates    :: Map peer (PeerFetchStatus, PeerFetchInFlight header),
       fetchStatePeerDeltaQs   :: Map peer (GSV (Time m)),
       fetchStatePeerReqVars   :: Map peer (TFetchRequestVar m header),
       fetchStateFetchedBlocks :: Point block -> Bool
     }

readStateVariables :: (MonadSTM m, Eq peer, Eq (Point header),
                       HasHeader header, HasHeader block)
                   => FetchTriggerVariables peer header block m
                   -> FetchNonTriggerVariables peer header block m
                   -> FetchStateFingerprint peer header block
                   -> Tr m (FetchStateSnapshot peer header block m,
                            FetchStateFingerprint peer header block)
readStateVariables FetchTriggerVariables{..}
                   FetchNonTriggerVariables{..}
                   fetchStateFingerprint = do

    -- Read all the trigger state variables
    fetchStateCurrentChain  <- readStateCurrentChain
    fetchStatePeerChains    <- readStateCandidateChains
    fetchStatePeerStatus    <- readStatePeerStatus

    -- Construct the change detection fingerprint
    let fetchStateFingerprint' =
          FetchStateFingerprint
            (ChainFragment.headPoint fetchStateCurrentChain)
            (Map.map ChainFragment.headPoint fetchStatePeerChains)
            fetchStatePeerStatus

    -- Check the fingerprint changed, or block and wait until it does
    check (fetchStateFingerprint' /= fetchStateFingerprint)

    -- Now read all the non-trigger state variables
    fetchStatePeerStates    <- readStatePeerStates
    fetchStatePeerDeltaQs   <- readStatePeerDeltaQs
    fetchStatePeerReqVars   <- readStatePeerReqVars
    fetchStateFetchedBlocks <- readStateFetchedBlocks

    -- Construct the overall snapshot of the state
    let fetchStateSnapshot =
          FetchStateSnapshot {
            fetchStateCurrentChain,
            fetchStatePeerChains,
            fetchStatePeerStates,
            fetchStatePeerDeltaQs,
            fetchStatePeerReqVars,
            fetchStateFetchedBlocks
          }

    return (fetchStateSnapshot, fetchStateFingerprint')


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

prioritisePeerChains :: HasHeader header
                     => (ChainFragment header ->
                         ChainFragment header -> Ordering)
                     -> (header -> Word)
                     -> [([ChainFragment header], PeerFetchInFlight header,
                                                  GSV time,
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

newtype FetchRequest header = FetchRequest [[header]]
  deriving Show

type SizeInBytes = Word
--TODO use this consistently and put somewhere sensible


-- | A range on a chain identified by two points. It is exclusive on the
-- lower end and inclusive on the upper end.
--
--data ChainRange header = ChainRange !(Point header) !(Point header)
--  deriving Show

data FetchDecline =
     FetchDeclineAllDownloaded
   | FetchDeclinePeerFailure
   | FetchDeclinePeerSlow
   | FetchDeclineReqsInFlightLimit  !Word
   | FetchDeclineBytesInFlightLimit !Word
   | FetchDeclinePeerBusy           !Word !Word !Word
   | FetchDeclineConcurrencyLimit   !Word
  deriving Show

data FetchPolicyParams = FetchPolicyParams {
       maxConcurrentFetchPeers  :: Word,
       maxInFlightReqsPerPeer   :: Word  -- A protocol constant.
     }

--TODO: compute these based on GSV
data PeerFetchInFlightLimits = PeerFetchInFlightLimits {
       inFlightBytesHighWatermark :: Word,
       inFlightBytesLowWatermark  :: Word
     }

fetchRequestDecisions :: HasHeader header
                      => (header -> Word)
                      -> FetchPolicyParams
                      -> [([ChainFragment header], PeerFetchStatus,
                                                   PeerFetchInFlight header,
                                                   peer)]
                      -> [ (FetchDecision header,  peer)]
fetchRequestDecisions blockFetchSize fetchPolicyParams chains =
    go nConcurrentFetchPeers0 chains
  where
    go !_ [] = []
    go !nConcurrentFetchPeers ((chain, peerFetchStatus,
                                peerFetchInFlight, peer) : cps) =
        (decision, peer) : go nConcurrentFetchPeers' cps
      where
        decision = fetchRequestDecision
                     blockFetchSize
                     fetchPolicyParams
                     nConcurrentFetchPeers
                     (error "TODO PeerFetchInFlightLimits")
                     peerFetchInFlight
                     peerFetchStatus
                     chain

        nConcurrentFetchPeers'
          -- increment if it was idle, and now will not be
          | peerFetchReqsInFlight peerFetchInFlight == 0
          , Right{} <- decision = nConcurrentFetchPeers + 1
          | otherwise           = nConcurrentFetchPeers


    nConcurrentFetchPeers0 =
        fromIntegral
      . length
      . filter (> 0)
      . map (\(_, _, PeerFetchInFlight{peerFetchReqsInFlight}, _) ->
                       peerFetchReqsInFlight)
      $ chains


fetchRequestDecision :: HasHeader header
                     => (header -> Word)
                     -> FetchPolicyParams
                     -> Word
                     -> PeerFetchInFlightLimits
                     -> PeerFetchInFlight header
                     -> PeerFetchStatus
                     -> [ChainFragment header]
                     -> FetchDecision header

fetchRequestDecision _ _ _ _ _ _ [] = Left FetchDeclineAllDownloaded

fetchRequestDecision _ _ _ _ _ PeerFetchStatusFailure _
                                    = Left FetchDeclinePeerFailure

fetchRequestDecision _ _ _ _ _ PeerFetchStatusAberrant _
                                    = Left FetchDeclinePeerSlow

fetchRequestDecision blockFetchSize
                     FetchPolicyParams {
                       maxConcurrentFetchPeers,
                       maxInFlightReqsPerPeer
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
                       => (header -> Word) -- ^ Block body size
                       -> Word -- ^ Current number of requests in flight
                       -> Word -- ^ Maximum number of requests in flight allowed
                       -> Word -- ^ Current number of bytes in flight
                       -> Word -- ^ Maximum number of bytes in flight allowed
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



{-
exPeerFetchDecisions :: [(FetchDecision BlockHeader, String)]
exPeerFetchDecisions =
    fetchRequestDecisions
      exampleConsensusFunctions
      fetchPolicyParams
      [  | (chain, peer) <- exPeerChains4 ]
  where
    fetchPolicyParams = FetchPolicyParams {
        maxInFlightReqsPerPeer   = 2,
        maxConcurrentFetchPeers  = 4
      }

    peerFetchStates =
      Map.fromList
        [ ("Peer1", defaultPeerFetchState { peerFetchInFlight = defaultPeerFetchInFlight { peerFetchReqsInFlight = 2 } } )
        , ("Peer2", defaultPeerFetchState { peerFetchInFlight = defaultPeerFetchInFlight { peerFetchReqsInFlight = 2 } } )
        , ("Peer3", defaultPeerFetchState { peerFetchInFlight = defaultPeerFetchInFlight { peerFetchReqsInFlight = 2 } } )
        , ("Peer4", defaultPeerFetchState)
        , ("Peer5", defaultPeerFetchState)
        , ("Peer6", defaultPeerFetchState)
        , ("Peer7", defaultPeerFetchState)
        ]
    defaultPeerFetchInFlight = PeerFetchInFlight 0 0 Set.empty
    defaultPeerFetchState = PeerFetchState
                              PeerFetchStatusReady
                              defaultPeerFetchInFlight
                              (error "TODO: defaultPeerFetchState1")
                              (error "TODO: defaultPeerFetchState2")
-}

exampleConsensusFunctions :: ConsensusFunctions BlockHeader Block
exampleConsensusFunctions =
    ConsensusFunctions {
      plausibleCandidateChain = \current candidate ->
           ChainFragment.headBlockNo candidate
         > ChainFragment.headBlockNo current,
      compareCandidateChains   = \a b ->
        compare (ChainFragment.headBlockNo a)
                (ChainFragment.headBlockNo b),
      blockFetchSize    = \_ -> 100,
      validateBlockBody = \_ _ -> True
    }



{-
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
-}

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
        peerDeltaQs =
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
-}
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

calculatePeerFetchInFlightLimits :: TimeMeasure time
                                 => (GSV time, GSV time)
                                 -> PeerFetchInFlightLimits
calculatePeerFetchInFlightLimits (_outboundGSV@(GSV g_out _s_out _v_out),
                                   _inboundGSV@(GSV g_in   s_in  _v_in)) =
    PeerFetchInFlightLimits {
      inFlightBytesLowWatermark,
      inFlightBytesHighWatermark
    }
  where
    -- To keep the remote end busy at all times, we want to make sure that by
    -- the time it finishes sending its last response that there's another
    -- request at the remote end that's ready to go. So we must initiate another
    -- request g_out seconds before the remote end becomes idle.
    --
    -- Now it turns out to be more convenient to measure this not in time, but
    -- based on the number of bytes of requests that are in-flight. This of
    -- course directly corresponds to time, via S_in.
    --
    -- The units of S_in is seconds / octet. We need to calculate the number
    -- of bytes that can be in flight during a time interval t. So we need
    -- octets / second * seconds-in-flight = octets.
    --
    -- > (1/s_in) * t   or equivalently  t/s_in
    --
    -- So for a remote peer, g_out seconds before it becomes idle, it will have
    -- \( g_in\/s_in \) bytes inbound. Our request will arrive after g_out
    -- seconds, we should request at minimum \( g_in\/s_in \) bytes.
    --
    -- We should also account for the fact that we do not have perfect
    -- scheduling and cannot initiate the request at exactly the right time, so
    -- we should request it slightly early (and thus request correspondingly
    -- more). Lets say our maximum schedule delay is @d@ seconds.
    --
    inFlightBytesLowWatermark =
      ceiling (toRational (g_out + g_in + d) / toRational s_in)

    d = 20 --TODO: switch to concrete type that's in RealFrac
    -- But note that the minimum here is based on the assumption that we can
    -- react as the /leading/ edge of the low watermark arrives, but in fact
    -- we can only react when the /trailing/ edge arrives. So when we 

    -- The high watermark is a bit arbitrary. It's just about making sure we
    -- have a bit of a buffer so we can ask for more in one go, rather than
    -- asking for lots of small requests very frequently.
    inFlightBytesHighWatermark = inFlightBytesLowWatermark * 2

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

{-
data FetchTrackingState header time = FetchTrackingState {
       fetchRequestsInFlight      :: Queue (FetchRequestBatch header time),
       fetchRequestsBytesInFlight :: Size
     }

type Queue = Q.BankersDequeue

data FetchRequestBatch header time =
     FetchRequestBatch
       !(ChainRange header)         -- the requested range
       [(BlockInfo header, time)]   -- blocks and trailing edge timeouts

data BlockInfo header = BlockInfo !(Point header) !Size
-}



estimateBlockFetchResponse :: TimeMeasure time
                           => GSV time
                           -> GSV time
                           -> PeerFetchInFlight header
                           -> [SizeInBytes]
                           -> Duration time
estimateBlockFetchResponse outboundGSV inboundGSV
                           PeerFetchInFlight{peerFetchBytesInFlight}
                           blockSizes =
    estimateDetltaQ99thPercentile $
         gsvTrailingEdgeArrive outboundGSV reqSize
      <> gsvTrailingEdgeArrive inboundGSV  rspSize
  where
    reqSize = 100 -- not exact, but it's small
    rspSize = peerFetchBytesInFlight + sum blockSizes

-- | The /trailing/ edge arrival schedule for a bunch of blocks.
--
blockArrivalShedule :: TimeMeasure time
                    => GSV time
                    -> GSV time
                    -> PeerFetchInFlight header
                    -> [SizeInBytes]
                    -> [Duration time]
blockArrivalShedule outboundGSV inboundGSV
                    PeerFetchInFlight{peerFetchBytesInFlight}
                    blockSizes =
    [ estimateDetltaQ99thPercentile $
           gsvTrailingEdgeArrive outboundGSV reqSize
        <> gsvTrailingEdgeArrive inboundGSV  rspSize
    | rspSize <- cumulativeSumFrom peerFetchBytesInFlight blockSizes
    ]
  where
    reqSize = 100 -- not exact, but it's small

    cumulativeSumFrom n = tail . scanl (+) n

{-
newFetchRequestBatch :: TimeMeasure time
                     => time
                     -> GSV time
                     -> GSV time
                     -> PeerFetchInFlight header
                     -> ChainRange header
                     -> [BlockInfo header]
                     -> FetchRequestBatch header time
newFetchRequestBatch now outboundGSV inboundGSV fetchTrackingState fetchRange fetchBlocks =
    FetchRequestBatch
      fetchRange
      [ (block, addTime trailingEdge now)
      | (block, trailingEdge) <- zip fetchBlocks blockTrailingEdges ]
  where
    blockTrailingEdges =
      blockArrivalShedule outboundGSV inboundGSV
                          fetchTrackingState
                          fetchBlocks
-}
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

data BlockFetchProtocolFailure =
       BlockFetchProtocolFailureTooFewBlocks
     | BlockFetchProtocolFailureTooManyBlocks
     | BlockFetchProtocolFailureWrongBlock
     | BlockFetchProtocolFailureInvalidBody
  deriving (Eq, Show)

instance Exception BlockFetchProtocolFailure

-- | The implementation of the client side of block fetch protocol designed to
-- work in conjunction with our fetch logic.
--
blockFetchClient :: forall header block m.
                    (MonadSTM m, MonadTime m, MonadThrow m,
                     HasHeader header, HasHeader block,
                     HeaderHash header ~ HeaderHash block)
                 => ConsensusFunctions header block
                 -> FetchClientStateVars header m
                 -> Tr m (GSV (Time m), GSV (Time m))
                 -> PeerPipelined (BlockFetch header block) AsClient BFIdle m ()
blockFetchClient ConsensusFunctions{blockFetchSize, validateBlockBody}
                 FetchClientStateVars {
                   fetchClientStatusVar,
                   fetchClientInFlightVar,
                   fetchClientRequestVar
                 }
                 readPeerGSVs =
    PeerPipelined (senderIdle Zero [])
  where
    senderIdle :: forall n.
                  Nat n
               -> [[header]]
               -> PeerSender (BlockFetch header block) AsClient
                             BFIdle n () m ()

    -- We have no requests to send. Check if we have any pending pipelined
    -- results to collect. If so, go round and collect any more. If not, block
    -- and wait for some new requests.
    senderIdle (Succ outstanding) [] =
      SenderCollect (Just (senderAwait (Succ outstanding)))
                    (\_ -> senderIdle outstanding [])

    -- And similarly if there are no pending pipelined results at all.
    senderIdle Zero [] = senderAwait Zero

    -- We now do have some requests that we have accepted but have yet to
    -- actually send out. Lets send out the first one.
    senderIdle outstanding (fragment:fragments) =
      SenderEffect $ do
{-
        now <- getMonotonicTime
        (outboundGSV, inboundGSV) <- atomically readPeerGSVs
        --TODO: should we pair this up with the senderAwait earlier?
        inFlight  <- readTVar fetchClientInFlightVar

        let blockTrailingEdges =
              blockArrivalShedule
                outboundGSV inboundGSV
                inFlight
                (map snd fragment)

        timeout <- newTimeout (head blockTrailingEdges)
        fork $ do
          fired <- awaitTimeout timeout
          when fired $
            atomically (writeTVar _ PeerFetchStatusAberrant)
-}
        let range = assert (not (null fragment)) $
                    ChainRange (blockPoint (head fragment))
                               (blockPoint (last fragment))
        return $
          SenderPipeline
            (ClientAgency TokIdle)
            (MsgRequestRange range)
            (receiverBusy fragment)
            (senderIdle (Succ outstanding) fragments)

    senderAwait :: forall n.
                   Nat n
                -> PeerSender (BlockFetch header block) AsClient
                              BFIdle n () m ()
    senderAwait outstanding =
      SenderEffect $ do
      -- Atomically grab our next request and update our tracking state.
      -- We have now accepted this request.
      --
      -- It is important to note that we only update our tracking state when
      -- we /accept/ the request, not when the fetch logic /sets/ the request.
      -- The fetching logic can update the request up until the point where
      -- we accept it here. From here on the request is considered to be
      -- in-flight, and the tracking state that the fetch logic uses now
      -- reflects that.
      --
      fragments <- atomically $ do
        FetchRequest fragments <- takeTFetchRequestVar fetchClientRequestVar
        PeerFetchInFlight{..}  <- readTVar fetchClientInFlightVar
        writeTVar fetchClientInFlightVar $!
          PeerFetchInFlight {
            peerFetchReqsInFlight   = peerFetchReqsInFlight
                                    + fromIntegral (length fragments),

            peerFetchBytesInFlight  = peerFetchBytesInFlight
                                    + sum [ blockFetchSize header
                                          | fragment <- fragments
                                          , header   <- fragment ],

            peerFetchBlocksInFlight = peerFetchBlocksInFlight
                          `Set.union` Set.fromList [ blockPoint header
                                                   | fragment <- fragments
                                                   , header   <- fragment ]
          }

        PeerFetchInFlightLimits {
          inFlightBytesHighWatermark
        } <- calculatePeerFetchInFlightLimits <$> readPeerGSVs

        -- Set our status to busy if we've got over the high watermark.
        -- Only update the variable if it changed, to avoid spurious wakeups.
        currentStatus <- readTVar fetchClientStatusVar
        when (peerFetchBytesInFlight >= inFlightBytesHighWatermark &&
              currentStatus == PeerFetchStatusReady) $
          writeTVar fetchClientStatusVar PeerFetchStatusBusy

        --TODO: think about status aberrant

        return fragments

      return (senderIdle outstanding fragments)


    receiverBusy :: [header]
                 -> PeerReceiver (BlockFetch header block) AsClient
                                 BFBusy BFIdle m ()
    receiverBusy headers =
      ReceiverAwait
        (ServerAgency TokBusy) $ \msg ->
        case msg of
          -- The server is reporting that the range we asked for does not exist.
          -- This can happen (even if we didn't make any mistakes) if their
          -- chain forked in the time between when they told us and when we
          -- asked for this range of blocks. If this happens, it should
          -- certainly be the case that this peer doesn't continue to tell us
          -- that this range of blocks is in their chain.
          --
          -- FIXME: For now we will not do the detailed error checking to check
          -- that the peer is not cheating us. Nor will we track these failure
          -- points to make sure we do not ask for extensions of this again.
          MsgNoBlocks   -> ReceiverDone ()
          --TODO: also adjust the in-flight stats

          MsgStartBatch -> ReceiverEffect $ do
            inFlightLimits <- calculatePeerFetchInFlightLimits <$>
                                atomically readPeerGSVs
            return $ receiverStreaming inFlightLimits headers

    receiverStreaming :: PeerFetchInFlightLimits
                      -> [header]
                      -> PeerReceiver (BlockFetch header block) AsClient
                                      BFStreaming BFIdle m ()
    receiverStreaming inFlightLimits@PeerFetchInFlightLimits {
                        inFlightBytesLowWatermark
                      }
                      headers =
      ReceiverAwait
        (ServerAgency TokStreaming) $ \msg ->
        case (msg, headers) of
          (MsgBatchDone, []) -> ReceiverEffect $ do
            atomically $ modifyTVar' fetchClientInFlightVar $ \inflight ->
              inflight {
                peerFetchReqsInFlight = peerFetchReqsInFlight inflight - 1
              }
            return (ReceiverDone ())


          (MsgBlock block, header:headers') -> ReceiverEffect $ do

{-
            -- Now it's totally possible that the timeout already fired
            -- if not, we can update it, making sure the delay is > 0
            now <- getMonotonicTime
            updateTimeout timeout (diffTime now )
-}

            -- Update our in-flight stats and our current status
            atomically $ do
              inflight <- readTVar fetchClientInFlightVar
              writeTVar fetchClientInFlightVar $!
                inflight {
                  peerFetchBytesInFlight  = peerFetchBytesInFlight inflight
                                          - blockFetchSize header,

                  peerFetchBlocksInFlight = blockPoint header
                               `Set.delete` peerFetchBlocksInFlight inflight
                }

              -- Now crucially, we don't want to end up below the in-flight low
              -- watermark because that's when the remote peer would go idle.
              -- But we only get notified of blocks on their /trailing/ edge,
              -- not their leading edge. Our next best thing is the trailing
              -- edge of the block before. So, we check if after the /next/
              -- block we would be below the low watermark, and update our
              -- status to ready if appropriate.
              --
              let nextBytesInFlight =
                      peerFetchBytesInFlight inflight
                    - blockFetchSize header
                    - maybe 0 blockFetchSize (listToMaybe headers')
              currentStatus <- readTVar fetchClientStatusVar
              when (nextBytesInFlight <= inFlightBytesLowWatermark &&
                    currentStatus == PeerFetchStatusBusy) $
                writeTVar fetchClientStatusVar PeerFetchStatusReady

            unless (blockPoint header == castPoint (blockPoint block)) $
              throwM BlockFetchProtocolFailureWrongBlock

            unless (validateBlockBody header block) $
              throwM BlockFetchProtocolFailureInvalidBody

            -- write it to the volatile block store
            -- update the volatile block heap, notifying of any new tips
            -- TODO: when do we reset the status from PeerFetchStatusAberrant
            -- to PeerFetchStatusReady/Busy?

            return (receiverStreaming inFlightLimits headers')

          (MsgBatchDone, (_:_)) -> ReceiverEffect $
            throwM BlockFetchProtocolFailureTooFewBlocks

          (MsgBlock _, []) -> ReceiverEffect $
            throwM BlockFetchProtocolFailureTooManyBlocks



newtype TFetchRequestVar m header =
        TFetchRequestVar (TMVar m (FetchRequest header))

newTFetchRequestVar :: MonadSTM m => Tr m (TFetchRequestVar m header)
newTFetchRequestVar = TFetchRequestVar <$> newEmptyTMVar

-- This may seem a bit odd, but we unconditionally overwrite the TMVar here.
-- The reason is that we are not using this TMVar as a one-place queue of
-- requests. Rather we use this as a box containing the current request.
-- The request is not considered accepted when we put it in the box, but
-- when the fetch protocol client takes it out of the box. Up until the
-- point that the request is accepted we can overwrite it with a updated
-- request. So the logic here is that we either fill the box or or
-- overwrite the contents. We achieve that by atomically trying to empty it
-- (ignoring any content), followed by filling it.
--
writeTFetchRequestVar :: MonadSTM m
                      => TFetchRequestVar m header
                      -> FetchRequest header
                      -> Tr m ()
writeTFetchRequestVar (TFetchRequestVar v) r = tryTakeTMVar v >> putTMVar v r

takeTFetchRequestVar :: MonadSTM m
                     => TFetchRequestVar m header
                     -> Tr m (FetchRequest header)
takeTFetchRequestVar (TFetchRequestVar v) = takeTMVar v

