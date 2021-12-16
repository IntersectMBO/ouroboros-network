{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

{-| Let's start with the big picture...

@
Key:  ┏━━━━━━━━━━━━┓  ╔═════════════╗  ┏━━━━━━━━━━━━━━┓   ╔════════════╗
      ┃ STM-based  ┃  ║active thread║  ┃state instance┃┓  ║ one thread ║╗
      ┃shared state┃  ║             ║  ┃   per peer   ┃┃  ║  per peer  ║║
      ┗━━━━━━━━━━━━┛  ╚═════════════╝  ┗━━━━━━━━━━━━━━┛┃  ╚════════════╝║
                                        ┗━━━━━━━━━━━━━━┛   ╚════════════╝
@

@
  ╔═════════════╗     ┏━━━━━━━━━━━━━┓
  ║ Chain sync  ║╗    ┃   Ledger    ┃
  ║  protocol   ║║◀───┨   state     ┃◀───────────╮
  ║(client side)║║    ┃             ┃            │
  ╚══════╤══════╝║    ┗━━━━━━━━━━━━━┛            │
   ╚═════╪═══════╝                               │
         ▼                                       │
  ┏━━━━━━━━━━━━━┓     ┏━━━━━━━━━━━━━┓     ╔══════╧══════╗
  ┃  Candidate  ┃     ┃   Set of    ┃     ║  Chain and  ║
  ┃  chains     ┃     ┃  downloaded ┠────▶║   ledger    ║
  ┃  (headers)  ┃     ┃   blocks    ┃     ║  validation ║
  ┗━━━━━┯━━━━━━━┛     ┗━━━━━┯━━━━━━━┛     ╚══════╤══════╝
        │                   │ ▲                  │
        │ ╭─────────────────╯ │                  │
░░░░░░░░▼░▼░░░░░░░░           │                  ▼
░░╔═════════════╗░░           │           ┏━━━━━━━━━━━━━┓     ╔═════════════╗
░░║    Block    ║░░           │           ┃   Current   ┃     ║ Block fetch ║╗
░░╢    fetch    ║◀────────────┼───────────┨    chain    ┠────▶║ protocol    ║║
░░║    logic    ║░░           │           ┃  (blocks)   ┃     ║(server side)║║
░░╚═════════════╝░░           │           ┠─────────────┨     ╚═════════════╝║
░░░░░░░░░▲░░░░░░░░░           │           ┃  Tentative  ┃      ╚═════════════╝
░░░░░░░░░▼░░░░░░░░░░░░░░░░░░░░│░░░░░░░░   ┃    chain    ┠──╮
░░┏━━━━━━━━━━━━━┓░░░░░╔═══════╧═════╗░░   ┃  (headers)  ┃  │  ╔═════════════╗
░░┃ Block fetch ┃┓░░░░║ block fetch ║╗░   ┗━━━━━━━━━━━━━┛  │  ║ Chain sync  ║╗
░░┃  state and  ┃┃◀──▶║  protocol   ║║░                    ╰─▶║ protocol    ║║
░░┃  requests   ┃┃░░░░║(client side)║║░                       ║(server side)║║
░░┗━━━━━━━━━━━━━┛┃░░░░╚═════════════╝║░                       ╚═════════════╝║
░░░┗━━━━━━━━━━━━━┛░░░░░╚═════════════╝░                        ╚═════════════╝
░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
@
Notes:

 * Thread communication is via STM based state.
 * Outbound: threads update STM state.
 * Inbound: threads wait on STM state changing (using retry).
 * These are no queues: there is only the current state, not all change events.

We consider the block fetch logic and the policy for the block fetch protocol
client together as one unit of functionality. This is the shaded area in the
diagram.

Looking at the diagram we see that these two threads interact with each other
and other threads via the following shared state

+-----------------------------+----------------+--------------------+
|  State                      |  Interactions  | Internal\/External |
+=============================+================+====================+
|  Candidate chains (headers) |  Read          |  External          |
+-----------------------------+----------------+--------------------+
|  Current chain (blocks)     |  Read          |  External          |
+-----------------------------+----------------+--------------------+
|  Set of downloaded blocks   |  Read & Write  |  External          |
+-----------------------------+----------------+--------------------+
|  Block fetch requests       |  Read & Write  |  Internal          |
+-----------------------------+----------------+--------------------+

The block fetch requests state is private between the block fetch logic
and the block fetch protocol client, so it is implemented here.

The other state is managed by the consensus layer and is considered external
here. So here we define interfaces for interacting with the external state.
These have to be provided when instantiating the block fetch logic.

-}
module Ouroboros.Network.BlockFetch
  ( blockFetchLogic
  , BlockFetchConfiguration (..)
  , BlockFetchConsensusInterface (..)
    -- ** Tracer types
  , FetchDecision
  , TraceFetchClientState (..)
  , TraceLabelPeer (..)
    -- * The 'FetchClientRegistry'
  , FetchClientRegistry
  , newFetchClientRegistry
  , bracketFetchClient
  , bracketSyncWithFetchClient
  , bracketKeepAliveClient
    -- * Re-export types used by 'BlockFetchConsensusInterface'
  , FetchMode (..)
  , FromConsensus (..)
  , SizeInBytes
  ) where

import           Data.Hashable (Hashable)
import           Data.Map.Strict (Map)
import           Data.Void
import           GHC.Stack (HasCallStack)

import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Tracer (Tracer)

import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import           Ouroboros.Network.Block
import           Ouroboros.Network.DeltaQ (SizeInBytes)

import           Ouroboros.Network.BlockFetch.ClientRegistry
                     (FetchClientPolicy (..), FetchClientRegistry,
                     bracketFetchClient, bracketKeepAliveClient,
                     bracketSyncWithFetchClient, newFetchClientRegistry,
                     readFetchClientsStateVars, readFetchClientsStatus,
                     readPeerGSVs, setFetchClientContext)
import           Ouroboros.Network.BlockFetch.ClientState (FromConsensus (..))
import           Ouroboros.Network.BlockFetch.State


-- | The consensus layer functionality that the block fetch logic requires.
--
-- These are provided as input to the block fetch by the consensus layer.
--
data BlockFetchConsensusInterface peer header block m =
     BlockFetchConsensusInterface {

       -- | Read the K-suffixes of the candidate chains.
       --
       -- Assumptions:
       -- * They must be already validated.
       -- * They may contain /fewer/ than @K@ blocks.
       -- * Their anchor does not have to intersect with the current chain.
       readCandidateChains    :: STM m (Map peer (AnchoredFragment header)),

       -- | Read the K-suffix of the current chain.
       --
       -- This must contain info on the last @K@ blocks (unless we're near
       -- the chain genesis of course).
       --
       readCurrentChain       :: STM m (AnchoredFragment header),

       -- | Read the current fetch mode that the block fetch logic should use.
       --
       -- The fetch mode is a dynamic part of the block fetch policy. In
       -- 'FetchModeBulkSync' it follows a policy that optimises for expected
       -- bandwidth over latency to fetch any particular block, whereas in
       -- 'FetchModeDeadline' it follows a policy optimises for the latency
       -- to fetch blocks, at the expense of wasting bandwidth.
       --
       -- This mode should be set so that when the node's current chain is near
       -- to \"now\" it uses the deadline mode, and when it is far away it uses
       -- the bulk sync mode.
       --
       readFetchMode          :: STM m FetchMode,

       -- | Recent, only within last K
       readFetchedBlocks      :: STM m (Point block -> Bool),

       -- | This and 'readFetchedBlocks' are required to be linked. Upon
       -- successful completion of 'addFetchedBlock' it must be the case that
       -- 'readFetchedBlocks' reports the block.
       addFetchedBlock        :: Point block -> block -> m (),

       -- | The highest stored/downloaded slot number.
       --
       -- This is used to optimise the filtering of fragments in the block
       -- fetch logic: when removing already downloaded blocks from a
       -- fragment, the filtering (with a linear cost) is stopped as soon as a
       -- block has a slot number higher than this slot number, as it cannot
       -- have been downloaded anyway.
       readFetchedMaxSlotNo    :: STM m MaxSlotNo,

       -- | Given the current chain, is the given chain plausible as a
       -- candidate chain. Classically for Ouroboros this would simply
       -- check if the candidate is strictly longer, but for Ouroboros
       -- with operational key certificates there are also cases where
       -- we would consider a chain of equal length to the current chain.
       --
       plausibleCandidateChain :: HasCallStack
                               => AnchoredFragment header
                               -> AnchoredFragment header -> Bool,

       -- | Compare two candidate chains and return a preference ordering.
       -- This is used as part of selecting which chains to prioritise for
       -- downloading block bodies.
       --
       compareCandidateChains  :: HasCallStack
                               => AnchoredFragment header
                               -> AnchoredFragment header
                               -> Ordering,

       -- | Much of the logic for deciding which blocks to download from which
       -- peer depends on making estimates based on recent performance metrics.
       -- These estimates of course depend on the amount of data we will be
       -- downloading.
       --
       blockFetchSize          :: header -> SizeInBytes,

       -- | Given a block header, validate the supposed corresponding block
       -- body.
       --
       blockMatchesHeader      :: header -> block -> Bool,

       -- | Calculate when a header's block was forged.
       --
       -- PRECONDITION: This function will succeed and give a _correct_ result
       -- when applied to headers obtained via this interface (ie via
       -- Consensus, ie via 'readCurrentChain' or 'readCandidateChains').
       --
       -- WARNING: This function may fail or, worse, __give an incorrect result
       -- (!!)__ if applied to headers obtained from sources outside of this
       -- interface. The 'FromConsensus' newtype wrapper is intended to make it
       -- difficult to make that mistake, so please pay that syntactic price
       -- and consider its meaning at each call to this function. Relatedly,
       -- preserve that argument wrapper as much as possible when deriving
       -- ancillary functions\/interfaces from this function.
       headerForgeUTCTime :: FromConsensus header -> STM m UTCTime,

       -- | Calculate when a block was forged.
       --
       -- PRECONDITION: Same as 'headerForgeUTCTime'.
       --
       -- WARNING: Same as 'headerForgeUTCTime'.
       blockForgeUTCTime  :: FromConsensus block -> STM m UTCTime
     }

-- | Configuration for FetchDecisionPolicy.
-- Should be determined by external local node config.
data BlockFetchConfiguration =
     BlockFetchConfiguration {
         -- | Maximum concurrent downloads during bulk syncing.
         bfcMaxConcurrencyBulkSync :: !Word,

         -- | Maximum concurrent downloads during deadline syncing.
         bfcMaxConcurrencyDeadline :: !Word,

         -- | Maximum requests in flight per each peer.
         bfcMaxRequestsInflight    :: !Word,

         -- | Desired intervall between calls to fetchLogicIteration
         bfcDecisionLoopInterval   :: !DiffTime,

         -- | Salt used when comparing peers
         bfcSalt                   :: !Int
     }

-- | Execute the block fetch logic. It monitors the current chain and candidate
-- chains. It decided which block bodies to fetch and manages the process of
-- fetching them, including making alternative decisions based on timeouts and
-- failures.
--
-- This runs forever and should be shut down using mechanisms such as async.
--
blockFetchLogic :: forall peer header block m.
                   ( HasHeader header
                   , HasHeader block
                   , HeaderHash header ~ HeaderHash block
                   , MonadDelay m
                   , MonadMonotonicTime m
                   , MonadSTM m
                   , Ord peer
                   , Hashable peer
                   )
                => Tracer m [TraceLabelPeer peer (FetchDecision [Point header])]
                -> Tracer m (TraceLabelPeer peer (TraceFetchClientState header))
                -> BlockFetchConsensusInterface peer header block m
                -> FetchClientRegistry peer header block m
                -> BlockFetchConfiguration
                -> m Void
blockFetchLogic decisionTracer clientStateTracer
                BlockFetchConsensusInterface{..}
                registry
                BlockFetchConfiguration{..} = do

    setFetchClientContext registry clientStateTracer fetchClientPolicy

    fetchLogicIterations
      decisionTracer clientStateTracer
      fetchDecisionPolicy
      fetchTriggerVariables
      fetchNonTriggerVariables
  where
    fetchClientPolicy :: FetchClientPolicy header block m
    fetchClientPolicy = FetchClientPolicy {
                          blockFetchSize,
                          blockMatchesHeader,
                          addFetchedBlock,
                          blockForgeUTCTime
                        }

    fetchDecisionPolicy :: FetchDecisionPolicy header
    fetchDecisionPolicy =
      FetchDecisionPolicy {
        maxInFlightReqsPerPeer   = bfcMaxRequestsInflight,
        maxConcurrencyBulkSync   = bfcMaxConcurrencyBulkSync,
        maxConcurrencyDeadline   = bfcMaxConcurrencyDeadline,
        decisionLoopInterval     = bfcDecisionLoopInterval,
        peerSalt                 = bfcSalt,

        plausibleCandidateChain,
        compareCandidateChains,
        blockFetchSize
      }

    fetchTriggerVariables :: FetchTriggerVariables peer header m
    fetchTriggerVariables =
      FetchTriggerVariables {
        readStateCurrentChain    = readCurrentChain,
        readStateCandidateChains = readCandidateChains,
        readStatePeerStatus      = readFetchClientsStatus registry
      }

    fetchNonTriggerVariables :: FetchNonTriggerVariables peer header block m
    fetchNonTriggerVariables =
      FetchNonTriggerVariables {
        readStateFetchedBlocks    = readFetchedBlocks,
        readStatePeerStateVars    = readFetchClientsStateVars registry,
        readStatePeerGSVs         = readPeerGSVs registry,
        readStateFetchMode        = readFetchMode,
        readStateFetchedMaxSlotNo = readFetchedMaxSlotNo
      }
