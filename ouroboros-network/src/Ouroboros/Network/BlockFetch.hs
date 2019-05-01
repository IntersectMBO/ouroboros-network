{-# LANGUAGE NamedFieldPuns      #-}
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
module Ouroboros.Network.BlockFetch (
    blockFetchLogic,
    BlockFetchConsensusInterface(..),

    -- * The 'FetchClientRegistry'
    FetchClientRegistry,
    newFetchClientRegistry,
  ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Void

import           Control.Monad.Class.MonadSTM
import           Control.Tracer (Tracer)

import           Ouroboros.Network.AnchoredFragment (AnchoredFragment (..))
import           Ouroboros.Network.Block
import           Ouroboros.Network.DeltaQ
                   ( PeerGSV(..), ballisticGSV, degenerateDistribution )

import           Ouroboros.Network.BlockFetch.Client
import           Ouroboros.Network.BlockFetch.State
import           Ouroboros.Network.BlockFetch.Types


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

       -- | Given the current chain, is the given chain plausible as a
       -- candidate chain. Classically for Ouroboros this would simply
       -- check if the candidate is strictly longer, but for Ouroboros
       -- with operational key certificates there are also cases where
       -- we would consider a chain of equal length to the current chain.
       --
       plausibleCandidateChain :: AnchoredFragment header
                               -> AnchoredFragment header -> Bool,

       -- | Compare two candidate chains and return a preference ordering.
       -- This is used as part of selecting which chains to prioritise for
       -- downloading block bodies.
       --
       compareCandidateChains  :: AnchoredFragment header
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
       blockMatchesHeader      :: header -> block -> Bool
     }


-- | Execute the block fetch logic. It monitors the current chain and candidate
-- chains. It decided which block bodies to fetch and manages the process of
-- fetching them, including making alternative decisions based on timeouts and
-- failures.
--
-- This runs forever and should be shut down using mechanisms such as async.
--
blockFetchLogic :: forall peer header block m.
                   (MonadSTM m, Ord peer,
                    HasHeader header, HasHeader block,
                    HeaderHash header ~ HeaderHash block)
                => Tracer m [FetchDecision [Point header]]
                -> BlockFetchConsensusInterface peer header block m
                -> FetchClientRegistry peer header m
                -> m Void
blockFetchLogic decisionTracer
                BlockFetchConsensusInterface{..}
                registry =
    fetchLogicIterations
      decisionTracer
      fetchDecisionPolicy
      fetchTriggerVariables
      fetchNonTriggerVariables
  where
    fetchDecisionPolicy :: FetchDecisionPolicy header
    fetchDecisionPolicy =
      FetchDecisionPolicy {
        -- TODO: This is a protocol constant, but determined elsewhere.
        -- It should be passed in.
        maxInFlightReqsPerPeer   = 10,

        -- TODO: These should be determined by external local node config.
        maxConcurrencyBulkSync   = 2,
        maxConcurrencyDeadline   = 1,

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
        readStateFetchedBlocks = readFetchedBlocks,
        readStatePeerStates    = readFetchClientsStates registry,
        readStatePeerGSVs      = readPeerGSVs,
        readStatePeerReqVars   = readFetchClientsReqVars registry,
        readStateFetchMode     = readFetchMode
      }

    -- TODO: get this from elsewhere once we have DeltaQ info available
    readPeerGSVs = Map.map (const dummyGSVs) <$> readFetchClientsStates registry
    -- roughly 10ms ping time and 1MBit/s bandwidth
    -- leads to ~2200 bytes in flight minimum
    dummyGSVs    = PeerGSV{outboundGSV, inboundGSV}
    inboundGSV   = ballisticGSV 10e-3 10e-6 (degenerateDistribution 0)
    outboundGSV  = inboundGSV

