{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

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
  , WhetherReceivingTentativeBlocks (..)
  ) where

import Data.Hashable (Hashable)
import Data.Void

import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Tracer (Tracer)

import Ouroboros.Network.Block
import Ouroboros.Network.SizeInBytes (SizeInBytes)

import Ouroboros.Network.BlockFetch.ClientRegistry (FetchClientPolicy (..),
           FetchClientRegistry, bracketFetchClient, bracketKeepAliveClient,
           bracketSyncWithFetchClient, newFetchClientRegistry,
           readFetchClientsStateVars, readFetchClientsStatus, readPeerGSVs,
           setFetchClientContext)
import Ouroboros.Network.BlockFetch.ConsensusInterface
           (BlockFetchConsensusInterface (..), FromConsensus (..),
           WhetherReceivingTentativeBlocks (..))
import Ouroboros.Network.BlockFetch.State



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

         -- | Desired interval between calls to fetchLogicIteration
         bfcDecisionLoopInterval   :: !DiffTime,

         -- | Salt used when comparing peers
         bfcSalt                   :: !Int
     }
     deriving (Show)

-- | Execute the block fetch logic. It monitors the current chain and candidate
-- chains. It decided which block bodies to fetch and manages the process of
-- fetching them, including making alternative decisions based on timeouts and
-- failures.
--
-- This runs forever and should be shut down using mechanisms such as async.
--
blockFetchLogic :: forall addr header block m.
                   ( HasHeader header
                   , HasHeader block
                   , HeaderHash header ~ HeaderHash block
                   , MonadDelay m
                   , MonadSTM m
                   , Ord addr
                   , Hashable addr
                   )
                => Tracer m [TraceLabelPeer addr (FetchDecision [Point header])]
                -> Tracer m (TraceLabelPeer addr (TraceFetchClientState header))
                -> BlockFetchConsensusInterface addr header block m
                -> FetchClientRegistry addr header block m
                -> BlockFetchConfiguration
                -> m Void
blockFetchLogic decisionTracer clientStateTracer
                BlockFetchConsensusInterface{..}
                registry
                BlockFetchConfiguration{..} = do

    setFetchClientContext registry clientStateTracer mkFetchClientPolicy

    fetchLogicIterations
      decisionTracer clientStateTracer
      fetchDecisionPolicy
      fetchTriggerVariables
      fetchNonTriggerVariables
      demoteCSJDynamo
  where
    mkFetchClientPolicy :: WhetherReceivingTentativeBlocks -> STM m (FetchClientPolicy header block m)
    mkFetchClientPolicy receivingTentativeBlocks = do
      addFetchedBlock <- mkAddFetchedBlock receivingTentativeBlocks
      pure FetchClientPolicy {
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

    fetchTriggerVariables :: FetchTriggerVariables addr header m
    fetchTriggerVariables =
      FetchTriggerVariables {
        readStateCurrentChain    = readCurrentChain,
        readStateCandidateChains = readCandidateChains,
        readStatePeerStatus      = readFetchClientsStatus registry
      }

    fetchNonTriggerVariables :: FetchNonTriggerVariables addr header block m
    fetchNonTriggerVariables =
      FetchNonTriggerVariables {
        readStateFetchedBlocks    = readFetchedBlocks,
        readStatePeerStateVars    = readFetchClientsStateVars registry,
        readStatePeerGSVs         = readPeerGSVs registry,
        readStateFetchMode        = readFetchMode,
        readStateFetchedMaxSlotNo = readFetchedMaxSlotNo,
        readStateChainSelStarvation = readChainSelStarvation
      }
