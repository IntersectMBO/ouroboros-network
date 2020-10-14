{-# LANGUAGE FlexibleContexts #-}

-- | Infrastructure required to run a node
--
-- The definitions in this module are independent from any specific protocol.
module Ouroboros.Consensus.Node.Run (
    -- * SerialiseDisk
    SerialiseDiskConstraints
  , ImmutableDbSerialiseConstraints
  , LgrDbSerialiseConstraints
  , VolatileDbSerialiseConstraints
    -- * SerialiseNodeToNode
  , SerialiseNodeToNodeConstraints (..)
    -- * SerialiseNodeToClient
  , SerialiseNodeToClientConstraints
    -- * RunNode
  , RunNode (..)
  ) where

import           Ouroboros.Network.Block (Serialised)
import           Ouroboros.Network.Util.ShowProxy

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Config.SupportsNode
import           Ouroboros.Consensus.HardFork.Abstract
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.CommonProtocolParams
import           Ouroboros.Consensus.Ledger.Inspect
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.Serialisation
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Consensus.Storage.ChainDB
                     (ImmutableDbSerialiseConstraints,
                     LgrDbSerialiseConstraints, SerialiseDiskConstraints,
                     VolatileDbSerialiseConstraints)
import           Ouroboros.Consensus.Storage.ChainDB.Init (InitChainDB)
import           Ouroboros.Consensus.Storage.ImmutableDB (ChunkInfo)
import           Ouroboros.Consensus.Storage.Serialisation

{-------------------------------------------------------------------------------
  RunNode proper
-------------------------------------------------------------------------------}

-- | Serialisation constraints needed by the node-to-node protocols
class ( ConvertRawHash blk
      , SerialiseNodeToNode blk blk
      , SerialiseNodeToNode blk (Header blk)
      , SerialiseNodeToNode blk (Serialised blk)
      , SerialiseNodeToNode blk (SerialisedHeader blk)
      , SerialiseNodeToNode blk (GenTx blk)
      , SerialiseNodeToNode blk (GenTxId blk)
      ) => SerialiseNodeToNodeConstraints blk where
  -- | An upper bound on the size in bytes of the block corresponding to the
  -- header. This can be an overestimate, but not an underestimate.
  --
  -- The block fetch client uses this to estimate how bytes will be in flight.
  -- This is also used to limit the number of bytes accepted when downloading
  -- a block.
  --
  -- This is part of this class as it depends on the node-to-node serialisation
  -- format used for blocks.
  estimateBlockSize :: Header blk -> SizeInBytes

-- | Serialisation constraints needed by the node-to-client protocols
class ( ConvertRawHash blk
      , SerialiseNodeToClient blk blk
      , SerialiseNodeToClient blk (Serialised blk)
      , SerialiseNodeToClient blk (GenTx blk)
      , SerialiseNodeToClient blk (ApplyTxErr blk)
      , SerialiseNodeToClient blk (SomeBlock Query blk)
      , SerialiseResult       blk (Query blk)
      ) => SerialiseNodeToClientConstraints blk

class ( LedgerSupportsProtocol           blk
      , InspectLedger                    blk
      , HasHardForkHistory               blk
      , LedgerSupportsMempool            blk
      , HasTxId                   (GenTx blk)
      , QueryLedger                      blk
      , SupportedNetworkProtocolVersion  blk
      , ConfigSupportsNode               blk
      , ConvertRawHash                   blk
      , CommonProtocolParams             blk
      , HasBinaryBlockInfo               blk
      , SerialiseDiskConstraints         blk
      , SerialiseNodeToNodeConstraints   blk
      , SerialiseNodeToClientConstraints blk
      , Show                (CannotForge blk)
      , Show             (ForgeStateInfo blk)
      , Show      (ForgeStateUpdateError blk)
      , ShowProxy                        blk
      , ShowProxy            (ApplyTxErr blk)
      , ShowProxy                 (GenTx blk)
      , ShowProxy                (Header blk)
      , ShowProxy                 (Query blk)
      , ShowProxy           (TxId (GenTx blk))
      ) => RunNode blk where
  nodeImmutableDbChunkInfo :: TopLevelConfig blk -> ChunkInfo

  -- | Check the integrity of a block, i.e., that it has not been corrupted by
  -- a bitflip.
  --
  -- Check this by, e.g., verifying whether the block has a valid signature
  -- and that the hash of the body matches the body hash stores in the header.
  --
  -- This does not check the validity of the contents of the block, e.g.,
  -- whether the transactions are valid w.r.t. the ledger, or whether it's
  -- sent by a malicious node.
  nodeCheckIntegrity :: TopLevelConfig blk -> blk -> Bool

  -- | This function is called when starting up the node, right after the
  -- ChainDB was opened, and before we connect to other nodes and start block
  -- production.
  --
  -- This function can be used to, for example, create the genesis EBB in case
  -- the chain(DB) is empty.
  --
  -- We only provide a limited interface to the chain DB. This is primarily
  -- useful for the definition of combinators (which may need to turn a
  -- 'InitChainDB' for one type of block into an 'InitChainDB' for a closely
  -- related type of block).
  nodeInitChainDB :: IOLike m
                  => TopLevelConfig blk
                  -> InitChainDB m blk
                  -> m ()
  nodeInitChainDB _ _ = return ()
