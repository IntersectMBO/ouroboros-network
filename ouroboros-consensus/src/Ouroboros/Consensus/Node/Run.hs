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
  , RunNode
  ) where

import           Ouroboros.Network.Block (Serialised)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config.SupportsNode
import           Ouroboros.Consensus.HardFork.Abstract
import           Ouroboros.Consensus.Ledger.CommonProtocolParams
import           Ouroboros.Consensus.Ledger.Inspect
import           Ouroboros.Consensus.Ledger.Query
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Ledger.SupportsPeerSelection
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Node.InitStorage
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.Serialisation
import           Ouroboros.Consensus.Util (ShowProxy)

import           Ouroboros.Consensus.Storage.ChainDB
                     (ImmutableDbSerialiseConstraints,
                     LgrDbSerialiseConstraints, SerialiseDiskConstraints,
                     VolatileDbSerialiseConstraints)
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
      , SerialiseNodeToClient blk (SomeSecond Query blk)
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
      , LedgerSupportsPeerSelection      blk
      , NodeInitStorage                  blk
      , BlockSupportsMetrics             blk
      , Show                (CannotForge blk)
      , Show             (ForgeStateInfo blk)
      , Show      (ForgeStateUpdateError blk)
      , ShowProxy                        blk
      , ShowProxy            (ApplyTxErr blk)
      , ShowProxy                 (GenTx blk)
      , ShowProxy                (Header blk)
      , ShowProxy                 (Query blk)
      , ShowProxy           (TxId (GenTx blk))
      ) => RunNode blk
  -- This class is intentionally empty. It is not necessarily compositional - ie
  -- the instance for 'HardForkBlock' might do more than merely delegate to the
  -- instance for each era - but we want as many of its super classes as
  -- possible to rely on compositional instances when possible. Not putting any
  -- methods here helps encourage that.
