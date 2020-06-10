{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- | Infrastructure required to run a node
--
-- The definitions in this module are independent from any specific protocol.
module Ouroboros.Consensus.Node.Run (
    -- * SerialiseDisk
    SerialiseDiskConstraints
  , ImmDbSerialiseConstraints
  , LgrDbSerialiseConstraints
  , VolDbSerialiseConstraints
    -- * SerialiseNodeToNode
  , SerialiseNodeToNodeConstraints
    -- * SerialiseNodeToClient
  , SerialiseNodeToClientConstraints
    -- * RunNode
  , RunNode (..)
  ) where

import           Control.Exception (SomeException)
import qualified Data.ByteString.Lazy as Lazy
import           Data.ByteString.Short (ShortByteString)
import           Data.Proxy (Proxy)
import           Data.Word (Word8)

import           Ouroboros.Network.Block (HeaderHash, Serialised)
import           Ouroboros.Network.BlockFetch (SizeInBytes)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Config.SupportsNode
import           Ouroboros.Consensus.HardFork.Abstract
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Node.Exit (ExitReason)
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.Serialisation
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Consensus.Storage.ChainDB (ImmDbSerialiseConstraints,
                     LgrDbSerialiseConstraints, SerialiseDiskConstraints,
                     VolDbSerialiseConstraints)
import           Ouroboros.Consensus.Storage.ChainDB.Init (InitChainDB)
import           Ouroboros.Consensus.Storage.ChainDB.Serialisation
                     (EncodeDiskDep, ReconstructNestedCtxt,
                     addReconstructedTypeEnvelope, reconstructPrefixLen)
import           Ouroboros.Consensus.Storage.Common (BinaryBlockInfo (..))
import           Ouroboros.Consensus.Storage.ImmutableDB (ChunkInfo)

{-------------------------------------------------------------------------------
  RunNode proper
-------------------------------------------------------------------------------}

-- | Serialisation constraints needed by the node-to-node protocols
class ( SerialiseNodeToNode blk (HeaderHash blk)
      , SerialiseNodeToNode blk blk
      , SerialiseNodeToNode blk (Header blk)
      , SerialiseNodeToNode blk (Serialised blk)
      , SerialiseNodeToNode blk (Serialised (Header blk))
      , SerialiseNodeToNode blk (GenTx blk)
      , SerialiseNodeToNode blk (GenTxId blk)
      ) => SerialiseNodeToNodeConstraints blk

-- | Serialisation constraints needed by the node-to-client protocols
class ( SerialiseNodeToClient blk (HeaderHash blk)
      , SerialiseNodeToClient blk blk
      , SerialiseNodeToClient blk (Serialised blk)
      , SerialiseNodeToClient blk (GenTx blk)
      , SerialiseNodeToClient blk (ApplyTxErr blk)
      , SerialiseNodeToClient blk (Some (Query blk))
      , SerialiseResult       blk (Query blk)
      ) => SerialiseNodeToClientConstraints blk

class ( LedgerSupportsProtocol    blk
      , HasHardForkHistory        blk
      , LedgerSupportsMempool     blk
      , HasTxId            (GenTx blk)
      , QueryLedger               blk
      , HasNetworkProtocolVersion blk
      , CanForge                  blk
      , ConfigSupportsNode        blk
      , HasCodecConfig            blk
      , ConvertRawHash            blk
      , SerialiseDiskConstraints         blk
      , SerialiseNodeToNodeConstraints   blk
      , SerialiseNodeToClientConstraints blk
      ) => RunNode blk where
  nodeBlockFetchSize      :: Header blk -> SizeInBytes

  nodeImmDbChunkInfo      :: TopLevelConfig blk -> ChunkInfo

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

  -- | Return information about the serialised block, i.e., how to extract the
  -- bytes corresponding to the header from the serialised block.
  nodeGetBinaryBlockInfo :: blk -> BinaryBlockInfo

  -- | Number of bytes from the start of the serialisation of a block needed
  -- for 'nodeAddHeaderEnvelope'
  --
  -- This will be the /minimum/ length of the 'ShortByteString' passed to
  -- 'nodeAddHeaderEnvelope'.
  nodeReconstructPrefixLen :: Proxy blk -> Word8

  default nodeReconstructPrefixLen
    :: ReconstructNestedCtxt Header blk => Proxy blk -> Word8
  nodeReconstructPrefixLen _ = reconstructPrefixLen (Proxy @(Header blk))

  -- | When extracting the bytes corresponding to header from a serialised
  -- block, it may be necessary to add an envelope to it to obtain a
  -- bytestring that can actually be decoded as a header.
  --
  -- For example, a CBOR tag may have to be added in front.
  --
  -- TODO: Now that we have 'HasNestedContent', we should get rid of this.
  -- <https://github.com/input-output-hk/ouroboros-network/issues/2237>
  nodeAddHeaderEnvelope
    :: CodecConfig blk
    -> ShortByteString  -- ^ First bytes ('nodeReconstructPrefixLen') of the
                        -- block
    -> SizeInBytes      -- ^ Block size
    -> Lazy.ByteString -> Lazy.ByteString

  default nodeAddHeaderEnvelope
    :: (ReconstructNestedCtxt Header blk, EncodeDiskDep (NestedCtxt Header) blk)
    => CodecConfig blk
    -> ShortByteString
    -> SizeInBytes  -- ^ Block size
    -> Lazy.ByteString -> Lazy.ByteString
  nodeAddHeaderEnvelope = addReconstructedTypeEnvelope (Proxy @(Header blk))

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

  -- | This function is called to determine whether a thrown exception is
  -- fatal and should shut down the node or not. If fatal, it should return a
  -- precise reason why. See 'Ouroboros.Consensus.Node.Exit' for more details.
  --
  -- One only has to handle exceptions specific to this @blk@.
  --
  -- Return 'Nothing' for exceptions not related to @blk@, they will be
  -- classified by the existing error policy.
  --
  -- Also return 'Nothing' for exceptions /related/ to @blk@ that are /not
  -- fatal/, they will fall through to the default policy.
  --
  -- NOTE: this is not about header/block/transaction validation errors, etc.
  -- These will not shut down the node and are handled already.
  --
  -- For example, if some exception indicates database corruption which could
  -- not be detected by the database, one could classify that exception as
  -- 'DatabaseCorruption', which would trigger an automatic database
  -- validation on the next startup.
  --
  -- In case you have no exceptions to handle specially, return 'Nothing'.
  -- This is what the default implementation does.
  nodeExceptionIsFatal :: Proxy blk -> SomeException -> Maybe ExitReason
  nodeExceptionIsFatal _ _ = Nothing
