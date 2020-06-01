{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

-- | Infrastructure required to run a node
--
-- The definitions in this module are independent from any specific protocol.
module Ouroboros.Consensus.Node.Run
  ( RunNode (..)
  ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import           Codec.Serialise (Serialise)
import           Control.Exception (SomeException)
import qualified Data.ByteString.Lazy as Lazy
import           Data.Proxy (Proxy)

import           Ouroboros.Network.Block (HeaderHash, Serialised)
import           Ouroboros.Network.BlockFetch (SizeInBytes)
import           Ouroboros.Network.Protocol.LocalStateQuery.Codec (Some (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Config.SupportsNode
import           Ouroboros.Consensus.HardFork.Abstract
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Node.Exit (ExitReason)
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Consensus.Storage.ChainDB.Init (InitChainDB)
import           Ouroboros.Consensus.Storage.ImmutableDB (BinaryInfo (..),
                     ChunkInfo)

{-------------------------------------------------------------------------------
  RunNode proper
-------------------------------------------------------------------------------}

class ( LedgerSupportsProtocol    blk
      , HasHardForkHistory        blk
      , LedgerSupportsMempool     blk
      , HasTxId            (GenTx blk)
      , QueryLedger               blk
      , HasNetworkProtocolVersion blk
      , CanForge                  blk
      , ConfigSupportsNode        blk
      , ConvertRawHash            blk
        -- TODO: Remove after reconsidering rewindConsensusState:
      , Serialise (HeaderHash blk)
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

  -- | When extracting the bytes corresponding to header from a serialised
  -- block, it may be necessary to add an envelope to it to obtain a
  -- bytestring that can actually be decoded as a header.
  --
  -- For example, a CBOR tag may have to be added in front.
  nodeAddHeaderEnvelope :: Proxy blk
                        -> IsEBB
                        -> SizeInBytes  -- ^ Block size
                        -> Lazy.ByteString -> Lazy.ByteString
  nodeAddHeaderEnvelope _ _ _ = id  -- Default to no envelope

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

  -- Encoders
  nodeEncodeBlockWithInfo  :: CodecConfig blk -> blk -> BinaryInfo Encoding
  nodeEncodeBlock          :: CodecConfig blk -> blk -> Encoding
  nodeEncodeBlock cfg blk = binaryBlob $ nodeEncodeBlockWithInfo cfg blk
  nodeEncodeHeader         :: CodecConfig blk
                           -> SerialisationVersion blk
                           -> Header blk -> Encoding
  nodeEncodeWrappedHeader  :: CodecConfig blk
                           -> SerialisationAcrossNetwork blk
                           -> Serialised (Header blk) -> Encoding
  nodeEncodeGenTx          :: CodecConfig blk -> GenTx blk -> Encoding
  nodeEncodeGenTxId        :: CodecConfig blk -> GenTxId blk -> Encoding
  nodeEncodeHeaderHash     :: CodecConfig blk -> HeaderHash blk -> Encoding
  nodeEncodeLedgerState    :: CodecConfig blk -> LedgerState blk -> Encoding
  nodeEncodeConsensusState :: CodecConfig blk -> ConsensusState (BlockProtocol blk) -> Encoding
  nodeEncodeApplyTxError   :: CodecConfig blk -> ApplyTxErr blk -> Encoding
  nodeEncodeAnnTip         :: CodecConfig blk -> AnnTip blk -> Encoding
  nodeEncodeQuery          :: CodecConfig blk -> Query blk result -> Encoding
  nodeEncodeResult         :: CodecConfig blk -> Query blk result -> result -> Encoding

  -- Decoders
  nodeDecodeHeader         :: CodecConfig blk
                           -> SerialisationVersion blk
                           -> forall s. Decoder s (Lazy.ByteString -> Header blk)
  nodeDecodeWrappedHeader  :: CodecConfig blk
                           -> SerialisationAcrossNetwork blk
                           -> forall s. Decoder s (Serialised (Header blk))
  nodeDecodeBlock          :: CodecConfig blk -> forall s. Decoder s (Lazy.ByteString -> blk)
  nodeDecodeGenTx          :: CodecConfig blk -> forall s. Decoder s (GenTx blk)
  nodeDecodeGenTxId        :: CodecConfig blk -> forall s. Decoder s (GenTxId blk)
  nodeDecodeHeaderHash     :: CodecConfig blk -> forall s. Decoder s (HeaderHash blk)
  nodeDecodeLedgerState    :: CodecConfig blk -> forall s. Decoder s (LedgerState blk)
  nodeDecodeConsensusState :: CodecConfig blk -> forall s. Decoder s (ConsensusState (BlockProtocol blk))
  nodeDecodeApplyTxError   :: CodecConfig blk -> forall s. Decoder s (ApplyTxErr blk)
  nodeDecodeAnnTip         :: CodecConfig blk -> forall s. Decoder s (AnnTip blk)
  nodeDecodeQuery          :: CodecConfig blk -> forall s. Decoder s (Some (Query blk))
  nodeDecodeResult         :: CodecConfig blk -> Query blk result -> forall s. Decoder s result
