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
import           Crypto.Random (MonadRandom)
import qualified Data.ByteString.Lazy as Lazy
import           Data.Proxy (Proxy)
import           Data.Word (Word32)

import           Cardano.Crypto (ProtocolMagicId)

import           Ouroboros.Network.Block (BlockNo, HeaderHash, Serialised,
                     SlotNo)
import           Ouroboros.Network.BlockFetch (SizeInBytes)
import           Ouroboros.Network.Magic (NetworkMagic)
import           Ouroboros.Network.Protocol.LocalStateQuery.Codec (Some (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime (SystemStart)
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Mempool
import           Ouroboros.Consensus.Node.Exit (ExitReason)
import           Ouroboros.Consensus.Node.LedgerDerivedInfo
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.State
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Consensus.Storage.ChainDB (ChainDB)
import           Ouroboros.Consensus.Storage.Common (EpochNo, EpochSize)
import           Ouroboros.Consensus.Storage.ImmutableDB (BinaryInfo (..),
                     HashInfo)

{-------------------------------------------------------------------------------
  RunNode proper
-------------------------------------------------------------------------------}

class ( LedgerSupportsProtocol    blk
      , LedgerDerivedInfo         blk
      , ApplyTx                   blk
      , HasTxId            (GenTx blk)
      , QueryLedger               blk
      , HasNetworkProtocolVersion blk
      , NoUnexpectedThunks (NodeState blk)
      , NoUnexpectedThunks (GenTxId blk)
        -- TODO: Remove after reconsidering rewindConsensusState:
      , Serialise (HeaderHash blk)
      ) => RunNode blk where
  nodeForgeBlock          :: MonadRandom m
                          => TopLevelConfig blk
                          -> Update m (NodeState blk)
                          -> SlotNo              -- ^ Current slot
                          -> BlockNo             -- ^ Current block number
                          -> ExtLedgerState blk  -- ^ Current ledger
                          -> [GenTx blk]         -- ^ Txs to add in the block
                          -> IsLeader (BlockProtocol blk)
                          -> m blk

  nodeBlockMatchesHeader  :: Header blk -> blk -> Bool
  nodeBlockFetchSize      :: Header blk -> SizeInBytes
  nodeIsEBB               :: Header blk -> Maybe EpochNo
  nodeEpochSize           :: Monad m
                          => Proxy blk
                          -> TopLevelConfig blk
                          -> EpochNo -> m EpochSize
  nodeStartTime           :: Proxy blk
                          -> TopLevelConfig blk
                          -> SystemStart
  nodeNetworkMagic        :: Proxy blk
                          -> TopLevelConfig blk
                          -> NetworkMagic
  nodeProtocolMagicId     :: Proxy blk
                          -> TopLevelConfig blk
                          -> ProtocolMagicId
  nodeHashInfo            :: Proxy blk
                          -> HashInfo (HeaderHash blk)

  -- | The maximum block size in bytes according to the currently adopted
  -- protocol parameters of the ledger state.
  nodeMaxBlockSize :: LedgerState blk -> Word32

  -- | The block encoding overhead size in bytes.
  --
  -- This encompasses the overhead in bytes for everything that is encoded
  -- within a block, excluding the actual generalized transactions. Given
  -- this, along with the 'nodeMaxBlockSize', it is possible to determine the
  -- amount of generalized transactions that we can include in a block.
  nodeBlockEncodingOverhead :: LedgerState blk -> Word32

  -- | Check the integrity of a block, i.e., that it has not been corrupted by
  -- a bitflip.
  --
  -- Check this by, e.g., verifying whether the block has a valid signature
  -- and that the hash of the body matches the body hash stores in the header.
  --
  -- This does not check the validity of the contents of the block, e.g.,
  -- whether the transactions are valid w.r.t. the ledger, or whether it's
  -- sent by a malicious node.
  nodeCheckIntegrity :: TopLevelConfig blk
                     -> blk -> Bool

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
  nodeInitChainDB :: IOLike m
                  => TopLevelConfig blk
                  -> ChainDB m blk
                  -> m ()
  nodeInitChainDB _ _ = return ()

  -- | This function is called to give a more precise reason why some
  -- exception that will shut down the node was thrown. See
  -- 'Ouroboros.Consensus.Node.Exit' for more details.
  --
  -- One only has to handle exceptions specific to this @blk@.
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
  nodeToExitReason :: Proxy blk -> SomeException -> Maybe ExitReason
  nodeToExitReason _ _ = Nothing

  -- Encoders
  nodeEncodeBlockWithInfo  :: TopLevelConfig blk -> blk -> BinaryInfo Encoding
  nodeEncodeBlock          :: TopLevelConfig blk -> blk -> Encoding
  nodeEncodeBlock cfg blk = binaryBlob $ nodeEncodeBlockWithInfo cfg blk
  nodeEncodeHeader         :: TopLevelConfig blk
                           -> SerialisationVersion (NetworkProtocolVersion blk)
                           -> Header blk -> Encoding
  nodeEncodeWrappedHeader  :: TopLevelConfig blk
                           -> NetworkProtocolVersion blk
                           -> Serialised (Header blk) -> Encoding
  nodeEncodeGenTx          :: GenTx  blk -> Encoding
  nodeEncodeGenTxId        :: GenTxId blk -> Encoding
  nodeEncodeHeaderHash     :: Proxy blk -> HeaderHash blk -> Encoding
  nodeEncodeLedgerState    :: TopLevelConfig blk -> LedgerState blk -> Encoding
  nodeEncodeConsensusState :: Proxy blk -> TopLevelConfig blk -> ConsensusState (BlockProtocol blk) -> Encoding
  nodeEncodeApplyTxError   :: Proxy blk -> ApplyTxErr blk -> Encoding
  nodeEncodeTipInfo        :: Proxy blk -> TipInfo blk -> Encoding
  nodeEncodeQuery          :: Query blk result -> Encoding
  nodeEncodeResult         :: Query blk result -> result -> Encoding

  -- Decoders
  nodeDecodeHeader         :: forall s. TopLevelConfig blk
                           -> SerialisationVersion (NetworkProtocolVersion blk)
                           -> Decoder s (Lazy.ByteString -> Header blk)
  nodeDecodeWrappedHeader  :: forall s. TopLevelConfig blk
                           -> NetworkProtocolVersion blk
                           -> Decoder s (Serialised (Header blk))
  nodeDecodeBlock          :: forall s. TopLevelConfig blk -> Decoder s (Lazy.ByteString -> blk)
  nodeDecodeGenTx          :: forall s. Decoder s (GenTx blk)
  nodeDecodeGenTxId        :: forall s. Decoder s (GenTxId blk)
  nodeDecodeHeaderHash     :: forall s. Proxy blk -> Decoder s (HeaderHash blk)
  nodeDecodeLedgerState    :: forall s. TopLevelConfig blk -> Decoder s (LedgerState blk)
  nodeDecodeConsensusState :: forall s. Proxy blk -> TopLevelConfig blk -> Decoder s (ConsensusState (BlockProtocol blk))
  nodeDecodeApplyTxError   :: forall s. Proxy blk -> Decoder s (ApplyTxErr blk)
  nodeDecodeTipInfo        :: forall s. Proxy blk -> Decoder s (TipInfo blk)
  nodeDecodeQuery          :: forall s. Decoder s (Some (Query blk))
  nodeDecodeResult         :: Query blk result -> forall s. Decoder s result
