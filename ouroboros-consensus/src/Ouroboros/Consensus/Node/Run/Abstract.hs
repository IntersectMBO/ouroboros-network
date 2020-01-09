{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | Infrastructure required to run a node
--
-- The definitions in this module are independent from any specific protocol.
module Ouroboros.Consensus.Node.Run.Abstract
  ( RunNode (..)
  ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import           Crypto.Random (MonadRandom)
import qualified Data.ByteString.Lazy as Lazy
import           Data.Proxy (Proxy)
import           Data.Word (Word32)

import           Cardano.Crypto (ProtocolMagicId)

import           Ouroboros.Network.Block (BlockNo, ChainHash (..), HeaderHash,
                     SlotNo)
import           Ouroboros.Network.BlockFetch (SizeInBytes)
import           Ouroboros.Network.Magic (NetworkMagic)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime (SystemStart)
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Byron
import           Ouroboros.Consensus.Mempool
import           Ouroboros.Consensus.Protocol.Abstract

import           Ouroboros.Storage.Common (EpochNo, EpochSize)
import           Ouroboros.Storage.ImmutableDB (BinaryInfo (..), HashInfo)

class (ProtocolLedgerView blk, ApplyTx blk) => RunNode blk where

  nodeForgeBlock          :: (HasNodeState (BlockProtocol blk) m, MonadRandom m)
                          => NodeConfig (BlockProtocol blk)
                          -> SlotNo         -- ^ Current slot
                          -> BlockNo        -- ^ Current block number
                          -> ChainHash blk  -- ^ Previous hash
                          -> [GenTx blk]    -- ^ Txs to add in the block
                          -> IsLeader (BlockProtocol blk)
                          -> m blk

  nodeBlockMatchesHeader  :: Header blk -> blk -> Bool
  nodeBlockFetchSize      :: Header blk -> SizeInBytes
  nodeIsEBB               :: Header blk -> Maybe EpochNo
  nodeEpochSize           :: Monad m
                          => Proxy blk
                          -> NodeConfig (BlockProtocol blk)
                          -> EpochNo -> m EpochSize
  nodeStartTime           :: Proxy blk
                          -> NodeConfig (BlockProtocol blk)
                          -> SystemStart
  nodeNetworkMagic        :: Proxy blk
                          -> NodeConfig (BlockProtocol blk)
                          -> NetworkMagic
  nodeProtocolMagicId     :: Proxy blk
                          -> NodeConfig (BlockProtocol blk)
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
  nodeCheckIntegrity      :: NodeConfig (BlockProtocol blk)
                          -> blk -> Bool

  -- | When extracting the bytes corresponding to header from a serialised
  -- block, it may be necessary to add an envelope to it to obtain a
  -- bytestring that can actually be decoded as a header.
  --
  -- For example, a CBOR tag may have to be added in front.
  nodeAddHeaderEnvelope   :: Proxy blk
                          -> IsEBB
                          -> Lazy.ByteString -> Lazy.ByteString
  nodeAddHeaderEnvelope _ _ = id  -- Default to no envelope

  -- Encoders
  nodeEncodeBlockWithInfo :: NodeConfig (BlockProtocol blk) -> blk -> BinaryInfo Encoding
  nodeEncodeBlock         :: NodeConfig (BlockProtocol blk) -> blk -> Encoding
  nodeEncodeBlock cfg blk =  binaryBlob $ nodeEncodeBlockWithInfo cfg blk
  nodeEncodeHeader        :: NodeConfig (BlockProtocol blk) -> Header blk -> Encoding
  nodeEncodeGenTx         :: GenTx  blk -> Encoding
  nodeEncodeGenTxId       :: GenTxId blk -> Encoding
  nodeEncodeHeaderHash    :: Proxy blk -> HeaderHash blk -> Encoding
  nodeEncodeLedgerState   :: NodeConfig (BlockProtocol blk) -> LedgerState blk -> Encoding
  nodeEncodeChainState    :: Proxy blk -> NodeConfig (BlockProtocol blk) -> ChainState (BlockProtocol blk) -> Encoding
  nodeEncodeApplyTxError  :: Proxy blk -> ApplyTxErr blk -> Encoding

  -- Decoders
  nodeDecodeHeader        :: forall s. NodeConfig (BlockProtocol blk) -> Decoder s (Lazy.ByteString -> Header blk)
  nodeDecodeBlock         :: forall s. NodeConfig (BlockProtocol blk) -> Decoder s (Lazy.ByteString -> blk)
  nodeDecodeGenTx         :: forall s. Decoder s (GenTx blk)
  nodeDecodeGenTxId       :: forall s. Decoder s (GenTxId blk)
  nodeDecodeHeaderHash    :: forall s. Proxy blk -> Decoder s (HeaderHash blk)
  nodeDecodeLedgerState   :: forall s. NodeConfig (BlockProtocol blk) -> Decoder s (LedgerState blk)
  nodeDecodeChainState    :: forall s. Proxy blk -> NodeConfig (BlockProtocol blk) -> Decoder s (ChainState (BlockProtocol blk))
  nodeDecodeApplyTxError  :: forall s. Proxy blk -> Decoder s (ApplyTxErr blk)
