{-# LANGUAGE DeriveFunctor           #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | Infrastructure required to run a node
--
-- The definitions in this module are independent from any specific protocol.
module Ouroboros.Consensus.Node.Run.Abstract
  ( RunNode (..)
  ) where

import           Cardano.Binary (WrappedDecoder, DecoderError
                                , decodeWrapped)
import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import           Crypto.Random (MonadRandom)
import qualified Data.ByteString.Lazy as Lazy
import           Data.Proxy (Proxy(..))
import qualified Data.Text as T
import           Data.Typeable (Typeable, typeRep)

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
  nodeIsEBB               :: blk -> Maybe EpochNo
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

  -- Encoders
  nodeEncodeBlockWithInfo :: NodeConfig (BlockProtocol blk) -> blk -> BinaryInfo Encoding
  nodeEncodeBlock         :: NodeConfig (BlockProtocol blk) -> blk -> Encoding
  nodeEncodeBlock cfg blk =  binaryBlob $ nodeEncodeBlockWithInfo cfg blk
  nodeEncodeHeader        :: NodeConfig (BlockProtocol blk) -> Header blk -> Encoding
  nodeEncodeGenTx         :: GenTx  blk -> Encoding
  nodeEncodeGenTxId       :: GenTxId blk -> Encoding
  nodeEncodeHeaderHash    :: Proxy blk -> HeaderHash blk -> Encoding
  nodeEncodeLedgerState   :: NodeConfig (BlockProtocol blk) -> LedgerState blk -> Encoding
  nodeEncodeChainState    :: Proxy blk -> ChainState (BlockProtocol blk) -> Encoding
  nodeEncodeApplyTxError  :: Proxy blk -> ApplyTxErr blk -> Encoding

  -- Decoders
  nodeDecodeHeader        :: NodeConfig (BlockProtocol blk) -> WrappedDecoder (Header blk)
  nodeDecodeBlock         :: NodeConfig (BlockProtocol blk) -> WrappedDecoder blk
  nodeDecodeGenTx         :: WrappedDecoder (GenTx blk)
  nodeDecodeGenTxId       :: WrappedDecoder (GenTxId blk)
  nodeDecodeHeaderHash    :: Proxy blk -> WrappedDecoder (HeaderHash blk)
  nodeDecodeLedgerState   :: NodeConfig (BlockProtocol blk) -> WrappedDecoder (LedgerState blk)
  nodeDecodeChainState    :: Proxy blk -> WrappedDecoder (ChainState (BlockProtocol blk))
  nodeDecodeApplyTxError  :: Proxy blk -> WrappedDecoder (ApplyTxErr blk)
