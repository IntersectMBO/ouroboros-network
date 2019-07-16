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
import           Data.Proxy (Proxy)

import           Ouroboros.Network.Block (BlockNo, ChainHash (..), HeaderHash,
                     SlotNo)
import           Ouroboros.Network.BlockFetch (SizeInBytes)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Byron
import           Ouroboros.Consensus.Mempool
import           Ouroboros.Consensus.Protocol.Abstract

import           Ouroboros.Storage.Common (EpochNo, EpochSize)

class (ProtocolLedgerView blk, ApplyTx blk) => RunNode blk where

  nodeForgeBlock         :: (HasNodeState (BlockProtocol blk) m, MonadRandom m)
                         => NodeConfig (BlockProtocol blk)
                         -> SlotNo         -- ^ Current slot
                         -> BlockNo        -- ^ Current block number
                         -> ChainHash blk  -- ^ Previous hash
                         -> [GenTx blk]    -- ^ Txs to add in the block
                         -> IsLeader (BlockProtocol blk)
                         -> m blk

  nodeBlockMatchesHeader :: Header blk -> blk -> Bool
  nodeBlockFetchSize     :: Header blk -> SizeInBytes
  nodeIsEBB              :: blk -> Bool
  nodeEpochSize          :: Monad m
                         => Proxy blk  -> EpochNo -> m EpochSize

  -- Encoders
  nodeEncodeBlock        :: NodeConfig (BlockProtocol blk) -> blk -> Encoding
  nodeEncodeHeader       :: NodeConfig (BlockProtocol blk) -> Header blk -> Encoding
  nodeEncodeGenTx        :: GenTx  blk -> Encoding
  nodeEncodeGenTxId      :: GenTxId blk -> Encoding
  nodeEncodeHeaderHash   :: Proxy blk -> HeaderHash blk -> Encoding
  nodeEncodeLedgerState  :: NodeConfig (BlockProtocol blk) -> LedgerState blk -> Encoding
  nodeEncodeChainState   :: Proxy blk -> ChainState (BlockProtocol blk) -> Encoding


  -- Decoders
  nodeDecodeHeader       :: forall s. NodeConfig (BlockProtocol blk) -> Decoder s (Header blk)
  nodeDecodeBlock        :: forall s. NodeConfig (BlockProtocol blk) -> Decoder s blk
  nodeDecodeGenTx        :: forall s. Decoder s (GenTx blk)
  nodeDecodeGenTxId      :: forall s. Decoder s (GenTxId blk)
  nodeDecodeHeaderHash   :: forall s. Proxy blk -> Decoder s (HeaderHash blk)
  nodeDecodeLedgerState  :: forall s. NodeConfig (BlockProtocol blk) -> Decoder s (LedgerState blk)
  nodeDecodeChainState   :: forall s. Proxy blk -> Decoder s (ChainState (BlockProtocol blk))
