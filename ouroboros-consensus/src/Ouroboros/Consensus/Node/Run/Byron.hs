{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Ouroboros.Consensus.Node.Run.Byron () where

import           Data.Coerce (coerce)

import qualified Cardano.Chain.Block as Cardano.Block
import qualified Cardano.Chain.Genesis as Genesis
import           Cardano.Chain.ProtocolConstants (kEpochSlots)
import           Cardano.Chain.Slotting (EpochSlots (..))
import qualified Cardano.Crypto as Crypto

import           Ouroboros.Network.Magic (NetworkMagic (..))

import           Ouroboros.Consensus.BlockchainTime (SystemStart (..))
import           Ouroboros.Consensus.Ledger.Byron
import           Ouroboros.Consensus.Node.Run.Abstract
import           Ouroboros.Consensus.Protocol.Abstract

import           Ouroboros.Storage.Common (EpochNo (..), EpochSize (..))

{-------------------------------------------------------------------------------
  RunNode instance
-------------------------------------------------------------------------------}

instance RunNode ByronBlock where
  nodeForgeBlock          = forgeByronBlock
  nodeBlockMatchesHeader  = byronBlockMatchesHeader
  nodeBlockFetchSize      = const 2000 -- TODO #593
  nodeIsEBB               = \blk -> case byronBlockRaw blk of
    Cardano.Block.BOBBlock _      -> Nothing
    Cardano.Block.BOBBoundary ebb -> Just
                                   . EpochNo
                                   . Cardano.Block.boundaryEpoch
                                   . Cardano.Block.boundaryHeader
                                   $ ebb

  -- The epoch size is fixed and can be derived from @k@ by the ledger
  -- ('kEpochSlots').
  nodeEpochSize           = \_proxy cfg _epochNo -> return
                              . (coerce :: EpochSlots -> EpochSize)
                              . kEpochSlots
                              . Genesis.gdK
                              . extractGenesisData
                              $ cfg

  -- Extract it from the 'Genesis.Config'
  nodeStartTime           = const
                          $ SystemStart
                          . Genesis.gdStartTime
                          . extractGenesisData
  nodeNetworkMagic        = const
                          $ NetworkMagic
                          . Crypto.unProtocolMagicId
                          . Genesis.gdProtocolMagicId
                          . extractGenesisData
  nodeProtocolMagicId     = const
                          $ Genesis.gdProtocolMagicId
                          . extractGenesisData
  nodeHashInfo            = const byronHashInfo


  nodeEncodeBlockWithInfo = const encodeByronBlockWithInfo
  nodeEncodeHeader        = const encodeByronHeader
  nodeEncodeGenTx         = encodeByronGenTx
  nodeEncodeGenTxId       = encodeByronGenTxId
  nodeEncodeHeaderHash    = const encodeByronHeaderHash
  nodeEncodeLedgerState   = const encodeByronLedgerState
  nodeEncodeChainState    = const encodeByronChainState
  nodeEncodeApplyTxError  = const encodeByronApplyTxError

  nodeDecodeBlock       c = Ann $ decodeByronBlock $ extractEpochSlots c
  nodeDecodeHeader      c = Ann $ decodeByronHeader $ extractEpochSlots c
  nodeDecodeGenTx         = Ann decodeByronGenTx
  nodeDecodeGenTxId       = Plain decodeByronGenTxId
  nodeDecodeHeaderHash    = const $ Plain decodeByronHeaderHash
  nodeDecodeLedgerState   = const $ Plain decodeByronLedgerState
  nodeDecodeChainState    = const $ Plain decodeByronChainState
  nodeDecodeApplyTxError  = const $ Ann decodeByronApplyTxError

extractGenesisData :: NodeConfig ByronConsensusProtocol -> Genesis.GenesisData
extractGenesisData = Genesis.configGenesisData . getGenesisConfig

extractEpochSlots :: NodeConfig ByronConsensusProtocol -> EpochSlots
extractEpochSlots = Genesis.configEpochSlots . getGenesisConfig
