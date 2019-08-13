{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Ouroboros.Consensus.Node.Run.Byron () where

import           Data.Reflection (given)

import qualified Cardano.Chain.Block as Cardano.Block
import qualified Cardano.Chain.Genesis as Genesis
import qualified Cardano.Chain.Update as PPrams

import           Ouroboros.Consensus.BlockchainTime (SystemStart (..), slotLengthFromMillisec)
import           Ouroboros.Consensus.Ledger.Byron
import           Ouroboros.Consensus.Ledger.Byron.Config
import           Ouroboros.Consensus.Ledger.Byron.Forge
import           Ouroboros.Consensus.Node.Run.Abstract
import           Ouroboros.Consensus.Protocol.ExtNodeConfig
import           Ouroboros.Consensus.Protocol.WithEBBs

{-------------------------------------------------------------------------------
  RunNode instance
-------------------------------------------------------------------------------}

instance ByronGiven => RunNode (ByronBlockOrEBB ByronConfig) where
  nodeForgeBlock         = forgeBlockOrEBB
  nodeBlockMatchesHeader = byronBlockOrEBBMatchesHeader
  nodeBlockFetchSize     = const 2000 -- TODO #593
  nodeIsEBB              = \blk -> case unByronBlockOrEBB blk of
    Cardano.Block.ABOBBlock _    -> False
    Cardano.Block.ABOBBoundary _ -> True
  nodeEpochSize          = \_ _ -> return 21600 -- TODO #226
  nodeSlotDuration       = const
                         $ slotLengthFromMillisec
                         . fromIntegral
                         . PPrams.ppSlotDuration
                         . Genesis.gdProtocolParameters
                         . Genesis.configGenesisData
                         . pbftGenesisConfig
                         . encNodeConfigExt
                         . unWithEBBNodeConfig

  -- Extract it from the 'Genesis.Config'
  nodeStartTime          = const
                         $ SystemStart
                         . Genesis.gdStartTime
                         . Genesis.configGenesisData
                         . pbftGenesisConfig
                         . encNodeConfigExt
                         . unWithEBBNodeConfig

  nodeEncodeBlock        = const encodeByronBlock
  nodeEncodeHeader       = const encodeByronHeader
  nodeEncodeGenTx        = encodeByronGenTx
  nodeEncodeGenTxId      = encodeByronGenTxId
  nodeEncodeHeaderHash   = const encodeByronHeaderHash
  nodeEncodeLedgerState  = const encodeByronLedgerState
  nodeEncodeChainState   = const encodeByronChainState
  nodeEncodeApplyTxError = const encodeByronApplyTxError

  nodeDecodeBlock        = const (decodeByronBlock given)
  nodeDecodeHeader       = const (decodeByronHeader given)
  nodeDecodeGenTx        = decodeByronGenTx
  nodeDecodeGenTxId      = decodeByronGenTxId
  nodeDecodeHeaderHash   = const decodeByronHeaderHash
  nodeDecodeLedgerState  = const decodeByronLedgerState
  nodeDecodeChainState   = const decodeByronChainState
  nodeDecodeApplyTxError = const decodeByronApplyTxError
