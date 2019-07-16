{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Ouroboros.Consensus.Node.Run.Byron () where

import           Ouroboros.Consensus.Ledger.Byron
import           Ouroboros.Consensus.Node.Run.Abstract
import           Ouroboros.Consensus.Protocol.ExtNodeConfig

import           Ouroboros.Consensus.Ledger.Byron.Config
import           Ouroboros.Consensus.Ledger.Byron.Forge

{-------------------------------------------------------------------------------
  RunNode instance
-------------------------------------------------------------------------------}

instance ByronGiven => RunNode (ByronBlock ByronConfig) where
  nodeForgeBlock         = forgeBlock
  nodeBlockMatchesHeader = \_hdr _blk -> True -- TODO #595
  nodeBlockFetchSize     = const 2000 -- TODO #593
  nodeIsEBB              = const False -- TODO #704
  nodeEpochSize          = \_ _ -> return 21600 -- TODO #226

  nodeEncodeBlock        = encodeByronBlock  . pbftEpochSlots . encNodeConfigExt
  nodeEncodeHeader       = encodeByronHeader . pbftEpochSlots . encNodeConfigExt
  nodeEncodeGenTx        = encodeByronGenTx
  nodeEncodeGenTxId      = encodeByronGenTxId
  nodeEncodeHeaderHash   = const encodeByronHeaderHash
  nodeEncodeLedgerState  = const encodeByronLedgerState
  nodeEncodeChainState   = const encodeByronChainState

  nodeDecodeBlock        = decodeByronBlock  . pbftEpochSlots . encNodeConfigExt
  nodeDecodeHeader       = decodeByronHeader . pbftEpochSlots . encNodeConfigExt
  nodeDecodeGenTx        = decodeByronGenTx
  nodeDecodeGenTxId      = decodeByronGenTxId
  nodeDecodeHeaderHash   = const decodeByronHeaderHash
  nodeDecodeLedgerState  = const decodeByronLedgerState
  nodeDecodeChainState   = const decodeByronChainState
