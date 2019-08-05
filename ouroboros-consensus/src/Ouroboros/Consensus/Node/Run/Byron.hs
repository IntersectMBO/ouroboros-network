{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Ouroboros.Consensus.Node.Run.Byron () where

import           Data.Reflection (given)

import qualified Cardano.Chain.Block as Cardano.Block
import           Ouroboros.Consensus.Ledger.Byron
import           Ouroboros.Consensus.Node.Run.Abstract

import           Ouroboros.Consensus.Ledger.Byron.Config
import           Ouroboros.Consensus.Ledger.Byron.Forge

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
