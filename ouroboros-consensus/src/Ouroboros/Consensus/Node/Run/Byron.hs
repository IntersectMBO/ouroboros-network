{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Ouroboros.Consensus.Node.Run.Byron () where

import           Data.Coerce (coerce)
import           Data.Reflection (given)

import qualified Cardano.Chain.Block as Cardano.Block
import qualified Cardano.Chain.Genesis as Genesis
import           Cardano.Chain.ProtocolConstants (kEpochSlots)
import           Cardano.Chain.Slotting (EpochSlots (..))

import           Ouroboros.Consensus.BlockchainTime (SystemStart (..))
import           Ouroboros.Consensus.Ledger.Byron
import           Ouroboros.Consensus.Ledger.Byron.Config
import           Ouroboros.Consensus.Ledger.Byron.Forge
import           Ouroboros.Consensus.Node.Run.Abstract
import           Ouroboros.Consensus.Protocol.ExtNodeConfig
import           Ouroboros.Consensus.Protocol.WithEBBs

import           Ouroboros.Storage.Common (EpochSize (..))

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

  -- The epoch size is fixed and can be derived from @k@ by the ledger
  -- ('kEpochSlots').
  nodeEpochSize          = \_proxy cfg _epochNo -> return
                             . (coerce :: EpochSlots -> EpochSize)
                             . kEpochSlots
                             . Genesis.gdK
                             . extractGenesisData
                             $ cfg

  -- Extract it from the 'Genesis.Config'
  nodeStartTime          = const
                         $ SystemStart
                         . Genesis.gdStartTime
                         . extractGenesisData

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

extractGenesisData :: NodeConfig (WithEBBs (ExtNodeConfig ByronConfig p))
                   -> Genesis.GenesisData
extractGenesisData = Genesis.configGenesisData
                   . pbftGenesisConfig
                   . encNodeConfigExt
                   . unWithEBBNodeConfig
