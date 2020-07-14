{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TypeApplications  #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Cardano.ByronHFC (
    ByronBlockHFC
  , HardForkBlock (ByronBlockHFC)
  , Header (ByronHeaderHFC)
  , CodecConfig (ByronHFCCodecConfig)
  , BlockConfig (ByronHFCBlockConfig)
  , HardForkLedgerConfig (ByronHFCLedgerConfig)
  , ConsensusConfig (ByronHFCConsensusConfig)
  , HardForkNodeToNodeVersion (ByronHFCNodeToNodeVersion1, ByronHFCNodeToNodeVersion2)
  , HardForkNodeToClientVersion (ByronHFCNodeToClientVersion1)
  ) where

import           Data.Functor.Contravariant (contramap)
import qualified Data.Map.Strict as Map
import           Data.SOP.Strict

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Storage.ChainDB.Serialisation

import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.Serialisation.Common

import           Ouroboros.Consensus.Byron.Ledger
import           Ouroboros.Consensus.Byron.Node ()

import           Ouroboros.Consensus.Cardano.CanHardFork
import           Ouroboros.Consensus.Cardano.Node ()

{-------------------------------------------------------------------------------
  Synonyms for convenience
-------------------------------------------------------------------------------}

-- | Byron as the single era in the hard fork combinator
type ByronBlockHFC = HardForkBlock '[ByronBlock]

pattern ByronBlockHFC :: ByronBlock -> ByronBlockHFC
pattern ByronBlockHFC b = HardForkBlock (OneEraBlock (Z (I b)))

{-# COMPLETE ByronBlockHFC #-}

pattern ByronHeaderHFC :: Header ByronBlock -> Header ByronBlockHFC
pattern ByronHeaderHFC h = HardForkHeader (OneEraHeader (Z h))

{-# COMPLETE ByronHeaderHFC #-}

pattern ByronHFCCodecConfig ::
     CodecConfig ByronBlock
  -> CodecConfig ByronBlockHFC
pattern ByronHFCCodecConfig ccfg =
    HardForkCodecConfig (PerEraCodecConfig (ccfg :* Nil))

{-# COMPLETE ByronHFCCodecConfig #-}

pattern ByronHFCBlockConfig ::
     BlockConfig ByronBlock
  -> BlockConfig ByronBlockHFC
pattern ByronHFCBlockConfig bcfg =
    HardForkBlockConfig (PerEraBlockConfig (bcfg :* Nil))

{-# COMPLETE ByronHFCBlockConfig #-}

pattern ByronHFCConsensusConfig ::
     PartialConsensusConfig (BlockProtocol ByronBlock)
  -> ConsensusConfig (BlockProtocol ByronBlockHFC)
pattern ByronHFCConsensusConfig ccfg <-
    HardForkConsensusConfig {
        hardForkConsensusConfigPerEra = PerEraConsensusConfig
          (WrapPartialConsensusConfig ccfg :* Nil)
      }

{-# COMPLETE ByronHFCConsensusConfig #-}

pattern ByronHFCLedgerConfig ::
     PartialLedgerConfig ByronBlock
  -> HardForkLedgerConfig '[ByronBlock]
pattern ByronHFCLedgerConfig lcfg <-
    HardForkLedgerConfig {
        hardForkLedgerConfigPerEra = PerEraLedgerConfig
          (WrapPartialLedgerConfig lcfg :* Nil)
      }

{-# COMPLETE ByronHFCLedgerConfig #-}


{-------------------------------------------------------------------------------
  NoHardForks instance
-------------------------------------------------------------------------------}

instance NoHardForks ByronBlock where
  getEraParams cfg =
      byronEraParams
        HardFork.NoLowerBound
        (byronGenesisConfig (configBlock cfg))
  toPartialConsensusConfig _ = id
  toPartialLedgerConfig _ cfg = ByronPartialLedgerConfig {
        byronLedgerConfig = cfg
      , triggerHardFork   = undefined
      }

{-------------------------------------------------------------------------------
  SupportedNetworkProtocolVersion instance
-------------------------------------------------------------------------------}

pattern ByronHFCNodeToNodeVersion1 :: HardForkNodeToNodeVersion '[ByronBlock]
pattern ByronHFCNodeToNodeVersion1 = HardForkNodeToNodeDisabled ByronNodeToNodeVersion1

pattern ByronHFCNodeToNodeVersion2 :: HardForkNodeToNodeVersion '[ByronBlock]
pattern ByronHFCNodeToNodeVersion2 = HardForkNodeToNodeDisabled ByronNodeToNodeVersion2

pattern ByronHFCNodeToClientVersion1 :: HardForkNodeToClientVersion '[ByronBlock]
pattern ByronHFCNodeToClientVersion1 = HardForkNodeToClientDisabled ByronNodeToClientVersion1

instance SupportedNetworkProtocolVersion ByronBlockHFC where
  supportedNodeToNodeVersions _ = Map.fromList [
        (NodeToNodeV_1, ByronHFCNodeToNodeVersion1)
        -- TODO enable V_2?
      ]

  supportedNodeToClientVersions _ = Map.fromList [
        (NodeToClientV_1, ByronHFCNodeToClientVersion1)
      , (NodeToClientV_2, ByronHFCNodeToClientVersion1)
        -- TODO enable V_3?
      ]

{-------------------------------------------------------------------------------
  SerialiseHFC instance
-------------------------------------------------------------------------------}

instance SerialiseHFC '[ByronBlock] where
  encodeDiskHfcBlock (ByronHFCCodecConfig ccfg) (ByronBlockHFC b) =
      encodeDisk ccfg b
  decodeDiskHfcBlock (ByronHFCCodecConfig ccfg) =
      fmap ByronBlockHFC <$> decodeDisk ccfg

  reconstructHfcPrefixLen _ = reconstructPrefixLen (Proxy @(Header ByronBlock))

  reconstructHfcNestedCtxt _ prefix blockSize =
      mapSomeNestedCtxt NCZ $
        reconstructNestedCtxt (Proxy @(Header ByronBlock)) prefix blockSize

{-------------------------------------------------------------------------------
  Project TopLevelConfig
-------------------------------------------------------------------------------}

-- | We are lucky that for Byron we can construct all the full configs from
-- the partial ones, which means we can reconstruct the 'TopLevelConfig' for
-- Byron.
projTopLevelConfig
  :: TopLevelConfig ByronBlockHFC
  -> TopLevelConfig ByronBlock
projTopLevelConfig cfg = byronCfg
  where
    TopLevelConfig {
        topLevelConfigProtocol = FullProtocolConfig {
            protocolConfigConsensus = ByronHFCConsensusConfig byronConsensusCfg
          }
      , topLevelConfigBlock = FullBlockConfig {
            blockConfigBlock  = ByronHFCBlockConfig  byronBlockCfg
          , blockConfigLedger = ByronHFCLedgerConfig byronLedgerCfg
          , blockConfigCodec  = ByronHFCCodecConfig  byronCodecCfg
          }
      } = cfg

    byronCfg :: TopLevelConfig ByronBlock
    byronCfg = TopLevelConfig {
        topLevelConfigProtocol = FullProtocolConfig {
            protocolConfigConsensus = byronConsensusCfg
          , protocolConfigIndep     = ()
          }
      , topLevelConfigBlock = FullBlockConfig {
            blockConfigBlock  = byronBlockCfg
          , blockConfigLedger = byronLedgerConfig byronLedgerCfg
          , blockConfigCodec  = byronCodecCfg
          }
      }

{-------------------------------------------------------------------------------
  RunNode instance
-------------------------------------------------------------------------------}

-- | Forward everything to ByronBlock
instance RunNode ByronBlockHFC where
  nodeBlockFetchSize (ByronHeaderHFC h) = nodeBlockFetchSize h

  nodeImmDbChunkInfo cfg = nodeImmDbChunkInfo (projTopLevelConfig cfg)

  nodeInitChainDB cfg initChainDB =
      nodeInitChainDB
        (projTopLevelConfig cfg)
        (contramap ByronBlockHFC initChainDB)

  nodeCheckIntegrity cfg (ByronBlockHFC b) =
      nodeCheckIntegrity (projTopLevelConfig cfg) b

  nodeGetBinaryBlockInfo (ByronBlockHFC b) = nodeGetBinaryBlockInfo b
