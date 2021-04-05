{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications  #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Cardano.ByronHFC (ByronBlockHFC) where

import qualified Data.Map.Strict as Map
import           Data.SOP.Strict

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Storage.Serialisation

import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.Degenerate
import           Ouroboros.Consensus.HardFork.Combinator.Serialisation.Common

import           Ouroboros.Consensus.Byron.Ledger
import           Ouroboros.Consensus.Byron.Node ()

import           Ouroboros.Consensus.Cardano.CanHardFork
import           Ouroboros.Consensus.Cardano.Node ()

{-------------------------------------------------------------------------------
  Synonym for convenience
-------------------------------------------------------------------------------}

-- | Byron as the single era in the hard fork combinator
type ByronBlockHFC = HardForkBlock '[ByronBlock]

{-------------------------------------------------------------------------------
  NoHardForks instance
-------------------------------------------------------------------------------}

instance NoHardForks ByronBlock where
  getEraParams cfg =
      byronEraParamsNeverHardForks (byronGenesisConfig (configBlock cfg))
  toPartialConsensusConfig _ = id
  toPartialLedgerConfig _ cfg = ByronPartialLedgerConfig {
        byronLedgerConfig    = cfg
      , byronTriggerHardFork = TriggerHardForkNever
      }

{-------------------------------------------------------------------------------
  SupportedNetworkProtocolVersion instance
-------------------------------------------------------------------------------}

-- | Forward to the ByronBlock instance. Only supports
-- 'HardForkNodeToNodeDisabled', which is compatible with nodes running with
-- 'ByronBlock'.
instance SupportedNetworkProtocolVersion ByronBlockHFC where
  supportedNodeToNodeVersions _ =
      Map.map HardForkNodeToNodeDisabled $
      supportedNodeToNodeVersions (Proxy @ByronBlock)

  supportedNodeToClientVersions _ =
      Map.map HardForkNodeToClientDisabled $
      supportedNodeToClientVersions (Proxy @ByronBlock)

  latestReleasedNodeVersion = latestReleasedNodeVersionDefault

{-------------------------------------------------------------------------------
  SerialiseHFC instance
-------------------------------------------------------------------------------}

-- | Forward to the ByronBlock instance, this means we don't add an era
-- wrapper around blocks on disk. This makes sure we're compatible with the
-- existing Byron blocks.
instance SerialiseHFC '[ByronBlock] where
  encodeDiskHfcBlock (DegenCodecConfig ccfg) (DegenBlock b) =
      encodeDisk ccfg b
  decodeDiskHfcBlock (DegenCodecConfig ccfg) =
      fmap DegenBlock <$> decodeDisk ccfg
  reconstructHfcPrefixLen _ =
      reconstructPrefixLen (Proxy @(Header ByronBlock))
  reconstructHfcNestedCtxt _ prefix blockSize =
      mapSomeNestedCtxt NCZ $
        reconstructNestedCtxt (Proxy @(Header ByronBlock)) prefix blockSize
  getHfcBinaryBlockInfo (DegenBlock b) =
      getBinaryBlockInfo b
