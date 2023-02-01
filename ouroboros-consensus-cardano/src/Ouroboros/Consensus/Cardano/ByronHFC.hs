{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Cardano.ByronHFC (ByronBlockHFC) where

import qualified Data.Map.Strict as Map
import           Data.SOP.Strict
import           GHC.Generics (Generic)
import           NoThunks.Class

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Tables
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Storage.Serialisation

import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.Degenerate
import           Ouroboros.Consensus.HardFork.Combinator.Serialisation.Common
import           Ouroboros.Consensus.HardFork.Combinator.State.Types

import           Ouroboros.Consensus.Byron.Ledger hiding (LedgerTables (..))
import qualified Ouroboros.Consensus.Byron.Ledger as Byron (LedgerTables (..))
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

{-------------------------------------------------------------------------------
  Ledger Tables
-------------------------------------------------------------------------------}

instance HasLedgerTables (LedgerState ByronBlockHFC) where
  data LedgerTables (LedgerState ByronBlockHFC) mk = NoTables
    deriving (Generic, Eq, Show, NoThunks)

instance CanSerializeLedgerTables (LedgerState ByronBlockHFC) where

instance LedgerTablesAreTrivial (LedgerState ByronBlockHFC) where
  trivialLedgerTables = NoTables

instance HasTickedLedgerTables (LedgerState ByronBlockHFC) where
  withLedgerTablesTicked (TickedHardForkLedgerState t st) NoTables =
      TickedHardForkLedgerState t
    . HardForkState
    . (\(TZ (Current s (FlipTickedLedgerState state))) ->
         TZ (Current s (FlipTickedLedgerState $ withLedgerTablesTicked state Byron.NoTables)))
    . getHardForkState
    $ st

instance CanStowLedgerTables (LedgerState ByronBlockHFC) where
