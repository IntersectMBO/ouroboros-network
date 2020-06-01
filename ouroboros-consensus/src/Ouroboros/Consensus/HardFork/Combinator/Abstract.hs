{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableSuperClasses    #-}

module Ouroboros.Consensus.HardFork.Combinator.Abstract (
    -- * Era info
    SingleEraInfo(..)
  , LedgerEraInfo(..)
    -- * SingleEraBlock
  , SingleEraBlock(..)
  , proxySingle
  , singleEraTransition'
    -- * CanHardFork
  , CanHardFork(..)
    -- * Blocks without transitions
  , NoHardForks(..)
  , noHardForksEpochInfo
    -- * Re-exports
  , IsNonEmpty(..)
  , ProofNonEmpty(..)
  ) where

import           Codec.Serialise (Serialise)
import           Data.Proxy
import           Data.SOP.Strict
import           Data.Text (Text)
import           Data.Typeable
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks (..))
import           Cardano.Slotting.EpochInfo
import           Cardano.Slotting.Slot

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HardFork.History (EraParams)
import           Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util.SOP

import           Ouroboros.Consensus.HardFork.Combinator.PartialConfig
import           Ouroboros.Consensus.HardFork.Combinator.Translation

{-------------------------------------------------------------------------------
  Era info
-------------------------------------------------------------------------------}

-- | Information about an era (mostly for type errors)
data SingleEraInfo blk = SingleEraInfo {
      singleEraName :: !Text
    }
  deriving stock    (Generic, Eq, Show)
  deriving anyclass (NoUnexpectedThunks, Serialise)

-- | Additional newtype wrapper around 'SingleEraInfo'
--
-- This is primarily useful for use in error messages: it marks which era
-- info came from the ledger, and which came from a tx/block/header/etc.
newtype LedgerEraInfo blk = LedgerEraInfo {
      getLedgerEraInfo :: SingleEraInfo blk
    }
  deriving stock   (Eq, Show)
  deriving newtype (NoUnexpectedThunks, Serialise)

{-------------------------------------------------------------------------------
  SingleEraBlock
-------------------------------------------------------------------------------}

-- | Blocks from which we can assemble a hard fork
class ( LedgerSupportsProtocol blk
      , LedgerSupportsMempool blk
      , HasTxId (GenTx blk)
      , QueryLedger blk
      , CanForge blk
      , HasPartialConsensusConfig (BlockProtocol blk)
      , HasPartialLedgerConfig blk
      , ConvertRawHash blk
        -- Instances required to support testing
      , Show blk
      , Show (Header blk)
      ) => SingleEraBlock blk where

  -- | Era transition
  --
  -- This should only report the transition point once it is stable (rollback
  -- cannot affect it anymore).
  --
  -- This takes the partial config rather than the full config for the same
  -- reason as 'singleEraParam'.
  singleEraTransition :: PartialLedgerConfig blk -> LedgerState blk -> Maybe EpochNo

  -- | Era information (for use in error messages)
  singleEraInfo       :: proxy blk -> SingleEraInfo blk

proxySingle :: Proxy SingleEraBlock
proxySingle = Proxy

singleEraTransition' :: SingleEraBlock blk
                     => WrapPartialLedgerConfig blk
                     -> LedgerState blk -> Maybe EpochNo
singleEraTransition' = singleEraTransition . unwrapPartialLedgerConfig

{-------------------------------------------------------------------------------
  CanHardFork
-------------------------------------------------------------------------------}

class (All SingleEraBlock xs, Typeable xs, IsNonEmpty xs) => CanHardFork xs where
  hardForkEraTranslation :: EraTranslation xs

instance SingleEraBlock blk => CanHardFork '[blk] where
  hardForkEraTranslation = trivialEraTranslation

{-------------------------------------------------------------------------------
  Blocks that don't /have/ any transitions
-------------------------------------------------------------------------------}

class SingleEraBlock blk => NoHardForks blk where
  -- | Extract 'EraParams' from the top-level config
  --
  -- The HFC itself does not care about this, as it must be given the full shape
  -- across /all/ eras.
  getEraParams :: TopLevelConfig blk -> EraParams

  -- | Construct partial consensus config from full consensus config
  --
  -- NOTE: This is basically just losing 'EpochInfo', but that is constant
  -- anyway when we are dealing with a single era.
  toPartialConsensusConfig :: proxy blk
                           -> ConsensusConfig (BlockProtocol blk)
                           -> PartialConsensusConfig (BlockProtocol blk)

  -- | Construct partial ledger config from full ledger config
  --
  -- See also 'toPartialConsensusConfig'
  toPartialLedgerConfig :: proxy blk
                        -> LedgerConfig blk -> PartialLedgerConfig blk

noHardForksEpochInfo :: NoHardForks blk
                     => TopLevelConfig blk -> EpochInfo Identity
noHardForksEpochInfo = fixedSizeEpochInfo . History.eraEpochSize . getEraParams
