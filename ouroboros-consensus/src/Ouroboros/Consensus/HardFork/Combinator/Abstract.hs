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
  , singleEraParams'
  , singleEraTransition'
    -- * CanHardFork
  , CanHardFork(..)
    -- * Re-exports
  , IsNonEmpty(..)
  , ProofNonEmpty(..)
  ) where

import qualified Data.ByteString as Strict
import           Data.Proxy
import           Data.SOP.Strict
import           Data.Text (Text)
import           Data.Typeable
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks (..))
import           Cardano.Slotting.Slot

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HardFork.History (EraParams)
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Mempool.API

import           Ouroboros.Consensus.HardFork.Combinator.PartialConfig
import           Ouroboros.Consensus.HardFork.Combinator.Translation
import           Ouroboros.Consensus.HardFork.Combinator.Util.SOP

{-------------------------------------------------------------------------------
  Era info
-------------------------------------------------------------------------------}

-- | Information about an era (mostly for type errors)
data SingleEraInfo blk = SingleEraInfo {
      singleEraName :: !Text
    }
  deriving stock    (Generic, Eq, Show)
  deriving anyclass (NoUnexpectedThunks)

-- | Additional newtype wrapper around 'SingleEraInfo'
--
-- This is primarily useful for use in error messages: it marks which era
-- info came from the ledger, and which came from a tx/block/header/etc.
newtype LedgerEraInfo blk = LedgerEraInfo {
      getLedgerEraInfo :: SingleEraInfo blk
    }
  deriving stock   (Eq, Show)
  deriving newtype (NoUnexpectedThunks)

{-------------------------------------------------------------------------------
  SingleEraBlock
-------------------------------------------------------------------------------}

-- | Blocks from which we can assemble a hard fork
class ( LedgerSupportsProtocol blk
      , ApplyTx blk
      , HasTxId (GenTx blk)
      , QueryLedger blk
      , CanForge blk
      , HasPartialConsensusConfig (BlockProtocol blk)
      , HasPartialLedgerConfig blk
        -- Instances required to support testing
      , Show blk
      , Show (Header blk)
      ) => SingleEraBlock blk where
  -- | Era parameters
  --
  -- The era parameters must be static (cannot depend on the ledger state).
  --
  -- Since we ne need this to construct the 'HardForkSummary' (and hence the
  -- 'EpochInfo', this takes the /partial/ config, not the full config
  -- (or we'd end up in a catch-22).
  singleEraParams     :: proxy blk -> PartialLedgerConfig blk -> EraParams

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

  -- | Get the raw hash
  --
  -- See documentation of 'OneEraHash' for rationale.
  getRawHash :: proxy blk -> HeaderHash blk -> Strict.ByteString

proxySingle :: Proxy SingleEraBlock
proxySingle = Proxy

singleEraParams' :: forall blk. SingleEraBlock blk
                 => WrapPartialLedgerConfig blk -> EraParams
singleEraParams' = singleEraParams (Proxy @blk) . unwrapPartialLedgerConfig

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
