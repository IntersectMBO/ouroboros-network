{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}

-- | Additional type family newtype wrappers
module Ouroboros.Consensus.HardFork.Combinator.SingleEra.Wrappers (
    -- * Partial config
    WrapPartialLedgerConfig(..)
  , WrapPartialConsensusConfig(..)
    -- * Convenience
  , singleEraParams'
  , singleEraTransition'
  ) where

import           Data.Proxy

import           Cardano.Prelude (NoUnexpectedThunks)
import           Cardano.Slotting.Slot

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HardFork.History (EraParams)
import           Ouroboros.Consensus.Ledger.Abstract

import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.PartialConfig

{-------------------------------------------------------------------------------
  Additional type family wrappers
-------------------------------------------------------------------------------}

newtype WrapPartialLedgerConfig    blk = WrapPartialLedgerConfig    { unwrapPartialLedgerConfig    :: PartialLedgerConfig                   blk  }
newtype WrapPartialConsensusConfig blk = WrapPartialConsensusConfig { unwrapPartialConsensusConfig :: PartialConsensusConfig (BlockProtocol blk) }

{-------------------------------------------------------------------------------
  Convenience functions
-------------------------------------------------------------------------------}

singleEraParams' :: forall blk. SingleEraBlock blk
                 => WrapPartialLedgerConfig blk -> EraParams
singleEraParams' = singleEraParams (Proxy @blk) . unwrapPartialLedgerConfig

singleEraTransition' :: SingleEraBlock blk
                     => WrapPartialLedgerConfig blk -> LedgerState blk -> Maybe EpochNo
singleEraTransition' = singleEraTransition . unwrapPartialLedgerConfig

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

deriving instance SingleEraBlock blk => NoUnexpectedThunks (WrapPartialConsensusConfig blk)
deriving instance SingleEraBlock blk => NoUnexpectedThunks (WrapPartialLedgerConfig    blk)
