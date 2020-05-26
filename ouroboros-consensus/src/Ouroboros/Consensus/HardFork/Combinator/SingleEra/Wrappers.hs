{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}

-- | Additional type family newtype wrappers
module Ouroboros.Consensus.HardFork.Combinator.SingleEra.Wrappers (
    -- * Convenience
    singleEraParams'
  , singleEraTransition'
  ) where

import           Data.Proxy

import           Cardano.Slotting.Slot

import           Ouroboros.Consensus.HardFork.History (EraParams)
import           Ouroboros.Consensus.Ledger.Abstract

import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.PartialConfig

{-------------------------------------------------------------------------------
  Convenience functions
-------------------------------------------------------------------------------}

singleEraParams' :: forall blk. SingleEraBlock blk
                 => WrapPartialLedgerConfig blk -> EraParams
singleEraParams' = singleEraParams (Proxy @blk) . unwrapPartialLedgerConfig

singleEraTransition' :: SingleEraBlock blk
                     => WrapPartialLedgerConfig blk -> LedgerState blk -> Maybe EpochNo
singleEraTransition' = singleEraTransition . unwrapPartialLedgerConfig
