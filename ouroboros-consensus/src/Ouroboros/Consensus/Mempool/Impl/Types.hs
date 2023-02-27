{-# LANGUAGE FlexibleContexts #-}
module Ouroboros.Consensus.Mempool.Impl.Types {-# DEPRECATED "Use Ouroboros.Consensus.Mempool instead" #-} (
    -- * Internal State
    InternalState (..)
  , initInternalState
  , isMempoolSize
    -- * Validation result
  , ValidationResult (..)
  , extendVRNew
  , extendVRPrevApplied
  , revalidateTxsFor
  , validateIS
  , validateStateFor
    -- * Tick ledger state
  , tickLedgerState
    -- * Conversions
  , internalStateFromVR
  , validationResultFromIS
  ) where

import           Ouroboros.Consensus.Ledger.Basics
import           Ouroboros.Consensus.Ledger.SupportsMempool

import           Ouroboros.Consensus.Mempool.Capacity
import           Ouroboros.Consensus.Mempool.Impl.Common

{-# DEPRECATED validateIS "This function should not be used (it will throw an error), it was internal no longer exists" #-}
validateIS
  :: InternalState blk
  -> LedgerState blk
  -> LedgerConfig blk
  -> MempoolCapacityBytesOverride
  -> ValidationResult (Validated (GenTx blk)) blk
validateIS = error "Deprecated"
