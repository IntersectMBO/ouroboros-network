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
  , validateStateFor
    -- * Tick ledger state
  , tickLedgerState
    -- * Conversions
  , internalStateFromVR
  , validationResultFromIS
  ) where

import           Ouroboros.Consensus.Mempool.Impl.Common
