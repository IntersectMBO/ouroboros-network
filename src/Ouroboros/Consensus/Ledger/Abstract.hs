{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}

-- | Abstract definition of a ledger
module Ouroboros.Consensus.Ledger.Abstract (
    UpdateLedger(..)
  ) where

{-------------------------------------------------------------------------------
  Abstract over blocks
-------------------------------------------------------------------------------}

-- | The (open) universe of blocks
class UpdateLedger (b :: *) where
  data family LedgerState b :: *

  -- Apply a block to the ledger state
  --
  -- TODO: We need to support rollback, so this probably won't be a pure
  -- function but rather something that lives in a monad with some actions
  -- that we can compute a "running diff" so that we can go back in time.
  applyLedgerState  :: b -> LedgerState b -> LedgerState b
