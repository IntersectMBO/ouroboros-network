{-# LANGUAGE RecordWildCards #-}

module Ouroboros.Storage.LedgerDB.Conf (
    LedgerDbConf(..)
  , PrevApplied(..)
    -- * Support for tests
  , PureLedgerDbConf
  , pureLedgerDbConf
  ) where

import           Data.Functor.Identity
import           Data.Void

import           Ouroboros.Consensus.Util ((.:))

{-------------------------------------------------------------------------------
  Callbacks required by the ledger DB
-------------------------------------------------------------------------------}

-- | Callbacks required by the ledger DB
data LedgerDbConf m l r b e = LedgerDbConf {
      -- | Get the genesis ledger
      --
      -- This is monadic because constructing the ledger state involves reading
      -- configuration files etc. It is also rarely called.
      ledgerDbGenesis :: m l

      -- | Apply a block (passed by value)
    , ledgerDbApply   :: PrevApplied -> b -> l -> Either e l

      -- | Resolve a block
      --
      -- Resolving a block reference to the actual block lives in @m@ because
      -- it might need to read the block from disk (and can therefore not be
      -- done inside an STM transaction).
      --
      -- NOTE: The ledger DB will only ask the 'ChainDB' for blocks it knows
      -- must exist. If the 'ChainDB' is unable to fulfill the request, data
      -- corruption must have happened and the 'ChainStateDB' should trigger
      -- validation mode.
    , ledgerDbResolve :: r -> m b
    }

-- | Have we previously successfully applied this block to a ledger state?
data PrevApplied =
    -- | This block was previously applied
    --
    -- If we re-apply it to compute a new ledger state, expensive checks such as
    -- the verification of cryptographic primitives can be skipped. Indeed,
    -- a previously verified block /cannot/ result in a validation error.
    PrevApplied

    -- | Not previously applied
    --
    -- All checks must be performed
  | NotPrevApplied

{-------------------------------------------------------------------------------
  Support for tests
-------------------------------------------------------------------------------}

-- | Ledger callbacks with no errors and where blocks are always passed by value
type PureLedgerDbConf l b = LedgerDbConf Identity l b b Void

pureLedgerDbConf :: l -> (b -> l -> l) -> PureLedgerDbConf l b
pureLedgerDbConf l f = LedgerDbConf {
      ledgerDbGenesis = Identity l
    , ledgerDbResolve = Identity
    , ledgerDbApply   = \_pa -> Right .: f
    }
