{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DerivingVia     #-}
{-# LANGUAGE RecordWildCards #-}

module Ouroboros.Storage.LedgerDB.Conf (
    LedgerDbConf(..)
    -- * Support for tests
  , PureLedgerDbConf
  , pureLedgerDbConf
  ) where

import           Data.Functor.Identity
import           Data.Void

import           Cardano.Prelude (NoUnexpectedThunks, OnlyCheckIsWHNF (..))

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
      ldbConfGenesis :: m l

      -- | Apply a block (passed by value)
    , ldbConfApply   :: b -> l -> Either e l

      -- | Apply a previously applied block
      --
      -- Since a block can only be applied to a single, specific, ledger state,
      -- if we apply a previously applied block again it will be applied in the
      -- very same ledger state, and therefore can't possibly fail.
      --
      -- NOTE: During testing it might be useful to implement 'ledgerDbReapply'
      -- using the code that /does/ do all checks, and throw an exception if
      -- any of them fail (as this would indicate a bug in the code somewhere).
      -- We do not have sufficient context in the ledger DB to /verify/ that
      -- when we reapply a block we are applying it to the correct ledger.
    , ldbConfReapply :: b -> l -> l

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
    , ldbConfResolve :: r -> m b
    }
  deriving NoUnexpectedThunks via OnlyCheckIsWHNF "LedgerDbConf" (LedgerDbConf m l r b e)

{-------------------------------------------------------------------------------
  Support for tests
-------------------------------------------------------------------------------}

-- | Ledger callbacks with no errors and where blocks are always passed by value
type PureLedgerDbConf l b = LedgerDbConf Identity l b b Void

pureLedgerDbConf :: l -> (b -> l -> l) -> PureLedgerDbConf l b
pureLedgerDbConf l f = LedgerDbConf {
      ldbConfGenesis = Identity l
    , ldbConfResolve = Identity
    , ldbConfApply   = Right .: f
    , ldbConfReapply = f
    }
