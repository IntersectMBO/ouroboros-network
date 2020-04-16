{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DerivingVia     #-}
{-# LANGUAGE RecordWildCards #-}

module Ouroboros.Consensus.Storage.LedgerDB.Conf (
    LedgerDbConf(..)
  ) where

import           Cardano.Prelude (NoUnexpectedThunks, OnlyCheckIsWHNF (..))

{-------------------------------------------------------------------------------
  Callbacks required by the ledger DB
-------------------------------------------------------------------------------}

-- | Callbacks required by the ledger DB
data LedgerDbConf l b e = LedgerDbConf {
      -- | Apply a block (passed by value)
      ldbConfApply   :: b -> l -> Either e l

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
    }
  deriving NoUnexpectedThunks via OnlyCheckIsWHNF "LedgerDbConf" (LedgerDbConf l b e)
