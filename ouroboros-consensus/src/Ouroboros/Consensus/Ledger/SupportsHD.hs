{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module Ouroboros.Consensus.Ledger.SupportsHD (LedgerSupportsHD) where

import           Ouroboros.Consensus.Ledger.Basics
import           Ouroboros.Consensus.Ledger.Tables

-- | The constraints required for a ledger state to be able to manage ledger
-- tables.
--
-- NOTE: The HardForkBlock doesn't satisfy this constraint because it doesn't
-- have tables, as the tables for HardForkBlocks are not defined compositionally
-- but rather as an mk of NS's. Because of this, we cannot say that a
-- @'SingleEraBlock'@ supports HD and then get the HardForkBlock instance for
-- free, as we do with the rest of the constraints in @'SingleEraBlock'@.
type LedgerSupportsHD blk =
      ( HasTickedLedgerTables (LedgerState blk)
      , CanSerializeLedgerTables (LedgerState blk)
      , CanStowLedgerTables (LedgerState blk)
      )
