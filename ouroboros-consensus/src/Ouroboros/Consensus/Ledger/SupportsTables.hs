{-# LANGUAGE ConstraintKinds #-}

-- | Collects the constraints on a ledger (state) that satisfies the `Tables`
-- classes
module Ouroboros.Consensus.Ledger.SupportsTables (LedgerSupportsTables) where

import           Ouroboros.Consensus.Ledger.Tables

type LedgerSupportsTables l =
  ( HasTickedLedgerTables l
  , CanSerializeLedgerTables l
  , CanStowLedgerTables l
  )
