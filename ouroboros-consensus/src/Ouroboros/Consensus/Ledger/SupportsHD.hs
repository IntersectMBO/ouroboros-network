{-# LANGUAGE ConstraintKinds #-}
-- |

module Ouroboros.Consensus.Ledger.SupportsHD (LedgerSupportsHD) where

import           Ouroboros.Consensus.Ledger.Tables

type LedgerSupportsHD l =
  ( HasTickedLedgerTables l
  , CanSerializeLedgerTables l
  , CanStowLedgerTables l
  )
