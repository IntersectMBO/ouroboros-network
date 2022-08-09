{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- |

module Ouroboros.Consensus.Ledger.SupportsUTxOHD (
    LedgerMustSupportUTxOHD
    , LedgerSupportsUTxOHD
  ) where


import Ouroboros.Consensus.Ledger.Basics
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.Ticked
import NoThunks.Class

class ( LedgerMustSupportUTxOHD l blk WithLedgerTables
      , LedgerMustSupportUTxOHD l blk WithoutLedgerTables
      ) => LedgerSupportsUTxOHD l blk where

class ( TickedTableStuff                             (l blk)     wt
      , StowableLedgerTables                         (l blk)     wt
      , SufficientSerializationForAnyBackingStore    (l blk)     wt
      , GetsBlockKeySets                             (l blk) blk wt
      , IgnoresMapKind                                   (l blk)
      , IgnoresMapKindTicked                             (l blk)
      , GetTip (l blk wt TrackingMK)
      , GetTip (l blk wt EmptyMK)
      , GetTip (l blk wt ValuesMK)
      , GetTip (Ticked1 (l blk wt) TrackingMK)
      , GetTip (Ticked1 (l blk wt) ValuesMK)
      , GetTip (Ticked1 (l blk wt) EmptyMK)
      , NoThunks (LedgerTables (l blk) wt ValuesMK) -- for backing store inmem
      , NoThunks (l blk wt EmptyMK)                 -- for DbChangelog
      , NoThunks (LedgerTables (l blk) wt SeqDiffMK) -- for DBChangelog
      , IsSwitchLedgerTables wt
      , ExtractLedgerTables (l blk)
      ) => LedgerMustSupportUTxOHD l blk wt where
