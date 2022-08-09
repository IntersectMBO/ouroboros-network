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
      ) => LedgerMustSupportUTxOHD l blk wt where
