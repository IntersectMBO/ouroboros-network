{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- |

module Ouroboros.Consensus.Ledger.SupportsUTxOHD (
    LedgerMustSupportUTxOHD
  , LedgerSupportsUTxOHD
  ) where

import           NoThunks.Class
import           Ouroboros.Consensus.Ledger.Basics
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Ticked

class ( -- LedgerMustSupportUTxOHD l blk WithLedgerTables
      -- , LedgerMustSupportUTxOHD l blk WithoutLedgerTables
      ) => LedgerSupportsUTxOHD l blk where

class ( -- StowableLedgerTables                      tbs
      -- , SufficientSerializationForAnyBackingStore tbs
      -- , GetsBlockKeySets                          blk tbs
      -- , GetTip (l wt TrackingMK)
      -- , GetTip (l wt EmptyMK)
      -- , GetTip (l wt ValuesMK)
      -- , GetTip (Ticked1 (l wt) TrackingMK)
      -- , GetTip (Ticked1 (l wt) ValuesMK)
      -- , GetTip (Ticked1 (l wt) EmptyMK)
      -- , NoThunks (LedgerTables l wt ValuesMK)  -- for backing store inmem
      -- , NoThunks (LedgerTables l wt SeqDiffMK) -- for DBChangelog
      -- , NoThunks              (l wt EmptyMK)   -- for DbChangelog
      -- , IsSwitchLedgerTables wt
      -- , ExtractLedgerTables l
      ) => LedgerMustSupportUTxOHD l blk wt where
