{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
-- |

module Ouroboros.Consensus.Ledger.SupportsUTxOHD (
    LedgerMustSupportUTxOHD
  , LedgerSupportsUTxOHD
  ) where

import           NoThunks.Class
import           Ouroboros.Consensus.Ledger.Basics
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Ticked

class ( TickedTableStuff                          l     wt
      , StowableLedgerTables                      l     wt
      , SufficientSerializationForAnyBackingStore l     wt
      , GetsBlockKeySets                          l blk wt
      , IgnoresMapKind                            l
      , IgnoresMapKindTicked                      l
      , GetTip (l wt TrackingMK)
      , GetTip (l wt EmptyMK)
      , GetTip (l wt ValuesMK)
      , GetTip (Ticked1 (l wt) TrackingMK)
      , GetTip (Ticked1 (l wt) ValuesMK)
      , GetTip (Ticked1 (l wt) EmptyMK)
      , NoThunks (LedgerTables l wt ValuesMK)  -- for backing store inmem
      , NoThunks (LedgerTables l wt SeqDiffMK) -- for DBChangelog
      , NoThunks              (l wt EmptyMK)   -- for DbChangelog
      , ExtractLedgerTables l
      ) => LedgerMustSupportUTxOHD l blk wt where

type LedgerSupportsUTxOHD l blk = (forall (wt :: SwitchLedgerTables). LedgerMustSupportUTxOHD l blk wt)
