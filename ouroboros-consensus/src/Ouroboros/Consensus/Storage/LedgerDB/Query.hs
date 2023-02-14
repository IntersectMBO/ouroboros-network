{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE DeriveAnyClass           #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE QuantifiedConstraints    #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UndecidableInstances     #-}

module Ouroboros.Consensus.Storage.LedgerDB.Query (
    ledgerDbAnchor
  , ledgerDbCurrent
  , ledgerDbIsSaturated
  , ledgerDbLastFlushedState
  , ledgerDbMaxRollback
  , ledgerDbPast
  , ledgerDbPrefix
  , ledgerDbSnapshots
  , ledgerDbTip
  ) where

import           Data.Word

import qualified Ouroboros.Network.AnchoredSeq as AS

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsTables

import           Ouroboros.Consensus.Storage.LedgerDB.DbChangelog
import           Ouroboros.Consensus.Storage.LedgerDB.LedgerDB

-- | The ledger state at the tip of the chain
ledgerDbCurrent :: GetTip l => LedgerDB l -> l EmptyMK
ledgerDbCurrent =
    either unDbChangelogState unDbChangelogState
  . AS.head
  . changelogVolatileStates
  . ledgerDbChangelog

-- | Information about the state of the ledger at the anchor
ledgerDbAnchor :: LedgerDB l -> l EmptyMK
ledgerDbAnchor =
    unDbChangelogState
  . AS.anchor
  . changelogVolatileStates
  . ledgerDbChangelog

-- | Get the most recently flushed ledger state. This is what will be serialized
-- when snapshotting.
ledgerDbLastFlushedState :: LedgerDB l -> l EmptyMK
ledgerDbLastFlushedState =
    unDbChangelogState
  . AS.anchor
  . changelogImmutableStates
  . ledgerDbChangelog

-- | All snapshots currently stored by the ledger DB (new to old)
--
-- This also includes the snapshot at the anchor. For each snapshot we also
-- return the distance from the tip.
ledgerDbSnapshots :: LedgerDB l -> [(Word64, l EmptyMK)]
ledgerDbSnapshots =
      zip [0..]
    . map unDbChangelogState
    . AS.toNewestFirst
    . changelogVolatileStates
    . ledgerDbChangelog

-- | How many blocks can we currently roll back?
ledgerDbMaxRollback :: GetTip l => LedgerDB l -> Word64
ledgerDbMaxRollback =
    fromIntegral
  . AS.length
  . changelogVolatileStates
  . ledgerDbChangelog

-- | Reference to the block at the tip of the chain
ledgerDbTip :: GetTip l => LedgerDB l -> Point l
ledgerDbTip = castPoint . getTip . ledgerDbCurrent

-- | Have we seen at least @k@ blocks?
ledgerDbIsSaturated :: GetTip l => SecurityParam -> LedgerDB l -> Bool
ledgerDbIsSaturated (SecurityParam k) db =
    ledgerDbMaxRollback db >= k

-- | Get a past ledger state
--
--  \( O(\log(\min(i,n-i)) \)
--
-- When no ledger state (or anchor) has the given 'Point', 'Nothing' is
-- returned.
ledgerDbPast ::
     ( HasHeader blk, IsLedger l, HeaderHash l ~ HeaderHash blk
     , StandardHash l, LedgerSupportsTables l
     )
  => Point blk
  -> LedgerDB l
  -> Maybe (l EmptyMK)
ledgerDbPast pt db = ledgerDbCurrent <$> ledgerDbPrefix pt db

-- | Get a prefix of the LedgerDB that ends at the given point
--
--  \( O(\log(\min(i,n-i)) \)
--
-- When no ledger state (or anchor) has the given 'Point', 'Nothing' is
-- returned.
ledgerDbPrefix ::
     ( HasHeader blk, IsLedger l, HeaderHash l ~ HeaderHash blk
     , StandardHash l, LedgerSupportsTables l
     )
  => Point blk
  -> LedgerDB l
  -> Maybe (LedgerDB l)
ledgerDbPrefix pt db
    | pt == castPoint (getTip (ledgerDbAnchor db))
    = Just . LedgerDB . rollbackToAnchor $ ledgerDbChangelog db
    | otherwise
    = LedgerDB <$> rollbackToPoint (castPoint pt) (ledgerDbChangelog db)
