{-# LANGUAGE TypeFamilies #-}

module Ouroboros.Consensus.Storage.LedgerDB.Query (
    anchor
  , current
  , getPastLedgerAt
  , isSaturated
  , lastFlushedState
  , maxRollback
  , rollback
  , snapshots
  , tip
  ) where

import           Data.Word

import qualified Ouroboros.Network.AnchoredSeq as AS

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract

import           Ouroboros.Consensus.Storage.LedgerDB.DbChangelog
import           Ouroboros.Consensus.Storage.LedgerDB.LedgerDB

-- | The ledger state at the tip of the chain
current :: GetTip l => LedgerDB l -> l EmptyMK
current =
    either unDbChangelogState unDbChangelogState
  . AS.head
  . changelogVolatileStates
  . ledgerDbChangelog

-- | Information about the state of the ledger at the anchor
anchor :: LedgerDB l -> l EmptyMK
anchor =
    unDbChangelogState
  . AS.anchor
  . changelogVolatileStates
  . ledgerDbChangelog

-- | Get the most recently flushed ledger state. This is what will be serialized
-- when snapshotting.
lastFlushedState :: LedgerDB l -> l EmptyMK
lastFlushedState =
    unDbChangelogState
  . AS.anchor
  . changelogImmutableStates
  . ledgerDbChangelog

-- | All snapshots currently stored by the ledger DB (new to old)
--
-- This also includes the snapshot at the anchor. For each snapshot we also
-- return the distance from the tip.
snapshots :: LedgerDB l -> [(Word64, l EmptyMK)]
snapshots =
      zip [0..]
    . map unDbChangelogState
    . AS.toNewestFirst
    . changelogVolatileStates
    . ledgerDbChangelog

-- | How many blocks can we currently roll back?
maxRollback :: GetTip l => LedgerDB l -> Word64
maxRollback =
    fromIntegral
  . AS.length
  . changelogVolatileStates
  . ledgerDbChangelog

-- | Reference to the block at the tip of the chain
tip :: GetTip l => LedgerDB l -> Point l
tip = castPoint . getTip . current

-- | Have we seen at least @k@ blocks?
isSaturated :: GetTip l => SecurityParam -> LedgerDB l -> Bool
isSaturated (SecurityParam k) db =
    maxRollback db >= k

-- | Get a past ledger state
--
--  \( O(\log(\min(i,n-i)) \)
--
-- When no ledger state (or anchor) has the given 'Point', 'Nothing' is
-- returned.
getPastLedgerAt ::
     ( HasHeader blk, IsLedger l, HeaderHash l ~ HeaderHash blk
     , StandardHash l, HasTickedLedgerTables l
     )
  => Point blk
  -> LedgerDB l
  -> Maybe (l EmptyMK)
getPastLedgerAt pt db = current <$> rollback pt db

-- | Get a prefix of the LedgerDB that ends at the given point
--
--  \( O(\log(\min(i,n-i)) \)
--
-- When no ledger state (or anchor) has the given 'Point', 'Nothing' is
-- returned.
rollback ::
     ( HasHeader blk, IsLedger l, HeaderHash l ~ HeaderHash blk
     , StandardHash l, HasTickedLedgerTables l
     )
  => Point blk
  -> LedgerDB l
  -> Maybe (LedgerDB l)
rollback pt db
    | pt == castPoint (getTip (anchor db))
    = Just . LedgerDB . rollbackToAnchor $ ledgerDbChangelog db
    | otherwise
    = LedgerDB <$> rollbackToPoint (castPoint pt) (ledgerDbChangelog db)
