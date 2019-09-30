module Ouroboros.Storage.LedgerDB.DiskPolicy (
    DiskPolicy(..)
  , defaultDiskPolicy
  ) where

import           Data.Time.Clock (DiffTime)

import           Ouroboros.Consensus.Protocol.Abstract (SecurityParam (..))
import           Ouroboros.Consensus.Util.IOLike

-- | On-disk policy
--
-- We only write ledger states that are older than @k@ blocks to disk (that is,
-- snapshots that are guaranteed valid). The on-disk policy determines how often
-- we write to disk and how many checkpoints we keep.
data DiskPolicy m = DiskPolicy {
      -- | How many snapshots do we want to keep on disk?
      --
      -- A higher number of on-disk snapshots is primarily a safe-guard against
      -- disk corruption: it trades disk space for reliability.
      --
      -- Examples:
      --
      -- * @0@: Delete the snapshot immediately after writing.
      --        Probably not a useful value :-D
      -- * @1@: Delete the previous snapshot immediately after writing the next
      --        Dangerous policy: if for some reason the deletion happens before
      --        the new snapshot is written entirely to disk (we don't @fsync@),
      --        we have no choice but to start at the genesis snapshot on the
      --        next startup.
      -- * @2@: Always keep 2 snapshots around. This means that when we write
      --        the next snapshot, we delete the oldest one, leaving the middle
      --        one available in case of truncation of the write. This is
      --        probably a sane value in most circumstances.
      onDiskNumSnapshots  :: Word

      -- | How frequently do we want to write to disk?
      --
      -- Specified as an STM transaction that gives the delay (in microsec)
      -- between writes, allowing the delay to be varied on the fly if needed.
      --
      -- Writing snapshots more often means we have less work to do when
      -- opening the database, but at the cost of doing more IO while the node
      -- is running.
      --
      -- A sensible value might be the time interval corresponding to @k@
      -- blocks (12 hours), resulting in a gap between the most recent on disk
      -- snapshot and the oldest in-memory snapshot between @k@ and @2k@ blocks.
      --
      -- NOTE: Specifying this as a time interval rather than in terms of number
      -- of blocks means that during chain synchronization (where a node is
      -- catching up with its neighbours) the frequency of writes /in terms of
      -- blocks/ automatically goes down.
    , onDiskWriteInterval :: STM m DiffTime
    }

-- | Default on-disk policy
defaultDiskPolicy :: MonadSTM m
                  => SecurityParam     -- ^ Maximum rollback
                  -> DiffTime          -- ^ Slot length
                  -> DiskPolicy m
defaultDiskPolicy (SecurityParam k) slotLength = DiskPolicy {
      onDiskNumSnapshots  = 2
    , onDiskWriteInterval = return (fromIntegral k * slotLength)
    }
