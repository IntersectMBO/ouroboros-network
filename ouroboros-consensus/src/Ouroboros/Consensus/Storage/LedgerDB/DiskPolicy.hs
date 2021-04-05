{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards    #-}

module Ouroboros.Consensus.Storage.LedgerDB.DiskPolicy (
    DiskPolicy (..)
  , defaultDiskPolicy
  ) where

import           Data.Time.Clock (secondsToDiffTime)
import           Data.Word
import           NoThunks.Class (NoThunks, OnlyCheckWhnf (..))

import           Control.Monad.Class.MonadTime

import           Ouroboros.Consensus.Config.SecurityParam

-- | On-disk policy
--
-- We only write ledger states that are older than @k@ blocks to disk (that is,
-- snapshots that are guaranteed valid). The on-disk policy determines how often
-- we write to disk and how many checkpoints we keep.
data DiskPolicy = DiskPolicy {
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
      onDiskNumSnapshots       :: Word

      -- | Should we write a snapshot of the ledger state to disk?
      --
      -- This function is passed two bits of information:
      --
      -- * The time since the last snapshot, or 'Nothing' if none was taken yet.
      --   Note that 'Nothing' merely means no snapshot had been taking yet
      --   since the node was started; it does not necessarily mean that none
      --   exist on disk.
      --
      -- * The distance in terms of blocks applied to the /oldest/ ledger
      --   snapshot in memory. During normal operation, this is the number of
      --   blocks written to the ImmutableDB since the last snapshot. On
      --   startup, it is computed by counting how many immutable blocks we had
      --   to reapply to get to the chain tip. This is useful, as it allows the
      --   policy to decide to take a snapshot /on node startup/ if a lot of
      --   blocks had to be replayed.
      --
      -- See also 'defaultDiskPolicy'
    , onDiskShouldTakeSnapshot :: Maybe DiffTime -> Word64 -> Bool
    }
  deriving NoThunks via OnlyCheckWhnf DiskPolicy

-- | Default on-disk policy
--
-- We want to take a snapshot every 50k blocks or roughly every hour (72
-- minutes actually) when @k = 2160@ (for other values of @k@ we scale
-- proportionally), but not more rapidly than 10 per hour (which is important
-- for bulk sync).
--
-- If users never leave their wallet running for long, however, this would mean
-- that we /never/ take snapshots after syncing (until we get to 50k blocks).
-- So, on startup, we take a snapshot as soon as there are @k@ blocks replayed.
-- This means that even if users frequently shut down their wallet, we still
-- take a snapshot roughly every @k@ blocks. It does mean the possibility of
-- an extra unnecessary snapshot during syncing (if the node is restarted), but
-- that is not a big deal.
defaultDiskPolicy :: SecurityParam -> DiskPolicy
defaultDiskPolicy (SecurityParam k) = DiskPolicy {..}
  where
    onDiskNumSnapshots :: Word
    onDiskNumSnapshots = 2

    onDiskShouldTakeSnapshot :: Maybe DiffTime -> Word64 -> Bool
    onDiskShouldTakeSnapshot (Just timeSinceLast) blocksSinceLast =
           timeSinceLast >= secondsToDiffTime (fromIntegral (k * 2))
        || (   blocksSinceLast >= 50_000
            && timeSinceLast > 6 * secondsToDiffTime 60)
    onDiskShouldTakeSnapshot Nothing blocksSinceLast =
           blocksSinceLast >= k
