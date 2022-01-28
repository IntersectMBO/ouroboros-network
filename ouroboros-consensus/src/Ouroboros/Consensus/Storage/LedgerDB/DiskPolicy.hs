{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards    #-}

{-| The logic for how snapshots are created is implicitly spread across different
  definitions. Here we will describe the general intention on the logic.

  Old-style Snapshots
  ===================

  The old-style database is an anchored sequence of @ExtLedgerState blk
  ValuesMK@ which contain a @LedgerState@ which has been deprived from its UTxO
  table, and said table is stored in the @ValuesMK@ part. These states contain a
  full UTxO and therefore they are complete pictures of the state of the UTxO
  after the application of a specific block.

  When we push a ledger state to the old database, we /prune/ it by keeping only
  the last @k@ elements in the anchored sequence. This means that said list will
  always have length @<= k + 1@ (plus the anchor). This doesn't imply that the
  anchor is written to the disk.

  Until the anchor gets serialized into the disk, if the node is shutdown, it
  will resume operation from an older point. To explain this we will use an
  example:

  >                                             |<---  k blocks --->|
  > Selected Chain:  Genesis |> ... > A > B > C  > D > E > F > ... > T
  > ImmutableDB:     Genesis |> ... > A > B > C
  > VolatileDB:                               C |> D > E > F > ... > T  (# blocks [D,T] <= k)
  > Latest Snapshot:                      *
  > LedgerDB:                                 C |> D > E > F > ... > T

  If the latest snapshot that was taken corresponds to B, if the node is
  shutdown in this instant, when it restarts, it will load the following state
  from disk:

  >                                              |<---  k blocks --->|
  > Selected Chain: Genesis |> ... > A > B >  C  > D > E > F > ... > T
  > ImmutableDB:    Genesis |> ... > A > B >  C
  > VolatileDB:                               C |> D > E > F > ... > T  (# blocks [D,T] <= k)
  > LedgerDB:                            B |>

  Proceeding then to replay the blocks on the ImmutableDB and then the ones on
  the VolatileDB (the latter will be validated when applied), reaching in the
  end the same state as above.

  Because each LedgerState is fully contained, there's no problem on the above
  logic, even if we have some spans of time in which a shutdown will make the
  node start from an older point.

  It is responsibility of the
  @Ouroboros.Conensus.Storage.ChainDB.Impl.Background.copyAndSnapshotRunner@
  copying the blocks to the disk and when dictated by the @DiskPolicy@, take a
  snapshot. Said snapshot will /always/ be taken from the current anchor of the
  anchored sequence, as said block is put in the ImmutableDB by the runner
  before taking the snapshot.

  The @DiskPolicy@ will specify how many snapshots to keep around, as it is
  possible to start from an older snapshot (due to them being self contained),
  and also how much time we want at least between snapshots, in order to avoid
  flushing too quickly.

  New-style Snapshot and flushing
  ===============================

  In the new-style database we hace mainly 3 moving pieces: the sequence of
  @ExtLedgerState blk EmptyMK@ states, the sequence of @TableDiff@s and the disk
  backend.

  Ledger states no longer contain a full UTxO and therefore they are dependent
  on the history before them to make a consistent view of the UTxO after
  applying a block. The list of can no longer be pruned to @k@ as we cannot lose
  the sequence of differences between what was last flushed and the @length -
  k@th block.

  So adapting the example above, we can have the following situation:

  >                                                |<---  k blocks --->|
  > Selected Chain:   Genesis |> ... > A >  B > C  > D > E > F > ... > T
  > ImmutableDB:      Genesis |> ... > A >  B > C
  > VolatileDB:                                 C |> D > E > F > ... > T  (# blocks [D,T] <= k)
  > DiskBackend:                       *
  > LedgerStates:                      A |> B > C >  D > E > F > ... > T
  > LedgerDiffs:                            B > C >  D > E > F > ... > T

  Note that the LedgerDiffs is a list and not an anchored sequence, and
  therefore can be empty.

  * Flushing :: applying a sequence of differences to the disk backend to make
    them permanent. We will abuse the name and say that we "flush a block" to
    mean that we flush the sequence of differences in the DbChangelog up to said
    block (included).

  * Snapshotting :: writing a serialization of a @ExtLedgerState blk EmptyMK@
    from the list of ledger states.

  Snapshotting and flushing shouldn't necessarily go together if the backend
  provides a restore-point functionality. We could flush frequently and only
  from time to time flush + create a restore point + snapshot. However, as we
  don't currently have said functionality, we will go with the simplest
  implementation which is just flushing + snapshotting from time to time. This
  imposes some more restrictions, namely:

  - There can only be one snapshot on the disk (or old ones can be just
    ignored, but then why even keep them around)

  - Said snapshot must be in sync with the contents on the disk backend.

  This is the only way we could, from the example above, construct a consistent
  state on startup as follows:

  >                                                |<---  k blocks --->|
  > Selected Chain:   Genesis |> ... > A >  B >  C  > D > E > F > ... > T
  > ImmutableDB:      Genesis |> ... > A >  B >  C
  > VolatileDB:                                  C |> D > E > F > ... > T  (# blocks [D,T] <= k)
  > DiskBackend:                       *
  > LedgerStates:                      A |> []
  > LedgerDiffs:                            []

  It is important to note also that only blocks that are more than @k@ blocks
  from the tip can be flushed + snapshotted as they are the ones belonging to
  the immutable database and therefore will remain unaltered forever. As the
  functionality is coupled with the one from the old-style LedgerDB, we can be
  sure that only blocks that have been copied to the ImmutableDB will be
  considered for flushing + snapshotting.

  The @DiskPolicy@ will specify how much time we want at least between
  performing flush+snapshot.
-}
module Ouroboros.Consensus.Storage.LedgerDB.DiskPolicy (
    DiskPolicy (..)
  , SnapshotInterval (..)
  , TimeSinceLast (..)
  , defaultDiskPolicy
  ) where

import           Data.Time.Clock (secondsToDiffTime)
import           Data.Word
import           GHC.Generics
import           NoThunks.Class (NoThunks, OnlyCheckWhnf (..))

import           Control.Monad.Class.MonadTime

import           Ouroboros.Consensus.Config.SecurityParam

-- | Length of time, requested by the user, that has to pass after which
-- a snapshot is taken. It can be:
--
-- 1. either explicitly provided by user in seconds
-- 2. or default value can be requested - the specific DiskPolicy determines
--    what that is exactly, see `defaultDiskPolicy` as an example
data SnapshotInterval =
    DefaultSnapshotInterval
  | RequestedSnapshotInterval DiffTime
  deriving stock (Eq, Generic, Show)

-- | On-disk policy
--
-- We only write ledger states that are older than @k@ blocks to disk (that is,
-- snapshots that are guaranteed valid). The on-disk policy determines how often
-- we write to disk and how many checkpoints we keep.
data DiskPolicy = DiskPolicy {
      -- | How many old-style snapshots do we want to keep on disk?
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
      onDiskNumOldSnapshots       :: Word

      -- | Should we write an old-style snapshot of the ledger state to disk?
      --
      -- This function is passed two bits of information:
      --
      -- * The time since the last snapshot, or 'NoSnapshotTakenYet' if none was taken yet.
      --   Note that 'NoSnapshotTakenYet' merely means no snapshot had been taking yet
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
    , onDiskShouldTakeOldSnapshot :: TimeSinceLast DiffTime -> Word64 -> Bool

      -- | Should we write a new-style snapshot of the ledger state to disk?
      --
      -- This function is passed two bits of information:
      --
      -- * The time since the last snapshot, or 'NoSnapshotTakenYet' if none was
      --   taken yet. Note that 'NoSnapshotTakenYet' merely means no snapshot
      --   had been taking yet since the node was started; it does not
      --   necessarily mean that none exist on disk.
      --
      -- * The distance in terms of blocks applied to the /oldest/ ledger
      --   snapshot in memory. During normal operation, this is the number of
      --   blocks written to the ImmutableDB since the last snapshot. On
      --   startup, it is computed by counting how many immutable blocks we had
      --   to reapply to get to the chain tip. This is useful, as it allows the
      --   policy to decide to take a snapshot /on node startup/ if a lot of
      --   blocks had to be replayed.
      --
      -- Note that in the new-style LedgerDB, there will only be one snapshot
      -- taken as it has to be in sync with the UTxO backend, so there is no
      -- concept of "how many snapshots should we keep on disk". TODO: This will
      -- change dramatically when we introduce restore-points or equivalent.
      --
      -- See also 'defaultDiskPolicy'
    , onDiskShouldTakeNewSnapshot :: TimeSinceLast DiffTime -> Word64 -> Bool
    }
  deriving NoThunks via OnlyCheckWhnf DiskPolicy

data TimeSinceLast time = NoSnapshotTakenYet | TimeSinceLast time
  deriving (Functor, Show)

-- | Default on-disk policy suitable to use with cardano-node
--
defaultDiskPolicy :: SecurityParam -> SnapshotInterval -> DiskPolicy
defaultDiskPolicy (SecurityParam k) requestedInterval = DiskPolicy {..}
  where
    onDiskNumOldSnapshots :: Word
    onDiskNumOldSnapshots = 2

    onDiskShouldTakeOldSnapshot ::
         TimeSinceLast DiffTime
      -> Word64
      -> Bool
    onDiskShouldTakeOldSnapshot NoSnapshotTakenYet blocksSinceLast =
      -- If users never leave their wallet running for long, this would mean
      -- that under some circumstances we would never take a snapshot
      -- So, on startup (when the 'time since the last snapshot' is `Nothing`),
      -- we take a snapshot as soon as there are @k@ blocks replayed.
      -- This means that even if users frequently shut down their wallet, we still
      -- take a snapshot roughly every @k@ blocks. It does mean the possibility of
      -- an extra unnecessary snapshot during syncing (if the node is restarted), but
      -- that is not a big deal.
      blocksSinceLast >= k

    onDiskShouldTakeOldSnapshot (TimeSinceLast timeSinceLast) blocksSinceLast =
         timeSinceLast >= snapshotInterval
      || substantialAmountOfBlocksWereProcessed blocksSinceLast timeSinceLast

    -- | We want to create a snapshot after a substantial amount of blocks were
    -- processed (hard-coded to 50k blocks). Given the fact that during bootstrap
    -- a fresh node will see a lot of blocks over a short period of time, we want
    -- to limit this condition to happen not more often then a fixed amount of
    -- time (here hard-coded to 6 minutes)
    substantialAmountOfBlocksWereProcessed blocksSinceLast timeSinceLast =
      let minBlocksBeforeSnapshot      = 50_000
          minTimeBeforeSnapshot        = 6 * secondsToDiffTime 60
      in    blocksSinceLast >= minBlocksBeforeSnapshot
         && timeSinceLast   >= minTimeBeforeSnapshot

    -- | Requested snapshot interval can be explicitly provided by the
    -- caller (RequestedSnapshotInterval) or the caller might request the default
    -- snapshot interval (DefaultSnapshotInterval). If the latter then the
    -- snapshot interval is defaulted to k * 2 seconds - when @k = 2160@ the interval
    -- defaults to 72 minutes.
    snapshotInterval = case requestedInterval of
      RequestedSnapshotInterval value -> value
      DefaultSnapshotInterval         -> secondsToDiffTime $ fromIntegral $ k * 2

    onDiskShouldTakeNewSnapshot timeSinceLast blocksSinceLast =
      let minTimeSinceLast = 3 * secondsToDiffTime 60
          minBlocksBeforeSnapshot = 2_160 * 2
      in minBlocksBeforeSnapshot < blocksSinceLast ||
         case timeSinceLast of
           NoSnapshotTakenYet -> True
           TimeSinceLast t    -> minTimeSinceLast < t
