module Ouroboros.Consensus.Storage.LedgerDB (
    -- * LedgerDB
    LedgerDB
  , LedgerDB'
  , LedgerDbCfg (..)
  , configLedgerDb
    -- * Initialization
  , BackingStoreSelector (..)
  , InitLog (..)
  , ReplayStart (..)
  , initialize
  , newBackingStore
  , newBackingStoreInitialiser
  , restoreBackingStore
    -- * Trace
  , ReplayGoal (..)
  , TraceLedgerDBEvent (..)
  , TraceReplayEvent (..)
  , decorateReplayTracerWithGoal
  , decorateReplayTracerWithStart
    -- * Querying
  , anchor
  , current
  , getPastLedgerAt
  , isSaturated
  , lastFlushedState
  , maxRollback
  , rollback
  , snapshots
  , tip
    -- * Updates
    -- ** Construct
  , mkWithAnchor
    -- ** Applying blocks
  , AnnLedgerError (..)
  , AnnLedgerError'
  , Ap (..)
  , ExceededRollback (..)
  , ThrowsLedgerError (..)
  , defaultThrowLedgerErrors
    -- ** Block resolution
  , ResolveBlock
  , ResolvesBlocks (..)
  , defaultResolveBlocks
    -- ** Operations
  , defaultResolveWithErrors
  , flush
  , prune
  , push
  , switch
  , volatileStatesBimap
    -- ** Pure API
  , push'
  , pushMany'
  , switch'
    -- ** Trace
  , PushGoal (..)
  , PushStart (..)
  , Pushing (..)
  , UpdateLedgerDbTraceEvent (..)
    -- * Streaming
  , NextItem (..)
  , StreamAPI (..)
  , streamAll
    -- * Snapshots
  , DiskSnapshot (..)
    -- ** Read from disk
  , SnapshotFailure (..)
  , diskSnapshotIsTemporary
  , listSnapshots
  , readSnapshot
    -- ** Write to disk
  , takeSnapshot
  , trimSnapshots
  , writeSnapshot
    -- ** Delete
  , deleteSnapshot
    -- ** Paths
  , snapshotToStatePath
  , snapshotToTablesPath
    -- ** Trace
  , TraceSnapshotEvent (..)
    -- * Disk policy
  , DiskPolicy (..)
  , SnapshotInterval (..)
  , TimeSinceLast (..)
  , defaultDiskPolicy
    -- * Test
  , decodeSnapshotBackwardsCompatible
  , encodeSnapshot
  ) where

import           Ouroboros.Consensus.Storage.LedgerDB.DiskPolicy
                     (DiskPolicy (..), SnapshotInterval (..),
                     TimeSinceLast (..), defaultDiskPolicy)
import           Ouroboros.Consensus.Storage.LedgerDB.Init
                     (BackingStoreSelector (..), InitLog (..), ReplayGoal (..),
                     ReplayStart (..), TraceLedgerDBEvent (..),
                     TraceReplayEvent (..), decorateReplayTracerWithGoal,
                     decorateReplayTracerWithStart, initialize, newBackingStore,
                     newBackingStoreInitialiser, restoreBackingStore)
import           Ouroboros.Consensus.Storage.LedgerDB.LedgerDB (LedgerDB,
                     LedgerDB', LedgerDbCfg (..), configLedgerDb, mkWithAnchor)
import           Ouroboros.Consensus.Storage.LedgerDB.Query (anchor, current,
                     getPastLedgerAt, isSaturated, lastFlushedState,
                     maxRollback, rollback, snapshots, tip)
import           Ouroboros.Consensus.Storage.LedgerDB.Snapshots
                     (DiskSnapshot (..), SnapshotFailure (..),
                     TraceSnapshotEvent (..), decodeSnapshotBackwardsCompatible,
                     deleteSnapshot, diskSnapshotIsTemporary, encodeSnapshot,
                     listSnapshots, readSnapshot, snapshotToStatePath,
                     snapshotToTablesPath, takeSnapshot, trimSnapshots,
                     writeSnapshot)
import           Ouroboros.Consensus.Storage.LedgerDB.Stream (NextItem (..),
                     StreamAPI (..), streamAll)
import           Ouroboros.Consensus.Storage.LedgerDB.Update
                     (AnnLedgerError (..), AnnLedgerError', Ap (..),
                     ExceededRollback (..), PushGoal (..), PushStart (..),
                     Pushing (..), ResolveBlock, ResolvesBlocks (..),
                     ThrowsLedgerError (..), UpdateLedgerDbTraceEvent (..),
                     defaultResolveBlocks, defaultResolveWithErrors,
                     defaultThrowLedgerErrors, flush, prune, push, push',
                     pushMany', switch, switch', volatileStatesBimap)
