module Ouroboros.Consensus.Storage.LedgerDB (
    -- * LedgerDB
    Checkpoint (..)
  , LedgerDB (..)
  , LedgerDB'
  , LedgerDbCfg (..)
  , configLedgerDb
    -- * Initialization
  , InitLog (..)
  , ReplayStart (..)
  , initLedgerDB
    -- * Trace
  , ReplayGoal (..)
  , TraceReplayEvent (..)
  , decorateReplayTracerWithGoal
  , decorateReplayTracerWithStart
    -- * Querying
  , ledgerDbAnchor
  , ledgerDbCurrent
  , ledgerDbIsSaturated
  , ledgerDbMaxRollback
  , ledgerDbPast
  , ledgerDbSnapshots
  , ledgerDbTip
    -- * Updates
    -- ** Construct
  , ledgerDbWithAnchor
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
    -- ** Operations
  , defaultResolveWithErrors
  , ledgerDbBimap
  , ledgerDbPrune
  , ledgerDbPush
  , ledgerDbSwitch
    -- ** Pure API
  , ledgerDbPush'
  , ledgerDbPushMany'
  , ledgerDbSwitch'
    -- ** Trace
  , PushGoal (..)
  , PushStart (..)
  , Pushing (..)
  , UpdateLedgerDbTraceEvent (..)
    -- * Streaming
  , NextBlock (..)
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
    -- ** Low-level API (primarily exposed for testing)
  , decodeSnapshotBackwardsCompatible
  , deleteSnapshot
  , encodeSnapshot
  , snapshotToFileName
  , snapshotToPath
    -- ** Trace
  , TraceSnapshotEvent (..)
    -- * Disk policy
  , DiskPolicy (..)
  , SnapshotInterval (..)
  , TimeSinceLast (..)
  , defaultDiskPolicy
  ) where

import           Ouroboros.Consensus.Storage.LedgerDB.DiskPolicy
                     (DiskPolicy (..), SnapshotInterval (..),
                     TimeSinceLast (..), defaultDiskPolicy)
import           Ouroboros.Consensus.Storage.LedgerDB.Init (InitLog (..),
                     ReplayGoal (..), ReplayStart (..), TraceReplayEvent (..),
                     decorateReplayTracerWithGoal,
                     decorateReplayTracerWithStart, initLedgerDB)
import           Ouroboros.Consensus.Storage.LedgerDB.LedgerDB (Checkpoint (..),
                     LedgerDB (..), LedgerDB', LedgerDbCfg (..), configLedgerDb)
import           Ouroboros.Consensus.Storage.LedgerDB.Query (ledgerDbAnchor,
                     ledgerDbCurrent, ledgerDbIsSaturated, ledgerDbMaxRollback,
                     ledgerDbPast, ledgerDbSnapshots, ledgerDbTip)
import           Ouroboros.Consensus.Storage.LedgerDB.Snapshots
                     (DiskSnapshot (..), SnapshotFailure (..),
                     TraceSnapshotEvent (..), decodeSnapshotBackwardsCompatible,
                     deleteSnapshot, diskSnapshotIsTemporary, encodeSnapshot,
                     listSnapshots, readSnapshot, snapshotToFileName,
                     snapshotToPath, takeSnapshot, trimSnapshots, writeSnapshot)
import           Ouroboros.Consensus.Storage.LedgerDB.Stream (NextBlock (..),
                     StreamAPI (..), streamAll)
import           Ouroboros.Consensus.Storage.LedgerDB.Update
                     (AnnLedgerError (..), AnnLedgerError', Ap (..),
                     ExceededRollback (..), PushGoal (..), PushStart (..),
                     Pushing (..), ResolveBlock, ResolvesBlocks (..),
                     ThrowsLedgerError (..), UpdateLedgerDbTraceEvent (..),
                     defaultResolveWithErrors, defaultThrowLedgerErrors,
                     ledgerDbBimap, ledgerDbPrune, ledgerDbPush, ledgerDbPush',
                     ledgerDbPushMany', ledgerDbSwitch, ledgerDbSwitch',
                     ledgerDbWithAnchor)
