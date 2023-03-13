{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
module Ouroboros.Consensus.Storage.LedgerDB.OnDisk {-# DEPRECATED "Use Ouroboros.Consensus.Storage.LedgerDB instead" #-} (
    -- * Opening the database
    InitFailure
  , LDB.SnapshotFailure (..)
  , LDB.InitLog (..)
  , initLedgerDB
    -- ** Instantiate in-memory to @blk@
  , LDB.AnnLedgerError'
  , LDB.LedgerDB'
    -- ** Abstraction over the stream API
  , NextBlock
  , LDB.StreamAPI (..)
    -- * Read from disk
  , readSnapshot
    -- * Write to disk
  , takeSnapshot
  , trimSnapshots
  , writeSnapshot
    -- * Low-level API (primarily exposed for testing)
  , deleteSnapshot
  , snapshotToFileName
  , snapshotToPath
    -- ** opaque
  , LDB.DiskSnapshot (..)
    -- * Trace events
  , LDB.ReplayGoal (..)
  , LDB.ReplayStart (..)
  , TraceEvent
  , LDB.TraceSnapshotEvent (..)
  , LDB.TraceReplayEvent (..)
  , decorateReplayTracerWithGoal
  ) where

import           Codec.Serialise.Decoding (Decoder)
import           Codec.Serialise.Encoding (Encoding)
import           Control.Monad.Except
import           Control.Tracer
import           Data.Word
import           GHC.Stack

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Basics
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Inspect
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Util.CBOR (ReadIncrementalErr)
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry

import           Ouroboros.Consensus.Storage.FS.API
import           Ouroboros.Consensus.Storage.FS.API.Types

import           Ouroboros.Consensus.Storage.LedgerDB.BackingStore
import           Ouroboros.Consensus.Storage.LedgerDB.DiskPolicy
import qualified Ouroboros.Consensus.Storage.LedgerDB as LDB

{-------------------------------------------------------------------------------
  Non-exported local aliases
-------------------------------------------------------------------------------}
type LedgerDB'        blk = LDB.LedgerDB' blk
type StreamAPI m      blk = LDB.StreamAPI m blk
type InitLog          blk = LDB.InitLog blk
type DiskSnapshot         = LDB.DiskSnapshot
type ReplayGoal       blk = LDB.ReplayGoal blk
type TraceReplayEvent blk = LDB.TraceReplayEvent blk

{-------------------------------------------------------------------------------
  Renamed types
-------------------------------------------------------------------------------}

{-# DEPRECATED InitFailure "Use Ouroboros.Consensus.Storage.LedgerDB (SnapshotFailure)" #-}
type InitFailure blk = LDB.SnapshotFailure blk

{-# DEPRECATED TraceEvent "Use Ouroboros.Consensus.Storage.LedgerDB (TraceSnapshotEvent)" #-}
type TraceEvent blk = LDB.TraceSnapshotEvent blk

{-# DEPRECATED NextBlock "Use Ouroboros.Consensus.Storage.LedgerDB (NextItem)" #-}
type NextBlock blk = LDB.NextItem blk

{-------------------------------------------------------------------------------
  Deprecated functions
-------------------------------------------------------------------------------}

{-# DEPRECATED initLedgerDB "Use Ouroboros.Consensus.Storage.LedgerDB (initLedgerDB)" #-}
initLedgerDB ::
     forall m blk. (
         IOLike m
       , LedgerSupportsProtocol blk
       , CanSerializeLedgerTables (LedgerState blk)
       , InspectLedger blk
       , HasCallStack
       )
  => Tracer m (ReplayGoal blk -> TraceReplayEvent blk)
  -> Tracer m (TraceEvent blk)
  -> SomeHasFS m
  -> ResourceRegistry m
  -> (forall s. Decoder s (ExtLedgerState blk EmptyMK))
  -> (forall s. Decoder s (HeaderHash blk))
  -> LDB.LedgerDbCfg (ExtLedgerState blk)
  -> m (ExtLedgerState blk ValuesMK) -- ^ Genesis ledger state
  -> StreamAPI m blk blk
  -> m (InitLog blk, LedgerDB' blk, Word64, LedgerBackingStore' m blk)
initLedgerDB = LDB.initialize

{-# DEPRECATED takeSnapshot "Use Ouroboros.Consensus.Storage.LedgerDB (takeSnapshot)" #-}
takeSnapshot ::
     (MonadThrow m, IsLedger (LedgerState blk))
  => Tracer m (TraceEvent blk)
  -> SomeHasFS m
  -> LedgerBackingStore' m blk
  -> (ExtLedgerState blk EmptyMK -> Encoding)
  -> LedgerDB' blk
  -> m (Maybe (DiskSnapshot, RealPoint blk))
takeSnapshot t fs bs e = LDB.takeSnapshot t fs bs e . LDB.anchor

{-# DEPRECATED trimSnapshots "Use Ouroboros.Consensus.Storage.LedgerDB (trimSnapshots)" #-}
trimSnapshots ::
     Monad m
  => Tracer m (TraceEvent r)
  -> SomeHasFS m
  -> DiskPolicy
  -> m [DiskSnapshot]
trimSnapshots = LDB.trimSnapshots

{-# DEPRECATED readSnapshot "Use Ouroboros.Consensus.Storage.LedgerDB (readSnapshot)" #-}
readSnapshot ::
     IOLike m => SomeHasFS m
  -> (forall s. Decoder s (ExtLedgerState blk EmptyMK))
  -> (forall s. Decoder s (HeaderHash blk))
  -> DiskSnapshot
  -> ExceptT ReadIncrementalErr m (ExtLedgerState blk EmptyMK)
readSnapshot = LDB.readSnapshot

{-# DEPRECATED writeSnapshot "Use Ouroboros.Consensus.Storage.LedgerDB (writeSnapshot)" #-}
writeSnapshot ::
     MonadThrow m
  => SomeHasFS m
  -> LedgerBackingStore' m blk
  -> (ExtLedgerState blk EmptyMK -> Encoding)
  -> DiskSnapshot
  -> ExtLedgerState blk EmptyMK -> m ()
writeSnapshot = LDB.writeSnapshot

{-# DEPRECATED deleteSnapshot "Use Ouroboros.Consensus.Storage.LedgerDB (deleteSnapshot)" #-}
deleteSnapshot :: SomeHasFS m -> DiskSnapshot -> m ()
deleteSnapshot = LDB.deleteSnapshot

{-# DEPRECATED snapshotToFileName "Snapshots no longer have only one path. This function will error if called" #-}
snapshotToFileName :: DiskSnapshot -> String
snapshotToFileName = error "Deprecated"

{-# DEPRECATED snapshotToPath "Snapshots no longer have only one path. This function will error if called" #-}
snapshotToPath :: DiskSnapshot -> FsPath
snapshotToPath = error "Deprecated"

{-# DEPRECATED decorateReplayTracerWithGoal "Use Ouroboros.Consensus.Storage.LedgerDB (decorateReplayTracerWithGoal)" #-}
decorateReplayTracerWithGoal
  :: Point blk -- ^ Tip of the ImmutableDB
  -> Tracer m (TraceReplayEvent blk)
  -> Tracer m (ReplayGoal blk -> TraceReplayEvent blk)
decorateReplayTracerWithGoal = LDB.decorateReplayTracerWithGoal
