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
  , LDB.NextBlock (..)
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

import           Ouroboros.Consensus.Storage.FS.API
import           Ouroboros.Consensus.Storage.FS.API.Types

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

{-------------------------------------------------------------------------------
  Deprecated functions
-------------------------------------------------------------------------------}

{-# DEPRECATED initLedgerDB "Use Ouroboros.Consensus.Storage.LedgerDB (initLedgerDB)" #-}
initLedgerDB ::
     forall m blk. (
         IOLike m
       , LedgerSupportsProtocol blk
       , InspectLedger blk
       , HasCallStack
       )
  => Tracer m (ReplayGoal blk -> TraceReplayEvent blk)
  -> Tracer m (TraceEvent blk)
  -> SomeHasFS m
  -> (forall s. Decoder s (ExtLedgerState blk))
  -> (forall s. Decoder s (HeaderHash blk))
  -> LDB.LedgerDbCfg (ExtLedgerState blk)
  -> m (ExtLedgerState blk) -- ^ Genesis ledger state
  -> StreamAPI m blk
  -> m (InitLog blk, LedgerDB' blk, Word64)
initLedgerDB = LDB.initLedgerDB

{-# DEPRECATED takeSnapshot "Use Ouroboros.Consensus.Storage.LedgerDB (takeSnapshot)" #-}
takeSnapshot ::
     (MonadThrow m, IsLedger (LedgerState blk))
  => Tracer m (TraceEvent blk)
  -> SomeHasFS m
  -> (ExtLedgerState blk -> Encoding)
  -> LedgerDB' blk -> m (Maybe (DiskSnapshot, RealPoint blk))
takeSnapshot t fs e = LDB.takeSnapshot t fs e . LDB.ledgerDbAnchor

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
  -> (forall s. Decoder s (ExtLedgerState blk))
  -> (forall s. Decoder s (HeaderHash blk))
  -> DiskSnapshot
  -> ExceptT ReadIncrementalErr m (ExtLedgerState blk)
readSnapshot = LDB.readSnapshot

{-# DEPRECATED writeSnapshot "Use Ouroboros.Consensus.Storage.LedgerDB (writeSnapshot)" #-}
writeSnapshot ::
     MonadThrow m => SomeHasFS m
  -> (ExtLedgerState blk -> Encoding)
  -> DiskSnapshot
  -> ExtLedgerState blk -> m ()
writeSnapshot = LDB.writeSnapshot

{-# DEPRECATED deleteSnapshot "Use Ouroboros.Consensus.Storage.LedgerDB (deleteSnapshot)" #-}
deleteSnapshot :: SomeHasFS m -> DiskSnapshot -> m ()
deleteSnapshot = LDB.deleteSnapshot

{-# DEPRECATED snapshotToFileName "Use Ouroboros.Consensus.Storage.LedgerDB (snapshotToFileName)" #-}
snapshotToFileName :: DiskSnapshot -> String
snapshotToFileName = LDB.snapshotToFileName

{-# DEPRECATED snapshotToPath "Use Ouroboros.Consensus.Storage.LedgerDB (snapshotToPath)" #-}
snapshotToPath :: DiskSnapshot -> FsPath
snapshotToPath = LDB.snapshotToPath

{-# DEPRECATED decorateReplayTracerWithGoal "Use Ouroboros.Consensus.Storage.LedgerDB (decorateReplayTracerWithGoal)" #-}
decorateReplayTracerWithGoal
  :: Point blk -- ^ Tip of the ImmutableDB
  -> Tracer m (TraceReplayEvent blk)
  -> Tracer m (ReplayGoal blk -> TraceReplayEvent blk)
decorateReplayTracerWithGoal = LDB.decorateReplayTracerWithGoal
