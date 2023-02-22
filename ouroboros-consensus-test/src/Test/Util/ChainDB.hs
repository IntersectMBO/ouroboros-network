{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module Test.Util.ChainDB (
    MinimalChainDbArgs (..)
  , NodeDBs (..)
  , emptyNodeDBs
  , fromMinimalChainDbArgs
  , mkTestChunkInfo
  ) where


import           Control.Tracer (nullTracer)
import           Data.Functor.Identity (Identity)
import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Config
                     (TopLevelConfig (topLevelConfigLedger),
                     configSecurityParam)
import           Ouroboros.Consensus.Fragment.InFuture (CheckInFuture (..))
import qualified Ouroboros.Consensus.Fragment.Validated as VF
import           Ouroboros.Consensus.HardFork.History.EraParams (EraParams,
                     eraEpochSize)
import           Ouroboros.Consensus.Ledger.Basics (LedgerConfig, ValuesMK)
import           Ouroboros.Consensus.Ledger.Extended (ExtLedgerState)
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Storage.ChainDB hiding
                     (TraceFollowerEvent (..))
import qualified Ouroboros.Consensus.Storage.ImmutableDB as ImmutableDB
import           Ouroboros.Consensus.Storage.ImmutableDB.Chunks.Internal
                     (simpleChunkInfo)
import qualified Ouroboros.Consensus.Storage.LedgerDB as LedgerDB
import qualified Ouroboros.Consensus.Storage.VolatileDB as VolatileDB
import           Ouroboros.Consensus.Util.IOLike hiding (invariant)
import           Ouroboros.Consensus.Util.ResourceRegistry (ResourceRegistry)
import           System.FS.API (SomeHasFS (..))
import qualified System.FS.Sim.MockFS as Mock
import           System.FS.Sim.MockFS
import           System.FS.Sim.STM (simHasFS)

-- | A vector with an element for each database of a node
--
-- The @db@ type parameter is instantiated by this module at types for mock
-- filesystems; either the 'MockFS' type or reference cells thereof.
data NodeDBs db = NodeDBs {
    nodeDBsImm :: db
  , nodeDBsVol :: db
  , nodeDBsLgr :: db
  }
  deriving (Functor, Foldable, Traversable)

emptyNodeDBs :: MonadSTM m => m (NodeDBs (StrictTVar m MockFS))
emptyNodeDBs = NodeDBs
  <$> uncheckedNewTVarM Mock.empty
  <*> uncheckedNewTVarM Mock.empty
  <*> uncheckedNewTVarM Mock.empty

-- | Minimal set of arguments for creating a ChainDB instance for testing purposes.
data MinimalChainDbArgs m blk = MinimalChainDbArgs {
    mcdbTopLevelConfig       :: TopLevelConfig blk
  , mcdbChunkInfo            :: ImmutableDB.ChunkInfo
  -- ^ Specifies the layout of the ImmutableDB on disk.
  , mcdbInitLedger           :: ExtLedgerState blk ValuesMK
  -- ^ The initial ledger state.
  , mcdbRegistry             :: ResourceRegistry m
  -- ^ Keeps track of non-lexically scoped resources.
  , mcdbNodeDBs              :: NodeDBs (StrictTVar m MockFS)
  -- ^ File systems underlying the immutable, volatile and ledger databases.
  -- Would be useful to default this to StrictTVar's containing empty MockFS's.
  , mcdbBackingStoreSelector :: LedgerDB.BackingStoreSelector m
  }

-- | Utility function to get a default chunk info in case we have EraParams available.
mkTestChunkInfo :: LedgerConfig blk ~ EraParams => TopLevelConfig blk -> ImmutableDB.ChunkInfo
mkTestChunkInfo = simpleChunkInfo . eraEpochSize . topLevelConfigLedger

-- | Creates a default set of of arguments for ChainDB tests.
fromMinimalChainDbArgs ::
     ( MonadThrow m
     , MonadSTM m
     , ConsensusProtocol (BlockProtocol blk)
     )
  => MinimalChainDbArgs m blk -> ChainDbArgs Identity m blk
fromMinimalChainDbArgs MinimalChainDbArgs {..} = ChainDbArgs {
    cdbHasFSImmutableDB       = SomeHasFS $ simHasFS (nodeDBsImm mcdbNodeDBs)
  , cdbHasFSVolatileDB        = SomeHasFS $ simHasFS (nodeDBsVol mcdbNodeDBs)
  , cdbHasFSLgrDB             = SomeHasFS $ simHasFS (nodeDBsLgr mcdbNodeDBs)
  , cdbImmutableDbValidation  = ImmutableDB.ValidateAllChunks
  , cdbVolatileDbValidation   = VolatileDB.ValidateAll
  , cdbMaxBlocksPerFile       = VolatileDB.mkBlocksPerFile 4
  , cdbDiskPolicy             = LedgerDB.defaultDiskPolicy (configSecurityParam mcdbTopLevelConfig)
                                  LedgerDB.DefaultSnapshotInterval
  -- Keep 2 ledger snapshots, and take a new snapshot at least every 2 * k seconds, where k is the
  -- security parameter.
  , cdbTopLevelConfig         = mcdbTopLevelConfig
  , cdbChunkInfo              = mcdbChunkInfo
  , cdbCheckIntegrity         = const True
  -- Getting a verified block component does not do any integrity checking, both for the
  -- ImmutableDB, as the VolatileDB. This is done in @extractBlockComponent@ in the iterator for the
  -- ImmutableDB, and in @getBlockComponent@ for the VolatileDB.
  , cdbGenesis                = return mcdbInitLedger
  , cdbCheckInFuture          = CheckInFuture $ \vf -> pure (VF.validatedFragment vf, [])
  -- Blocks are never in the future.
  , cdbImmutableDbCacheConfig = ImmutableDB.CacheConfig 2 60
  -- Cache at most 2 chunks and expire each chunk after 60 seconds of being unused.
  , cdbTracer                 = nullTracer
  , cdbTraceLedger            = nullTracer
  , cdbRegistry               = mcdbRegistry
  , cdbGcDelay                = 1
  , cdbGcInterval             = 1
  , cdbBlocksToAddSize        = 1
  , cdbBackingStoreSelector   = mcdbBackingStoreSelector
  }
