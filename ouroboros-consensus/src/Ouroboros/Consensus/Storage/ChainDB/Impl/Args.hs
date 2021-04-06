{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}

module Ouroboros.Consensus.Storage.ChainDB.Impl.Args (
    ChainDbArgs (..)
  , ChainDbSpecificArgs (..)
  , RelativeMountPoint (..)
  , defaultArgs
    -- * Internal
  , fromChainDbArgs
  ) where

import           Data.Time.Clock (DiffTime, secondsToDiffTime)

import           Control.Tracer (Tracer, contramap, nullTracer)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Fragment.InFuture (CheckInFuture)
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Util.Args
import           Ouroboros.Consensus.Util.ResourceRegistry (ResourceRegistry)

import           Ouroboros.Consensus.Storage.FS.API

import           Ouroboros.Consensus.Storage.ChainDB.Impl.LgrDB (LedgerDB')
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.LgrDB as LgrDB
import           Ouroboros.Consensus.Storage.ChainDB.Impl.Types
                     (TraceEvent (..))
import           Ouroboros.Consensus.Storage.ImmutableDB (ChunkInfo)
import qualified Ouroboros.Consensus.Storage.ImmutableDB as ImmutableDB
import           Ouroboros.Consensus.Storage.LedgerDB.DiskPolicy
                     (DiskPolicy (..))
import qualified Ouroboros.Consensus.Storage.VolatileDB as VolatileDB

{-------------------------------------------------------------------------------
  Arguments
-------------------------------------------------------------------------------}

data ChainDbArgs f m blk = ChainDbArgs {

      -- HasFS instances
      cdbHasFSImmutableDB       :: SomeHasFS m
    , cdbHasFSVolatileDB        :: SomeHasFS m
    , cdbHasFSLgrDB             :: SomeHasFS m

      -- Policy
    , cdbImmutableDbValidation  :: ImmutableDB.ValidationPolicy
    , cdbVolatileDbValidation   :: VolatileDB.BlockValidationPolicy
    , cdbMaxBlocksPerFile       :: VolatileDB.BlocksPerFile
    , cdbDiskPolicy             :: LgrDB.DiskPolicy

      -- Integration
    , cdbTopLevelConfig         :: HKD f (TopLevelConfig blk)
    , cdbChunkInfo              :: HKD f ChunkInfo
    , cdbCheckIntegrity         :: HKD f (blk -> Bool)
    , cdbGenesis                :: HKD f (m (ExtLedgerState blk))
    , cdbCheckInFuture          :: HKD f (CheckInFuture m blk)
    , cdbImmutableDbCacheConfig :: ImmutableDB.CacheConfig

      -- Misc
    , cdbTracer                 :: Tracer m (TraceEvent blk)
    , cdbTraceLedger            :: Tracer m (LedgerDB' blk)
    , cdbRegistry               :: HKD f (ResourceRegistry m)
    , cdbGcDelay                :: DiffTime
    , cdbGcInterval             :: DiffTime
    , cdbBlocksToAddSize        :: Word
      -- ^ Size of the queue used to store asynchronously added blocks. This
      -- is the maximum number of blocks that could be kept in memory at the
      -- same time when the background thread processing the blocks can't keep
      -- up.
    }

-- | Arguments specific to the ChainDB, not to the ImmutableDB, VolatileDB, or
-- LedgerDB.
data ChainDbSpecificArgs f m blk = ChainDbSpecificArgs {
      cdbsBlocksToAddSize :: Word
    , cdbsCheckInFuture   :: HKD f (CheckInFuture m blk)
    , cdbsGcDelay         :: DiffTime
      -- ^ Delay between copying a block to the ImmutableDB and triggering a
      -- garbage collection for the corresponding slot on the VolatileDB.
      --
      -- The goal of the delay is to ensure that the write to the ImmutableDB
      -- has been flushed to disk before deleting the block from the
      -- VolatileDB, so that a crash won't result in the loss of the block.
    , cdbsGcInterval      :: DiffTime
      -- ^ Batch all scheduled GCs so that at most one GC happens every
      -- 'cdbsGcInterval'.
    , cdbsRegistry        :: HKD f (ResourceRegistry m)
      -- ^ TODO: the ImmutableDB takes a 'ResourceRegistry' too, but we're
      -- using it for ChainDB-specific things. Revisit these arguments.
    , cdbsTracer          :: Tracer m (TraceEvent blk)
    }

-- | Default arguments
--
-- The following fields must still be defined:
--
-- * 'cdbsTracer'
-- * 'cdbsRegistry'
-- * 'cdbsCheckInFuture'
--
-- We a 'cdbsGcDelay' of 60 seconds and a 'cdbsGcInterval' of 10 seconds, this
-- means (see the properties in "Test.Ouroboros.Storage.ChainDB.GcSchedule"):
--
-- * The length of the 'GcSchedule' queue is @<= ⌈gcDelay / gcInterval⌉ + 1@,
--   i.e., @<= 7@.
-- * The overlap (number of blocks in both the VolatileDB and the ImmutableDB)
--   is the number of blocks synced in @gcDelay + gcInterval@ = 70s. E.g, when
--   bulk syncing at 1k-2k blocks/s, this means 70k-140k blocks. During normal
--   operation, we receive 1 block/20s (for Byron /and/ for Shelley), meaning
--   at most 4 blocks.
-- * The unnecessary overlap (the blocks that we haven't GC'ed yet but could
--   have, because of batching) < the number of blocks sync in @gcInterval@.
--   E.g., when syncing at 1k-2k blocks/s, this means 10k-20k blocks. During
--   normal operation, we receive 1 block/20s, meaning at most 1 block.
defaultSpecificArgs :: Monad m => ChainDbSpecificArgs Defaults m blk
defaultSpecificArgs = ChainDbSpecificArgs {
      cdbsBlocksToAddSize = 10
    , cdbsCheckInFuture   = NoDefault
    , cdbsGcDelay         = secondsToDiffTime 60
    , cdbsGcInterval      = secondsToDiffTime 10
    , cdbsRegistry        = NoDefault
    , cdbsTracer          = nullTracer
    }

-- | Default arguments
--
-- See 'ImmutableDB.defaultArgs', 'VolatileDB.defaultArgs', 'LgrDB.defaultArgs',
-- and 'defaultSpecificArgs' for a list of which fields are not given a default
-- and must therefore be set explicitly.
defaultArgs ::
     forall m blk.
     Monad m
  => (RelativeMountPoint -> SomeHasFS m)
  -> DiskPolicy
  -> ChainDbArgs Defaults m blk
defaultArgs mkFS diskPolicy =
  toChainDbArgs (ImmutableDB.defaultArgs immFS)
                (VolatileDB.defaultArgs  volFS)
                (LgrDB.defaultArgs       lgrFS diskPolicy)
                defaultSpecificArgs
  where
    immFS, volFS, lgrFS :: SomeHasFS m

    immFS = mkFS $ RelativeMountPoint "immutable"
    volFS = mkFS $ RelativeMountPoint "volatile"
    lgrFS = mkFS $ RelativeMountPoint "ledger"

-- | Internal: split 'ChainDbArgs' into 'ImmutableDbArgs', 'VolatileDbArgs,
-- 'LgrDbArgs', and 'ChainDbSpecificArgs'.
fromChainDbArgs ::
     forall m blk f. MapHKD f
  => ChainDbArgs f m blk
  -> ( ImmutableDB.ImmutableDbArgs f m blk
     , VolatileDB.VolatileDbArgs   f m blk
     , LgrDB.LgrDbArgs             f m blk
     , ChainDbSpecificArgs         f m blk
     )
fromChainDbArgs ChainDbArgs{..} = (
      ImmutableDB.ImmutableDbArgs {
          immCacheConfig      = cdbImmutableDbCacheConfig
        , immCheckIntegrity   = cdbCheckIntegrity
        , immChunkInfo        = cdbChunkInfo
        , immCodecConfig      = mapHKD (Proxy @(f (CodecConfig blk))) configCodec cdbTopLevelConfig
        , immHasFS            = cdbHasFSImmutableDB
        , immRegistry         = cdbRegistry
        , immTracer           = contramap TraceImmutableDBEvent cdbTracer
        , immValidationPolicy = cdbImmutableDbValidation
        }
    , VolatileDB.VolatileDbArgs {
          volCheckIntegrity   = cdbCheckIntegrity
        , volCodecConfig      = mapHKD (Proxy @(f (CodecConfig blk))) configCodec cdbTopLevelConfig
        , volHasFS            = cdbHasFSVolatileDB
        , volMaxBlocksPerFile = cdbMaxBlocksPerFile
        , volValidationPolicy = cdbVolatileDbValidation
        , volTracer           = contramap TraceVolatileDBEvent cdbTracer
        }
    , LgrDB.LgrDbArgs {
          lgrTopLevelConfig   = cdbTopLevelConfig
        , lgrHasFS            = cdbHasFSLgrDB
        , lgrDiskPolicy       = cdbDiskPolicy
        , lgrGenesis          = cdbGenesis
        , lgrTracer           = contramap TraceLedgerEvent cdbTracer
        , lgrTraceLedger      = cdbTraceLedger
        }
    , ChainDbSpecificArgs {
          cdbsTracer          = cdbTracer
        , cdbsRegistry        = cdbRegistry
        , cdbsGcDelay         = cdbGcDelay
        , cdbsGcInterval      = cdbGcInterval
        , cdbsCheckInFuture   = cdbCheckInFuture
        , cdbsBlocksToAddSize = cdbBlocksToAddSize
        }
    )

-- | Internal: construct 'ChainDbArgs' from 'ImmutableDbArgs', 'VolatileDbArgs,
-- 'LgrDbArgs', and 'ChainDbSpecificArgs'.
--
-- Useful in 'defaultArgs'
toChainDbArgs ::
     ImmutableDB.ImmutableDbArgs f m blk
  -> VolatileDB.VolatileDbArgs   f m blk
  -> LgrDB.LgrDbArgs             f m blk
  -> ChainDbSpecificArgs         f m blk
  -> ChainDbArgs                 f m blk
toChainDbArgs ImmutableDB.ImmutableDbArgs {..}
              VolatileDB.VolatileDbArgs {..}
              LgrDB.LgrDbArgs {..}
              ChainDbSpecificArgs {..} = ChainDbArgs{
      -- HasFS instances
      cdbHasFSImmutableDB       = immHasFS
    , cdbHasFSVolatileDB        = volHasFS
    , cdbHasFSLgrDB             = lgrHasFS
      -- Policy
    , cdbImmutableDbValidation  = immValidationPolicy
    , cdbVolatileDbValidation   = volValidationPolicy
    , cdbMaxBlocksPerFile       = volMaxBlocksPerFile
    , cdbDiskPolicy             = lgrDiskPolicy
      -- Integration
    , cdbTopLevelConfig         = lgrTopLevelConfig
    , cdbChunkInfo              = immChunkInfo
    , cdbCheckIntegrity         = immCheckIntegrity
    , cdbGenesis                = lgrGenesis
    , cdbCheckInFuture          = cdbsCheckInFuture
    , cdbImmutableDbCacheConfig = immCacheConfig
      -- Misc
    , cdbTracer                 = cdbsTracer
    , cdbTraceLedger            = lgrTraceLedger
    , cdbRegistry               = immRegistry
    , cdbGcDelay                = cdbsGcDelay
    , cdbGcInterval             = cdbsGcInterval
    , cdbBlocksToAddSize        = cdbsBlocksToAddSize
    }

{-------------------------------------------------------------------------------
  Relative mount points
-------------------------------------------------------------------------------}

-- | A relative path for a 'MountPoint'
--
-- The root is determined by context.
newtype RelativeMountPoint = RelativeMountPoint FilePath
