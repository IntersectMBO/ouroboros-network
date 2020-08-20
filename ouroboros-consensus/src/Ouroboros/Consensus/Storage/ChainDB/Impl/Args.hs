{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}

module Ouroboros.Consensus.Storage.ChainDB.Impl.Args
  ( ChainDbArgs (..)
  , ChainDbSpecificArgs (..)
  , defaultArgs
    -- * Internal
  , fromChainDbArgs
  ) where

import           Data.Time.Clock (DiffTime, secondsToDiffTime)

import           Control.Tracer (Tracer, contramap)

import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Fragment.InFuture (CheckInFuture)
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Util.ResourceRegistry (ResourceRegistry)

import           Ouroboros.Consensus.Storage.FS.API

import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.LgrDB as LgrDB
import           Ouroboros.Consensus.Storage.ChainDB.Impl.Types
                     (TraceEvent (..))
import           Ouroboros.Consensus.Storage.ImmutableDB (ChunkInfo)
import qualified Ouroboros.Consensus.Storage.ImmutableDB as ImmutableDB
import qualified Ouroboros.Consensus.Storage.VolatileDB as VolatileDB

{-------------------------------------------------------------------------------
  Arguments
-------------------------------------------------------------------------------}

data ChainDbArgs m blk = ChainDbArgs {

      -- HasFS instances
      cdbHasFSImmutableDB       :: SomeHasFS m
    , cdbHasFSVolatileDB        :: SomeHasFS m
    , cdbHasFSLgrDB             :: SomeHasFS m

      -- Policy
    , cdbImmutableDbValidation  :: ImmutableDB.ValidationPolicy
    , cdbVolatileDbValidation   :: VolatileDB.BlockValidationPolicy
    , cdbMaxBlocksPerFile       :: VolatileDB.BlocksPerFile
    , cdbParamsLgrDB            :: LgrDB.LedgerDbParams
    , cdbDiskPolicy             :: LgrDB.DiskPolicy

      -- Integration
    , cdbTopLevelConfig         :: TopLevelConfig blk
    , cdbChunkInfo              :: ChunkInfo
    , cdbCheckIntegrity         :: blk -> Bool
    , cdbGenesis                :: m (ExtLedgerState blk)
    , cdbCheckInFuture          :: CheckInFuture m blk
    , cdbImmutableDbCacheConfig :: ImmutableDB.CacheConfig

      -- Misc
    , cdbTracer                 :: Tracer m (TraceEvent blk)
    , cdbTraceLedger            :: Tracer m (LgrDB.LedgerDB blk)
    , cdbRegistry               :: ResourceRegistry m
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
data ChainDbSpecificArgs m blk = ChainDbSpecificArgs {
      cdbsTracer          :: Tracer m (TraceEvent blk)
    , cdbsRegistry        :: ResourceRegistry m
      -- ^ TODO: the ImmutableDB takes a 'ResourceRegistry' too, but we're
      -- using it for ChainDB-specific things. Revisit these arguments.
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
    , cdbsCheckInFuture   :: CheckInFuture m blk
    , cdbsBlocksToAddSize :: Word
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
defaultSpecificArgs :: ChainDbSpecificArgs m blk
defaultSpecificArgs = ChainDbSpecificArgs{
      cdbsGcDelay         = secondsToDiffTime 60
    , cdbsGcInterval      = secondsToDiffTime 10
    , cdbsBlocksToAddSize = 10
      -- Fields without a default
    , cdbsTracer          = error "no default for cdbsTracer"
    , cdbsRegistry        = error "no default for cdbsRegistry"
    , cdbsCheckInFuture   = error "no default for cdbsCheckInFuture"
    }

-- | Default arguments for use within IO
--
-- See 'ImmutableDB.defaultArgs', 'VolatileDB.defaultArgs', 'LgrDB.defaultArgs',
-- and 'defaultSpecificArgs' for a list of which fields are not given a default
-- and must therefore be set explicitly.
defaultArgs :: FilePath -> ChainDbArgs IO blk
defaultArgs fp = toChainDbArgs (ImmutableDB.defaultArgs fp)
                               (VolatileDB.defaultArgs  fp)
                               (LgrDB.defaultArgs       fp)
                               defaultSpecificArgs


-- | Internal: split 'ChainDbArgs' into 'ImmutableDbArgs', 'VolatileDbArgs,
-- 'LgrDbArgs', and 'ChainDbSpecificArgs'.
fromChainDbArgs ::
     ChainDbArgs m blk
  -> ( ImmutableDB.ImmutableDbArgs m blk
     , VolatileDB.VolatileDbArgs   m blk
     , LgrDB.LgrDbArgs             m blk
     , ChainDbSpecificArgs         m blk
     )
fromChainDbArgs ChainDbArgs{..} = (
      ImmutableDB.ImmutableDbArgs {
          immCacheConfig      = cdbImmutableDbCacheConfig
        , immCheckIntegrity   = cdbCheckIntegrity
        , immChunkInfo        = cdbChunkInfo
        , immCodecConfig      = configCodec cdbTopLevelConfig
        , immHasFS            = cdbHasFSImmutableDB
        , immRegistry         = cdbRegistry
        , immTracer           = contramap TraceImmutableDBEvent cdbTracer
        , immValidationPolicy = cdbImmutableDbValidation
        }
    , VolatileDB.VolatileDbArgs {
          volCheckIntegrity   = cdbCheckIntegrity
        , volCodecConfig      = configCodec cdbTopLevelConfig
        , volHasFS            = cdbHasFSVolatileDB
        , volMaxBlocksPerFile = cdbMaxBlocksPerFile
        , volValidationPolicy = cdbVolatileDbValidation
        , volTracer           = contramap TraceVolatileDBEvent cdbTracer
        }
    , LgrDB.LgrDbArgs {
          lgrTopLevelConfig       = cdbTopLevelConfig
        , lgrHasFS                = cdbHasFSLgrDB
        , lgrParams               = cdbParamsLgrDB
        , lgrDiskPolicy           = cdbDiskPolicy
        , lgrGenesis              = cdbGenesis
        , lgrTracer               = contramap TraceLedgerEvent cdbTracer
        , lgrTraceLedger          = cdbTraceLedger
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
     ImmutableDB.ImmutableDbArgs m blk
  -> VolatileDB.VolatileDbArgs   m blk
  -> LgrDB.LgrDbArgs             m blk
  -> ChainDbSpecificArgs         m blk
  -> ChainDbArgs                 m blk
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
    , cdbParamsLgrDB            = lgrParams
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
