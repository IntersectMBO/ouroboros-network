{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index
  ( -- * Index
    Index (..)
  , readOffset
  , readEntry
    -- * File-backed index
  , fileBackedIndex
    -- * Cached index
  , cachedIndex
  , CacheConfig (..)
  ) where

import           Control.Tracer (Tracer)
import           Data.Functor.Identity (Identity (..))
import           Data.Word (Word64)
import           GHC.Stack (HasCallStack)

import           Cardano.Prelude (OnlyCheckIsWHNF (..))

import           Ouroboros.Consensus.Block (IsEBB)
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry

import           Ouroboros.Consensus.Storage.FS.API (HasFS)
import           Ouroboros.Consensus.Storage.FS.API.Types (AllowExisting,
                     Handle)

import           Ouroboros.Consensus.Storage.ImmutableDB.Chunks
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index.Cache
                     (CacheConfig (..))
import qualified Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index.Cache as Cache
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index.Primary
                     (SecondaryOffset)
import qualified Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index.Primary as Primary
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index.Secondary
                     (BlockSize)
import qualified Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index.Secondary as Secondary
import           Ouroboros.Consensus.Storage.ImmutableDB.Types (HashInfo,
                     TraceCacheEvent, WithBlockSize (..))

{------------------------------------------------------------------------------
  Index
------------------------------------------------------------------------------}

-- | Bundle the operations on the primary and secondary index that touch the
-- files. This allows us to easily introduce an intermediary caching layer.
data Index m hash h = Index
  { -- | See 'Primary.readOffsets'
    readOffsets
      :: forall t. (HasCallStack, Traversable t)
      => ChunkNo
      -> t RelativeSlot
      -> m (t (Maybe SecondaryOffset))

    -- |  See 'Primary.readFirstFilledSlot'
  , readFirstFilledSlot
      :: HasCallStack
      => ChunkNo
      -> m (Maybe RelativeSlot)

    -- | See 'Primary.open'
  , openPrimaryIndex
      :: HasCallStack
      => ChunkNo
      -> AllowExisting
      -> m (Handle h)

    -- | See 'Primary.appendOffsets'
  , appendOffsets
      :: forall f. (HasCallStack, Foldable f)
      => Handle h
      -> f SecondaryOffset
      -> m ()

    -- | See 'Secondary.readEntries'
  , readEntries
      :: forall t. (HasCallStack, Traversable t)
      => ChunkNo
      -> t (IsEBB, SecondaryOffset)
      -> m (t (Secondary.Entry hash, BlockSize))

    -- | See 'Secondary.readAllEntries'
  , readAllEntries
      :: HasCallStack
      => SecondaryOffset
      -> ChunkNo
      -> (Secondary.Entry hash -> Bool)
      -> Word64
      -> IsEBB
      -> m [WithBlockSize (Secondary.Entry hash)]

    -- | See 'Secondary.appendEntry'
  , appendEntry
      :: HasCallStack
      => ChunkNo
      -> Handle h
      -> WithBlockSize (Secondary.Entry hash)
      -> m Word64

    -- | Close the index and stop any background threads.
    --
    -- Should be called when the ImmutableDB is closed.
  , close
      :: HasCallStack
      => m ()

    -- | Restart a closed index using the given chunk as the current chunk,
    -- drop all previously cached information.
    --
    -- NOTE: this will only used in the testsuite, when we need to truncate.
  , restart
      :: HasCallStack
      => ChunkNo
      -> m ()
  }
  deriving NoUnexpectedThunks via OnlyCheckIsWHNF "Index" (Index m hash h)

-- | See 'Primary.readOffset'.
readOffset
  :: Functor m
  => Index m hash h
  -> ChunkNo
  -> RelativeSlot
  -> m (Maybe SecondaryOffset)
readOffset index chunk slot = runIdentity <$>
    readOffsets index chunk (Identity slot)

-- | See 'Secondary.readEntry'.
readEntry
  :: Functor m
  => Index m hash h
  -> ChunkNo
  -> IsEBB
  -> SecondaryOffset
  -> m (Secondary.Entry hash, BlockSize)
readEntry index chunk isEBB slotOffset = runIdentity <$>
    readEntries index chunk (Identity (isEBB, slotOffset))

{------------------------------------------------------------------------------
  File-backed index
------------------------------------------------------------------------------}

fileBackedIndex
  :: forall m hash h. MonadCatch m
  => HasFS m h
  -> ChunkInfo
  -> HashInfo hash
  -> Index m hash h
fileBackedIndex hasFS chunkInfo hashInfo = Index
    { readOffsets         = Primary.readOffsets         hasFS
    , readFirstFilledSlot = Primary.readFirstFilledSlot hasFS chunkInfo
    , openPrimaryIndex    = Primary.open                hasFS
    , appendOffsets       = Primary.appendOffsets       hasFS
    , readEntries         = Secondary.readEntries       hasFS hashInfo
    , readAllEntries      = Secondary.readAllEntries    hasFS hashInfo
    , appendEntry         = \_chunk h (WithBlockSize _ entry) ->
                            Secondary.appendEntry       hasFS hashInfo h entry
      -- Nothing to do
    , close               = return ()
    , restart             = \_newCurChunk -> return ()
    }

{------------------------------------------------------------------------------
  Cached index
------------------------------------------------------------------------------}

-- | Caches the current chunk's indices as well as a number of past chunk's
-- indices.
--
-- Spawns a background thread to expire past chunks from the cache that
-- haven't been used for a while.
cachedIndex
  :: forall m hash h. (IOLike m, NoUnexpectedThunks hash)
  => HasFS m h
  -> HashInfo hash
  -> ResourceRegistry m
  -> Tracer m TraceCacheEvent
  -> CacheConfig
  -> ChunkInfo
  -> ChunkNo  -- ^ Current chunk
  -> m (Index m hash h)
cachedIndex hasFS hashInfo registry tracer cacheConfig chunkInfo chunk = do
    cacheEnv <- Cache.newEnv
                  hasFS
                  hashInfo
                  registry
                  tracer
                  cacheConfig
                  chunkInfo
                  chunk
    return Index
      { readOffsets         = Cache.readOffsets         cacheEnv
      , readFirstFilledSlot = Cache.readFirstFilledSlot cacheEnv
      , openPrimaryIndex    = Cache.openPrimaryIndex    cacheEnv
      , appendOffsets       = Cache.appendOffsets       cacheEnv
      , readEntries         = Cache.readEntries         cacheEnv
      , readAllEntries      = Cache.readAllEntries      cacheEnv
      , appendEntry         = Cache.appendEntry         cacheEnv
      , close               = Cache.close               cacheEnv
      , restart             = Cache.restart             cacheEnv
      }
