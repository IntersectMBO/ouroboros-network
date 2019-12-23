{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wredundant-constraints #-}
module Ouroboros.Storage.ImmutableDB.Impl.Index
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

import           Control.Monad.Class.MonadThrow

import           Cardano.Prelude (NoUnexpectedThunks (..), OnlyCheckIsWHNF (..))

import           Ouroboros.Consensus.Block (IsEBB)
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry

import           Ouroboros.Storage.Common (EpochNo)
import           Ouroboros.Storage.FS.API (HasFS)
import           Ouroboros.Storage.FS.API.Types (AllowExisting, Handle)
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling)

import           Ouroboros.Storage.ImmutableDB.Impl.Index.Cache
                     (CacheConfig (..))
import qualified Ouroboros.Storage.ImmutableDB.Impl.Index.Cache as Cache
import           Ouroboros.Storage.ImmutableDB.Impl.Index.Primary
                     (SecondaryOffset)
import qualified Ouroboros.Storage.ImmutableDB.Impl.Index.Primary as Primary
import           Ouroboros.Storage.ImmutableDB.Impl.Index.Secondary (BlockSize)
import qualified Ouroboros.Storage.ImmutableDB.Impl.Index.Secondary as Secondary
import           Ouroboros.Storage.ImmutableDB.Layout (RelativeSlot)
import           Ouroboros.Storage.ImmutableDB.Types (HashInfo,
                     ImmutableDBError, TraceCacheEvent, WithBlockSize (..))

{------------------------------------------------------------------------------
  Index
------------------------------------------------------------------------------}

-- | Bundle the operations on the primary and secondary index that touch the
-- files. This allows us to easily introduce an intermediary caching layer.
data Index m hash h = Index
  { -- | See 'Primary.readOffsets'
    readOffsets
      :: forall t. (HasCallStack, Traversable t)
      => EpochNo
      -> t RelativeSlot
      -> m (t (Maybe SecondaryOffset))

    -- |  See 'Primary.readFirstFilledSlot'
  , readFirstFilledSlot
      :: HasCallStack
      => EpochNo
      -> m (Maybe RelativeSlot)

    -- | See 'Primary.open'
  , openPrimaryIndex
      :: HasCallStack
      => EpochNo
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
      => EpochNo
      -> t (IsEBB, SecondaryOffset)
      -> m (t (Secondary.Entry hash, BlockSize))

    -- | See 'Secondary.readAllEntries'
  , readAllEntries
      :: HasCallStack
      => SecondaryOffset
      -> EpochNo
      -> (Secondary.Entry hash -> Bool)
      -> Word64
      -> IsEBB
      -> m [WithBlockSize (Secondary.Entry hash)]

    -- | See 'Secondary.appendEntry'
  , appendEntry
      :: HasCallStack
      => EpochNo
      -> Handle h
      -> WithBlockSize (Secondary.Entry hash)
      -> m Word64

    -- | Reset the index, drop all cached information.
    --
    -- To be called when the index files were manipulated without using
    -- 'Index'. Any cached information could have become stale.
    --
    -- NOTE: this will only used in the testsuite, when we need to truncate.
  , reset
      :: HasCallStack
      => EpochNo  --  The (new) current epoch
      -> m ()
  }
  deriving NoUnexpectedThunks via OnlyCheckIsWHNF "Index" (Index m hash h)

-- | See 'Primary.readOffset'.
readOffset
  :: Functor m
  => Index m hash h
  -> EpochNo
  -> RelativeSlot
  -> m (Maybe SecondaryOffset)
readOffset index epoch slot = runIdentity <$>
    readOffsets index epoch (Identity slot)

-- | See 'Secondary.readEntry'.
readEntry
  :: Functor m
  => Index m hash h
  -> EpochNo
  -> IsEBB
  -> SecondaryOffset
  -> m (Secondary.Entry hash, BlockSize)
readEntry index epoch isEBB slotOffset = runIdentity <$>
    readEntries index epoch (Identity (isEBB, slotOffset))

{------------------------------------------------------------------------------
  File-backed index
------------------------------------------------------------------------------}

fileBackedIndex
  :: forall m hash h. MonadThrow m
  => HasFS m h
  -> ErrorHandling ImmutableDBError m
  -> HashInfo hash
  -> Index m hash h
fileBackedIndex hasFS err hashInfo = Index
    { readOffsets         = Primary.readOffsets         hasFS err
    , readFirstFilledSlot = Primary.readFirstFilledSlot hasFS err
    , openPrimaryIndex    = Primary.open                hasFS
    , appendOffsets       = Primary.appendOffsets       hasFS
    , readEntries         = Secondary.readEntries       hasFS err hashInfo
    , readAllEntries      = Secondary.readAllEntries    hasFS err hashInfo
    , appendEntry         = \_epoch h (WithBlockSize _ entry) ->
                            Secondary.appendEntry       hasFS     hashInfo h entry
      -- Nothing to reset, as nothing is cached
    , reset              = \_newCurEpoch -> return ()
    }

{------------------------------------------------------------------------------
  Cached index
------------------------------------------------------------------------------}

-- | Caches the current epoch's indices as well as a number of past epochs'
-- indices.
--
-- Spawns a background thread to expire past epochs from the cache that
-- haven't been used for a while.
cachedIndex
  :: forall m hash h. (IOLike m, NoUnexpectedThunks hash)
  => HasFS m h
  -> ErrorHandling ImmutableDBError m
  -> HashInfo hash
  -> ResourceRegistry m
  -> Tracer m TraceCacheEvent
  -> CacheConfig
  -> EpochNo  -- ^ Current epoch
  -> m (Index m hash h)
cachedIndex hasFS err hashInfo registry tracer cacheConfig epoch = do
    cacheEnv <- Cache.newEnv hasFS err hashInfo registry tracer cacheConfig epoch
    return Index
      { readOffsets         = Cache.readOffsets         cacheEnv
      , readFirstFilledSlot = Cache.readFirstFilledSlot cacheEnv
      , openPrimaryIndex    = Cache.openPrimaryIndex    cacheEnv
      , appendOffsets       = Cache.appendOffsets       cacheEnv
      , readEntries         = Cache.readEntries         cacheEnv
      , readAllEntries      = Cache.readAllEntries      cacheEnv
      , appendEntry         = Cache.appendEntry         cacheEnv
      , reset               = Cache.reset               cacheEnv
      }
