{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Ouroboros.Consensus.Storage.LedgerDB.HD.BackingStore (
    -- * Backing store interface
    BackingStore (..)
  , BackingStorePath (..)
  , BackingStoreValueHandle (..)
  , bsRead
  , withBsValueHandle
  ) where

import           NoThunks.Class (NoThunks, OnlyCheckWhnfNamed (..))

import           Cardano.Slotting.Slot (SlotNo, WithOrigin (..))

import qualified Ouroboros.Consensus.Storage.FS.API as FS
import qualified Ouroboros.Consensus.Storage.FS.API.Types as FS
import           Ouroboros.Consensus.Util.IOLike (IOLike)
import qualified Ouroboros.Consensus.Util.IOLike as IOLike

{-------------------------------------------------------------------------------
  Backing store interface
-------------------------------------------------------------------------------}

-- | A backing store for a map
data BackingStore m keys values diff = BackingStore {
    -- | Close the backing store
    --
    -- Other methods throw exceptions if called on a closed store.
    bsClose       :: !(m ())
    -- | Create a persistent copy
    --
    -- Each backing store implementation will offer a way to initialize itself
    -- from such a path.
  , bsCopy        :: !(FS.SomeHasFS m -> BackingStorePath -> m ())
    -- | Open a 'BackingStoreValueHandle' capturing the current value of the
    -- entire database
  , bsValueHandle :: !(m (WithOrigin SlotNo, BackingStoreValueHandle m keys values))
    -- | Apply a valid diff to the contents of the backing store
  , bsWrite       :: !(SlotNo -> diff -> m ())
  }

-- | TODO Is there a good way to not assume that any function that creates a
-- 'BackingStore' doesn't hold space leaks in its closure?
deriving via OnlyCheckWhnfNamed "BackingStore" (BackingStore m keys values diff)
  instance NoThunks (BackingStore m keys values diff)

newtype BackingStorePath = BackingStorePath FS.FsPath

-- | An ephemeral handle to an immutable value of the entire database
--
-- The performance cost is usually minimal unless this handle is held open too
-- long.
data BackingStoreValueHandle m keys values = BackingStoreValueHandle {
    -- | Close the handle
    --
    -- Other methods throw exceptions if called on a closed handle.
    bsvhClose :: !(m ())
    -- | Read the given keys from the handle
    --
    -- Absent keys will merely not be present in the result instead of causing a
    -- failure or an exception.
  , bsvhRead  :: !(keys -> m values)
  }

-- | A combination of 'bsValueHandle' and 'bsvhRead'
bsRead ::
     IOLike m
  => BackingStore m keys values diff
  -> keys
  -> m (WithOrigin SlotNo, values)
bsRead store keys = withBsValueHandle store $ \slot vh -> do
    values <- bsvhRead vh keys
    pure (slot, values)

-- | A 'IOLike.bracket'ed 'bsValueHandle'
withBsValueHandle ::
     IOLike m
  => BackingStore m keys values diff
  -> (WithOrigin SlotNo -> BackingStoreValueHandle m keys values -> m a)
  -> m a
withBsValueHandle store kont =
    IOLike.bracket
      (bsValueHandle store)
      (bsvhClose . snd)
      (uncurry kont)
