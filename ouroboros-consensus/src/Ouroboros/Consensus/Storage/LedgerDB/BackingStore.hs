{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE StandaloneDeriving         #-}

-- | A store for key-value maps that can be extended with deltas.
--
-- When we talk about deltas we mean differences on key-value entries, for
-- example a deletion of a key-value entry or an insertion.
--
-- Its intended use is for storing data structures from the 'LedgerState' and
-- update them with differences produced by executing the Ledger rules.
module Ouroboros.Consensus.Storage.LedgerDB.BackingStore (
    -- * Backing store interface
    BackingStore (..)
  , BackingStoreInitialiser (..)
  , BackingStorePath (..)
  , BackingStoreValueHandle (..)
  , InitFrom (..)
  , RangeQuery (..)
  , bsRead
  , initFromCopy
  , initFromValues
  , withBsValueHandle
    -- * Ledger DB wrappers
  , LedgerBackingStore (..)
  , LedgerBackingStoreInitialiser (..)
  , LedgerBackingStoreValueHandle (..)
  ) where

import           Cardano.Slotting.Slot (SlotNo, WithOrigin (..))
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks, OnlyCheckWhnfNamed (..))
import           Ouroboros.Consensus.Ledger.Tables
import           Ouroboros.Consensus.Util.IOLike (IOLike)
import qualified Ouroboros.Consensus.Util.IOLike as IOLike
import qualified System.FS.API as FS
import qualified System.FS.API.Types as FS

{-------------------------------------------------------------------------------
  Backing store interface
-------------------------------------------------------------------------------}

data InitFrom values =
    InitFromValues !(WithOrigin SlotNo) !values
  | InitFromCopy !BackingStorePath

-- | Initialisation for a backing store
newtype BackingStoreInitialiser m keys values diff = BackingStoreInitialiser {
    bsiInit :: FS.SomeHasFS m -> InitFrom values -> m (BackingStore m keys values diff)
  }
  deriving newtype NoThunks

initFromValues ::
     BackingStoreInitialiser m keys values diff
  -> FS.SomeHasFS m
  -> WithOrigin SlotNo
  -> values
  -> m (BackingStore m keys values diff)
initFromValues bsi shfs sl vs = bsiInit bsi shfs (InitFromValues sl vs)

initFromCopy ::
     BackingStoreInitialiser m keys values diff
  -> FS.SomeHasFS m
  -> BackingStorePath
  -> m (BackingStore m keys values diff)
initFromCopy bsi shfs bsp = bsiInit bsi shfs (InitFromCopy bsp)

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
    --
    -- The destination path must not already exist. After this operation, it
    -- will be a directory.
  , bsCopy        :: !(FS.SomeHasFS m -> BackingStorePath -> m ())
    -- | Open a 'BackingStoreValueHandle' capturing the current value of the
    -- entire database
  , bsValueHandle :: !(m (WithOrigin SlotNo, BackingStoreValueHandle m keys values))
    -- | Apply a valid diff to the contents of the backing store
  , bsWrite       :: !(SlotNo -> diff -> m ())
  }

deriving via OnlyCheckWhnfNamed "BackingStore" (BackingStore m keys values diff)
  instance NoThunks (BackingStore m keys values diff)

newtype BackingStorePath = BackingStorePath FS.FsPath
  deriving stock (Show, Eq, Ord)
  deriving newtype NoThunks

-- | An ephemeral handle to an immutable value of the entire database
--
-- The performance cost is usually minimal unless this handle is held open too
-- long. We expect clients of the BackingStore to not retain handles for a long
-- time.
data BackingStoreValueHandle m keys values = BackingStoreValueHandle {
    -- | Close the handle
    --
    -- Other methods throw exceptions if called on a closed handle.
    bsvhClose     :: !(m ())
    -- | See 'RangeQuery'
  , bsvhRangeRead :: !(RangeQuery keys -> m values)
    -- | Read the given keys from the handle
    --
    -- Absent keys will merely not be present in the result instead of causing a
    -- failure or an exception.
  , bsvhRead      :: !(keys -> m values)
  }

data RangeQuery keys = RangeQuery {
      -- | The result of this range query begin at first key that is strictly
      -- greater than the greatest key in 'rqPrev'.
      --
      -- If the given set of keys is 'Just' but contains no keys, then the query
      -- will return no results. (This is the steady-state once a looping range
      -- query reaches the end of the table.)
      rqPrev  :: Maybe keys
      -- | Roughly how many values to read.
      --
      -- The query may return a different number of values than this even if it
      -- has not reached the last key. The only crucial invariant is that the
      -- query only returns an empty map if there are no more keys to read on
      -- disk.
      --
      -- FIXME: #4398 can we satisfy this invariant if we read keys from disk
      -- but all of them were deleted in the changelog?
    , rqCount :: !Int
    }
    deriving stock (Show, Eq)

deriving via OnlyCheckWhnfNamed "BackingStoreValueHandle" (BackingStoreValueHandle m keys values)
  instance NoThunks (BackingStoreValueHandle m keys values)

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

{-------------------------------------------------------------------------------
  Ledger DB wrappers
-------------------------------------------------------------------------------}

newtype LedgerBackingStoreInitialiser m l = LedgerBackingStoreInitialiser
  (BackingStoreInitialiser m
    (LedgerTables l KeysMK)
    (LedgerTables l ValuesMK)
    (LedgerTables l DiffMK)
  )
  deriving newtype (NoThunks)

-- | A handle to the backing store for the ledger tables
newtype LedgerBackingStore m l = LedgerBackingStore
    (BackingStore m
      (LedgerTables l KeysMK)
      (LedgerTables l ValuesMK)
      (LedgerTables l DiffMK)
    )
  deriving newtype (NoThunks)

-- | A handle to the backing store for the ledger tables
data LedgerBackingStoreValueHandle m l = LedgerBackingStoreValueHandle
    !(WithOrigin SlotNo)
    !(BackingStoreValueHandle m
      (LedgerTables l KeysMK)
      (LedgerTables l ValuesMK)
    )
  deriving stock    (Generic)
  deriving anyclass (NoThunks)

