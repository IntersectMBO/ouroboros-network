{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}

-- | LMDB resource status with read-append-write locking
module Ouroboros.Consensus.Storage.LedgerDB.BackingStore.LMDB.Status (
    -- * Status
    Status (..)
  , StatusLock
    -- * Locks
  , new
  , withReadAccess
  , withWriteAccess
  ) where

import           Control.Exception (Exception)
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)
import           Ouroboros.Consensus.Util.IOLike (IOLike, MonadThrow (throwIO))
import           Ouroboros.Consensus.Util.MonadSTM.RAWLock (RAWLock)
import qualified Ouroboros.Consensus.Util.MonadSTM.RAWLock as RAW

{-------------------------------------------------------------------------------
  Status
-------------------------------------------------------------------------------}

-- | A 'RAWLock' for 'Status'.
newtype StatusLock m = StatusLock { getStatusLock :: RAWLock m Status }

-- | Whether a resource is open or closed.
--
-- Resources that we keep track of are: (i) the full LMDB backing store, and
-- (ii) each of the LMDB backing store value handles.
data Status = Open | Closed
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NoThunks)

{-------------------------------------------------------------------------------
  Locks
-------------------------------------------------------------------------------}

-- | Create a new 'StatusLock'.
new :: IOLike m => Status -> m (StatusLock m)
new st = StatusLock <$> RAW.new st

-- | A variant of 'RAW.withWriteAccess' that throws an exception if @'Status' ==
-- 'Closed'@.
--
-- Note: contrary to 'RAW.withWriteAccess', the action to perform with the
-- acquired lock is not of type @'Status' -> ('Status', a)@. The 'Status' is
-- known to be 'Open', or an exception would have been thrown.
withWriteAccess ::
     (IOLike m, Exception e)
  => StatusLock m
  -> e                -- ^ The exception to throw
  -> m (Status, a)    -- ^ Action to perform, possibly updating the 'Status'
  -> m a
withWriteAccess lock exc k =
  RAW.withWriteAccess (getStatusLock lock) $ \case
    Open   -> k
    Closed -> throwIO exc

-- | A variant of 'RAW.withReadAccess' that throws an exception if @'Status' ==
-- 'Closed'@.
--
-- Note: contrary to 'RAW.withReadAccess', the action to perform with the
-- acquired lock is not of type @'Status' -> a@. The 'Status' is known to be
-- 'Open', or an exception would have been thrown.
withReadAccess ::
     (IOLike m, Exception e)
  => StatusLock m
  -> e                -- ^ The exception to throw
  -> m a              -- ^ Action to perform
  -> m a
withReadAccess lock exc k =
  RAW.withReadAccess (getStatusLock lock) $ \case
    Open   -> k
    Closed -> throwIO exc
