module Ouroboros.Consensus.Util.MonadSTM.NormalForm (
    module LazySTM
  , module Ouroboros.Consensus.Util.MonadSTM.StrictMVar
  , module StrictSTM
  , newEmptyMVar
  , newMVar
  , newTVar
  , newTVarIO
    -- * Temporary
  , uncheckedNewEmptyMVar
  , uncheckedNewMVar
  , uncheckedNewTVarM
  ) where

import qualified Control.Concurrent.Class.MonadSTM.Strict as Strict
import           Control.Concurrent.Class.MonadSTM.Strict.TMVar as StrictSTM hiding
                     (newTMVar, newTMVarIO)
import           Control.Concurrent.Class.MonadSTM.Strict.TVar as StrictSTM hiding
                     (newTVar, newTVarIO, newTVarWithInvariantIO)
import           Control.Concurrent.Class.MonadSTM.TBQueue as LazySTM
import           Control.Concurrent.Class.MonadSTM.TQueue as LazySTM
import           Control.Monad.Class.MonadSTM as StrictSTM
import           GHC.Stack
import           NoThunks.Class (NoThunks (..), unsafeNoThunks)
import           Ouroboros.Consensus.Util.MonadSTM.StrictMVar hiding
                     (newEmptyMVar, newEmptyMVarWithInvariant, newMVar,
                     newMVarWithInvariant)
import qualified Ouroboros.Consensus.Util.MonadSTM.StrictMVar as Strict

-- TODO: use strict versions of 'TQueue' and 'TBQueue'.  Previously the
-- 'Control.Monad.Class.MonadSTM.Strict' was imported which
-- exported lazy 'TQueue' and 'TBQueue',  I (@coot) think that the intention was
-- to use strict versions.

{-------------------------------------------------------------------------------
  Wrappers that check for thunks
-------------------------------------------------------------------------------}

newTVarIO :: (MonadSTM m, HasCallStack, NoThunks a)
          => a -> m (StrictTVar m a)
newTVarIO = Strict.newTVarWithInvariantIO (fmap show . unsafeNoThunks)

newTVar :: (MonadSTM m, HasCallStack, NoThunks a)
          => a -> STM m (StrictTVar m a)
newTVar = Strict.newTVarWithInvariant (fmap show . unsafeNoThunks)

newMVar :: (MonadSTM m, HasCallStack, NoThunks a)
        => a -> m (StrictMVar m a)
newMVar = Strict.newMVarWithInvariant (fmap show . unsafeNoThunks)

newEmptyMVar :: (MonadSTM m, NoThunks a) => a -> m (StrictMVar m a)
newEmptyMVar = Strict.newEmptyMVarWithInvariant (fmap show . unsafeNoThunks)

{-------------------------------------------------------------------------------
  Unchecked wrappers (where we don't check for thunks)

  These will eventually be removed.
-------------------------------------------------------------------------------}

uncheckedNewTVarM :: MonadSTM m => a -> m (StrictTVar m a)
uncheckedNewTVarM = Strict.newTVarIO

uncheckedNewMVar :: MonadSTM m => a -> m (StrictMVar m a)
uncheckedNewMVar = Strict.newMVar

uncheckedNewEmptyMVar :: MonadSTM m => a -> m (StrictMVar m a)
uncheckedNewEmptyMVar = Strict.newEmptyMVar
