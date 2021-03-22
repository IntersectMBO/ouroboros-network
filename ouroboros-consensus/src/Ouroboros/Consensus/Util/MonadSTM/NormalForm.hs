module Ouroboros.Consensus.Util.MonadSTM.NormalForm (
    module Control.Monad.Class.MonadSTM.Strict
  , module Ouroboros.Consensus.Util.MonadSTM.StrictMVar
  , newEmptyMVar
  , newMVar
  , newTVarIO
    -- * Temporary
  , uncheckedNewEmptyMVar
  , uncheckedNewMVar
  , uncheckedNewTVarM
  ) where

import           GHC.Stack
import           NoThunks.Class (NoThunks (..), unsafeNoThunks)

import           Control.Monad.Class.MonadSTM.Strict hiding (newEmptyTMVarIO,
                     newTMVar, newTMVarIO, newTVar, newTVarIO,
                     newTVarWithInvariantIO)
import           Ouroboros.Consensus.Util.MonadSTM.StrictMVar hiding
                     (newEmptyMVar, newEmptyMVarWithInvariant, newMVar,
                     newMVarWithInvariant)

import qualified Control.Monad.Class.MonadSTM.Strict as Strict
import qualified Ouroboros.Consensus.Util.MonadSTM.StrictMVar as Strict

{-------------------------------------------------------------------------------
  Wrappers that check for thunks
-------------------------------------------------------------------------------}

newTVarIO :: (MonadSTM m, HasCallStack, NoThunks a)
          => a -> m (StrictTVar m a)
newTVarIO = Strict.newTVarWithInvariantIO (fmap show . unsafeNoThunks)

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
