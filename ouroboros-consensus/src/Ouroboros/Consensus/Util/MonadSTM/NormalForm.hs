module Ouroboros.Consensus.Util.MonadSTM.NormalForm (
    module Control.Monad.Class.MonadSTM.Strict
  , module Ouroboros.Consensus.Util.MonadSTM.StrictMVar
  , newTVarIO
  , newMVar
  , newEmptyMVar
    -- * Temporary
  , uncheckedNewTVarM
  , uncheckedNewMVar
  , uncheckedNewEmptyMVar
  ) where

import           GHC.Stack

import           Cardano.Prelude (NoUnexpectedThunks (..),
                     unsafeNoUnexpectedThunks)

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

newTVarIO :: (MonadSTM m, HasCallStack, NoUnexpectedThunks a)
          => a -> m (StrictTVar m a)
newTVarIO = Strict.newTVarWithInvariantIO unsafeNoUnexpectedThunks

newMVar :: (MonadSTM m, HasCallStack, NoUnexpectedThunks a)
        => a -> m (StrictMVar m a)
newMVar = Strict.newMVarWithInvariant unsafeNoUnexpectedThunks

newEmptyMVar :: (MonadSTM m, NoUnexpectedThunks a) => a -> m (StrictMVar m a)
newEmptyMVar = Strict.newEmptyMVarWithInvariant unsafeNoUnexpectedThunks

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
