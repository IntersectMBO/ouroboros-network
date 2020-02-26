module Ouroboros.Consensus.Util.MonadSTM.NormalForm (
    module Control.Monad.Class.MonadSTM.Strict
  , module Ouroboros.Consensus.Util.MonadSTM.StrictMVar
  , newTVarM
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

import           Control.Monad.Class.MonadSTM.Strict hiding (newEmptyTMVarM,
                     newTMVar, newTMVarM, newTVar, newTVarM,
                     newTVarWithInvariantM)
import           Ouroboros.Consensus.Util.MonadSTM.StrictMVar hiding
                     (newEmptyMVar, newEmptyMVarWithInvariant, newMVar,
                     newMVarWithInvariant)

import qualified Control.Monad.Class.MonadSTM.Strict as Strict
import qualified Ouroboros.Consensus.Util.MonadSTM.StrictMVar as Strict

{-------------------------------------------------------------------------------
  Wrappers that check for thunks
-------------------------------------------------------------------------------}

newTVarM :: (MonadSTM m, HasCallStack, NoUnexpectedThunks a)
         => a -> m (StrictTVar m a)
newTVarM = Strict.newTVarWithInvariantM unsafeNoUnexpectedThunks

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
uncheckedNewTVarM = Strict.newTVarM

uncheckedNewMVar :: MonadSTM m => a -> m (StrictMVar m a)
uncheckedNewMVar = Strict.newMVar

uncheckedNewEmptyMVar :: MonadSTM m => a -> m (StrictMVar m a)
uncheckedNewEmptyMVar = Strict.newEmptyMVar
