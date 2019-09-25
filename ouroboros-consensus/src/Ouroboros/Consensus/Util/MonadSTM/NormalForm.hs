module Ouroboros.Consensus.Util.MonadSTM.NormalForm (
    module Control.Monad.Class.MonadSTM.Strict
  , uncheckedNewTVarM
  , uncheckedNewTMVarM
  , uncheckedNewEmptyTMVarM
    -- Temporary
  , newTVarWithInvariantM
  , newTMVarWithInvariantM
  ) where

import           GHC.Stack

import           Control.Monad.Class.MonadSTM.Strict hiding (newEmptyTMVar,
                     newEmptyTMVarM, newEmptyTMVarWithInvariantM, newTMVar,
                     newTMVarM, newTMVarWithInvariantM, newTVar, newTVarM,
                     newTVarWithInvariantM)
import qualified Control.Monad.Class.MonadSTM.Strict as Strict

{-------------------------------------------------------------------------------
  Unchecked wrappers (where we don't check for thunks)

  These will eventually be removed.
-------------------------------------------------------------------------------}

uncheckedNewTVarM :: MonadSTM m => a -> m (StrictTVar m a)
uncheckedNewTVarM = Strict.newTVarM

uncheckedNewTMVarM :: MonadSTM m => a -> m (StrictTMVar m a)
uncheckedNewTMVarM = Strict.newTMVarM

uncheckedNewEmptyTMVarM :: MonadSTM m => m (StrictTMVar m a)
uncheckedNewEmptyTMVarM = Strict.newEmptyTMVarM

{-------------------------------------------------------------------------------
  Deprecated
-------------------------------------------------------------------------------}

newTVarWithInvariantM :: (MonadSTM m, HasCallStack)
                      => (a -> Maybe String) -- ^ Invariant (expect 'Nothing')
                      -> a
                      -> m (StrictTVar m a)
newTVarWithInvariantM = Strict.newTVarWithInvariantM

newTMVarWithInvariantM :: (MonadSTM m, HasCallStack)
                       => (a -> Maybe String)  -- ^ Invariant (expect 'Nothing')
                       -> a
                       -> m (StrictTMVar m a)
newTMVarWithInvariantM = Strict.newTMVarWithInvariantM
