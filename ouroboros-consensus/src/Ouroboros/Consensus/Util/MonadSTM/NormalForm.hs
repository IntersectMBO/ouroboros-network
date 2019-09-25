module Ouroboros.Consensus.Util.MonadSTM.NormalForm (
    module Control.Monad.Class.MonadSTM.Strict
  , newTVarM
  , newTMVarM
  , newEmptyTMVarM
    -- Temporary
  , uncheckedNewTVar
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
  Wrap the variable constructors
-------------------------------------------------------------------------------}

newTVarM :: MonadSTM m => a -> m (StrictTVar m a)
newTVarM = Strict.newTVarM

newTMVarM :: MonadSTM m => a -> m (StrictTMVar m a)
newTMVarM = Strict.newTMVarM

newEmptyTMVarM :: MonadSTM m => m (StrictTMVar m a)
newEmptyTMVarM = Strict.newEmptyTMVarM

{-------------------------------------------------------------------------------
  Deprecated
-------------------------------------------------------------------------------}

uncheckedNewTVar :: MonadSTM m => a -> STM m (StrictTVar m a)
uncheckedNewTVar = Strict.newTVar

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
