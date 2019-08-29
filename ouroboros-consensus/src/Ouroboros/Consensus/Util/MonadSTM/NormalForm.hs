module Ouroboros.Consensus.Util.MonadSTM.NormalForm (
    module Control.Monad.Class.MonadSTM.Strict
  , newTVarM
  , newTMVarM
  , newEmptyTMVarM
    -- * Deprecated
  , uncheckedNewTVarM
  , uncheckedNewEmptyTMVarM
  , uncheckedNewTMVarM
  , uncheckedNewTVar
    -- ** Convenience re-exports
    -- (Modules that use this module will most likely also need these)
  , Generic
  , CardanoPrelude.NoUnexpectedThunks(..)
  , CardanoPrelude.DontCheckForThunks(..)
  , CardanoPrelude.UseIsNormalForm(..)
  , CardanoPrelude.allNoUnexpectedThunks
  , CardanoPrelude.noUnexpectedThunksUsingNormalForm
  ) where

import           Control.Monad.Class.MonadSTM.Strict hiding (newEmptyTMVar,
                     newEmptyTMVarM, newEmptyTMVarWithInvariantM, newTMVar,
                     newTMVarM, newTMVarWithInvariantM, newTVar, newTVarM,
                     newTVarWithInvariantM)
import qualified Control.Monad.Class.MonadSTM.Strict as Strict
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks)
import qualified Cardano.Prelude as CardanoPrelude

import           Ouroboros.Consensus.Util (unsafeNoThunks)

newTVarM :: (MonadSTM m, NoUnexpectedThunks a) => a -> m (StrictTVar m a)
newTVarM = Strict.newTVarWithInvariantM unsafeNoThunks

newEmptyTMVarM :: (MonadSTM m, NoUnexpectedThunks a) => m (StrictTMVar m a)
newEmptyTMVarM = Strict.newEmptyTMVarWithInvariantM unsafeNoThunks

newTMVarM :: (MonadSTM m, NoUnexpectedThunks a) => a -> m (StrictTMVar m a)
newTMVarM = Strict.newTMVarWithInvariantM unsafeNoThunks

{-------------------------------------------------------------------------------
  Deprecated
-------------------------------------------------------------------------------}

{-# DEPRECATED uncheckedNewTVarM "Use newTVarM" #-}
uncheckedNewTVarM :: MonadSTM m => a -> m (StrictTVar m a)
uncheckedNewTVarM = Strict.newTVarWithInvariantM (const Nothing)

{-# DEPRECATED uncheckedNewEmptyTMVarM "Use newEmptyTMVarM" #-}
uncheckedNewEmptyTMVarM :: MonadSTM m => m (StrictTMVar m a)
uncheckedNewEmptyTMVarM = Strict.newEmptyTMVarWithInvariantM (const Nothing)

{-# DEPRECATED uncheckedNewTMVarM "Use newTMvarM" #-}
uncheckedNewTMVarM :: MonadSTM m => a -> m (StrictTMVar m a)
uncheckedNewTMVarM = Strict.newTMVarWithInvariantM (const Nothing)

{-# DEPRECATED uncheckedNewTVar "Use newTVarM" #-}
uncheckedNewTVar :: MonadSTM m => a -> STM m (StrictTVar m a)
uncheckedNewTVar = Strict.newTVar
