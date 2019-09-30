module Ouroboros.Consensus.Util.MonadSTM.NormalForm (
    module Control.Monad.Class.MonadSTM.Strict
  , newTVarM
  , newTMVarM
  , newEmptyTMVarM
    -- * Temporary
  , uncheckedNewTVarM
  , uncheckedNewTMVarM
  , uncheckedNewEmptyTMVarM
  ) where

import           GHC.Stack
import           System.IO.Unsafe (unsafePerformIO)

import           Cardano.Prelude (NoUnexpectedThunks (..), ThunkInfo (..))

import           Control.Monad.Class.MonadSTM.Strict hiding (newEmptyTMVarM,
                     newEmptyTMVarWithInvariantM, newTMVar, newTMVarM,
                     newTMVarWithInvariantM, newTVar, newTVarM,
                     newTVarWithInvariantM)
import qualified Control.Monad.Class.MonadSTM.Strict as Strict

{-------------------------------------------------------------------------------
  Wrappers that check for thunks
-------------------------------------------------------------------------------}

newTVarM :: (MonadSTM m, HasCallStack, NoUnexpectedThunks a)
         => a -> m (StrictTVar m a)
newTVarM = Strict.newTVarWithInvariantM unsafeNoThunks

newTMVarM :: (MonadSTM m, HasCallStack, NoUnexpectedThunks a)
          => a -> m (StrictTMVar m a)
newTMVarM = Strict.newTMVarWithInvariantM unsafeNoThunks

newEmptyTMVarM :: (MonadSTM m, NoUnexpectedThunks a)
               => m (StrictTMVar m a)
newEmptyTMVarM = Strict.newEmptyTMVarWithInvariantM unsafeNoThunks

{-------------------------------------------------------------------------------
  Auxiliary: check for thunks
-------------------------------------------------------------------------------}

unsafeNoThunks :: NoUnexpectedThunks a => a -> Maybe String
unsafeNoThunks a = unsafePerformIO $ errorMessage <$> noUnexpectedThunks [] a
  where
    errorMessage :: ThunkInfo -> Maybe String
    errorMessage NoUnexpectedThunks     = Nothing
    errorMessage (UnexpectedThunk info) = Just $ show info

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
