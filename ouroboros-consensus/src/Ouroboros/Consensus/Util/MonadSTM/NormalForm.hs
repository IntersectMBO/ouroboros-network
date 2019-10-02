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
    -- * Low-level API
  , unsafeNoThunks
  ) where

import qualified Data.Text as Text
import           GHC.Stack
import           System.IO.Unsafe (unsafePerformIO)

import           Cardano.Prelude (ClosureTreeOptions (..),
                     NoUnexpectedThunks (..), ThunkInfo (..),
                     TraverseCyclicClosures (..), TreeDepth (..),
                     buildAndRenderClosureTree)

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
newTVarM = Strict.newTVarWithInvariantM unsafeNoThunks

newMVar :: (MonadSTM m, HasCallStack, NoUnexpectedThunks a)
        => a -> m (StrictMVar m a)
newMVar = Strict.newMVarWithInvariant unsafeNoThunks

newEmptyMVar :: (MonadSTM m, NoUnexpectedThunks a) => a -> m (StrictMVar m a)
newEmptyMVar = Strict.newEmptyMVarWithInvariant unsafeNoThunks

{-------------------------------------------------------------------------------
  Auxiliary: check for thunks
-------------------------------------------------------------------------------}

unsafeNoThunks :: NoUnexpectedThunks a => a -> Maybe String
unsafeNoThunks a = unsafePerformIO $ errorMessage =<< noUnexpectedThunks [] a
  where
    errorMessage :: ThunkInfo -> IO (Maybe String)
    errorMessage NoUnexpectedThunks     = return Nothing
    errorMessage (UnexpectedThunk info) = do
        -- We render the tree /after/ checking; in a way, this is not correct,
        -- because 'noUnexpectedThunks' might have forced some stuff. However,
        -- computing the tree beforehand, even when there is no failure, would
        -- be prohibitively expensive.
        tree <- buildAndRenderClosureTree opts a
        return $ Just $ show info ++ "\nTree:\n" ++ Text.unpack tree

    opts :: ClosureTreeOptions
    opts = ClosureTreeOptions {
        ctoMaxDepth       = AnyDepth
      , ctoCyclicClosures = NoTraverseCyclicClosures
      }

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
