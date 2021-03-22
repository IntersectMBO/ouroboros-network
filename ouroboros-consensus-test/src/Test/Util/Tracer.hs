module Test.Util.Tracer (
    recordingTracerIORef
  , recordingTracerTVar
  ) where

import           Data.IORef

import           Control.Tracer

import           Ouroboros.Consensus.Util.MonadSTM.NormalForm

-- | Create a 'Tracer' that stores all events in an 'IORef' that is atomically
-- updated. The second return value lets you obtain the events recorded so far
-- (from oldest to newest). Obtaining the events does not erase them.
recordingTracerIORef :: IO (Tracer IO ev, IO [ev])
recordingTracerIORef = newIORef [] >>= \ref -> return
    ( Tracer $ \ev -> atomicModifyIORef' ref $ \evs -> (ev:evs, ())
    , reverse <$> readIORef ref
    )

-- | Create a 'Tracer' that stores all events in a 'TVar' that is atomically
-- updated. The second return value lets you obtain the events recorded so far
-- (from oldest to newest). Obtaining the events does not erase them.
recordingTracerTVar :: MonadSTM m => m (Tracer m ev, m [ev])
recordingTracerTVar = uncheckedNewTVarM [] >>= \ref -> return
    ( Tracer $ \ev -> atomically $ modifyTVar ref (ev:)
    , atomically $ reverse <$> readTVar ref
    )
