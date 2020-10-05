module Control.Monad.Class.MonadEventlog (
    MonadEventlog(..),
    -- * Deprecated API
    traceEventM,
    traceMarkerM
  ) where

import           Control.Monad.Reader
import qualified Debug.Trace as IO (traceEventIO, traceMarkerIO)

class Monad m => MonadEventlog m where

  -- | Emits a message to the eventlog, if eventlog profiling is available and
  -- enabled at runtime.
  traceEventIO :: String -> m ()

  -- | Emits a marker to the eventlog, if eventlog profiling is available and
  -- enabled at runtime.
  --
  -- The 'String' is the name of the marker. The name is just used in the
  -- profiling tools to help you keep clear which marker is which.
  traceMarkerIO :: String -> m ()


traceEventM :: MonadEventlog m => String -> m ()
traceEventM = traceEventIO
{-# DEPRECATED traceEventM "Use traceEventIO" #-}


traceMarkerM :: MonadEventlog m => String -> m ()
traceMarkerM = traceMarkerIO
{-# DEPRECATED traceMarkerM "Use traceEventIO" #-}


--
-- Instances for IO
--

instance MonadEventlog IO where
  traceEventIO = IO.traceEventIO
  traceMarkerIO = IO.traceMarkerIO

--
-- Instance for ReaderT
--

instance MonadEventlog m => MonadEventlog (ReaderT r m) where
  traceEventIO = lift . traceEventIO
  traceMarkerIO = lift . traceMarkerIO
