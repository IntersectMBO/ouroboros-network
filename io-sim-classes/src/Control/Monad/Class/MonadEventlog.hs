module Control.Monad.Class.MonadEventlog (
    MonadEventlog(..)
  ) where

import           Control.Monad.Reader
import           Debug.Trace (traceEventIO, traceMarkerIO)

class Monad m => MonadEventlog m where

  -- | Emits a message to the eventlog, if eventlog profiling is available and
  -- enabled at runtime.
  traceEventM :: String -> m ()

  -- | Emits a marker to the eventlog, if eventlog profiling is available and
  -- enabled at runtime.
  --
  -- The 'String' is the name of the marker. The name is just used in the
  -- profiling tools to help you keep clear which marker is which.
  traceMarkerM :: String -> m ()


--
-- Instances for IO
--

instance MonadEventlog IO where
  traceEventM = traceEventIO
  traceMarkerM = traceMarkerIO

--
-- Instance for ReaderT
--

instance MonadEventlog m => MonadEventlog (ReaderT r m) where
  traceEventM = lift . traceEventM
  traceMarkerM = lift . traceMarkerM
