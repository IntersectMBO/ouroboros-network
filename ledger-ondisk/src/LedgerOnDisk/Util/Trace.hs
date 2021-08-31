-- |

module LedgerOnDisk.Util.Trace where

import Control.Tracer
import Control.Monad.IO.Class
import Control.Monad.Writer
import Control.Monad.STM
import Data.Foldable

atomicallyTrace :: MonadIO m => Tracer m x -> WriterT [x] STM a -> m a
atomicallyTrace tracer m = do
  (a, w) <- liftIO . atomically . runWriterT $ m
  for_ w $ traceWith tracer
  pure a
