{-# LANGUAGE DeriveGeneric #-}

module Ouroboros.Consensus.Util.IOLike.MonadTimeOnlyForTracer (
    MonadTimeOnlyForTracer (..)
  , module X
  , defaultgetCurrentTimeOnlyForTracer
  ) where

import           Control.Monad.Reader

import           Control.Monad.Class.MonadTime hiding (MonadTime (..))
import qualified Control.Monad.Class.MonadTime as X

import           Ouroboros.Network.Tracers.OnlyForTracer (OnlyForTracer)

{-------------------------------------------------------------------------------
  MonadTimeOnlyForTracer
-------------------------------------------------------------------------------}

-- | A variation of 'MonadTime.MonadTime' from "Control.Monad.Class.MonadTime"
-- that is immediately wrapped in 'OnlyForTracer'
class MonadMonotonicTime m => MonadTimeOnlyForTracer m where
  -- | Wall clock time.
  --
  getCurrentTimeOnlyForTracer :: m (OnlyForTracer UTCTime)

-- | Defer to the actual 'X.MonadTime' from "Control.Monad.Class.MonadTime"
defaultgetCurrentTimeOnlyForTracer :: X.MonadTime m => m (OnlyForTracer UTCTime)
defaultgetCurrentTimeOnlyForTracer = pure <$> X.getCurrentTime

instance MonadTimeOnlyForTracer IO where
  getCurrentTimeOnlyForTracer = defaultgetCurrentTimeOnlyForTracer

instance MonadTimeOnlyForTracer m => MonadTimeOnlyForTracer (ReaderT r m) where
  getCurrentTimeOnlyForTracer = lift getCurrentTimeOnlyForTracer
