{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Ouroboros.Consensus.Util.IOLike (
    IOLike
    -- * Re-exports
    -- *** MonadThrow
  , MonadThrow(..)
  , MonadCatch(..)
  , MonadMask(..)
  , Exception(..)
  , SomeException
  , ExitCase(..)
    -- *** MonadSTM
  , module Ouroboros.Consensus.Util.MonadSTM.NormalForm
  , MonadSTMTxExtended(..)
    -- *** MonadFork
  , MonadFork(..) -- TODO: Should we hide this in favour of MonadAsync?
  , labelThisThread
  , MonadThread(..)
    -- *** MonadAsync
  , MonadAsyncSTM(..)
  , MonadAsync(..)
  , ExceptionInLinkedThread(..)
  , link
  , linkTo
    -- *** MonadST
  , MonadST(..)
    -- *** MonadTime
  , MonadMonotonicTime(..)
  , Time(..)
  , DiffTime
  , addTime
  , diffTime
    -- *** MonadDelay
  , MonadDelay -- Opaque to favor the usage of 'threadDelay'
    -- *** MonadDelay Wrappers
  , threadDelay
    -- *** MonadEventlog
  , MonadEventlog(..)
    -- *** Cardano prelude
  , NoUnexpectedThunks(..)
  ) where

import qualified Control.Concurrent.STM as IO

import           Cardano.Prelude (Natural, NoUnexpectedThunks (..))

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadEventlog
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime hiding (MonadTime (..))
import           Control.Monad.Class.MonadTimer hiding (threadDelay)
import qualified Control.Monad.Class.MonadTimer as MonadTimer

import           Ouroboros.Consensus.Util.MonadSTM.NormalForm
import           Ouroboros.Consensus.Util.Orphans ()

{-------------------------------------------------------------------------------
  MonadSTMTxExtended
-------------------------------------------------------------------------------}

-- | Additional STM functionality
class MonadSTMTx stm => MonadSTMTxExtended stm where
  -- 'lengthTBQueue' has only been added in stm-2.5.0.0, while io-sim-classes
  -- supports older versions too. In consensus, we require stm >= 2.5, giving
  -- us the real stm implementation.
  lengthTBQueue  :: TBQueue_ stm a -> stm Natural

instance MonadSTMTxExtended IO.STM where
  lengthTBQueue = IO.lengthTBQueue

{-------------------------------------------------------------------------------
  IOLike
-------------------------------------------------------------------------------}

class ( MonadAsync              m
      , MonadEventlog           m
      , MonadFork               m
      , MonadST                 m
      , MonadDelay              m
      , MonadThread             m
      , MonadThrow              m
      , MonadCatch              m
      , MonadMask               m
      , MonadMonotonicTime      m
      , MonadThrow         (STM m)
      , MonadSTMTxExtended (STM m)
      , forall a. NoUnexpectedThunks (m a)
      , forall a. NoUnexpectedThunks a => NoUnexpectedThunks (StrictTVar m a)
      , forall a. NoUnexpectedThunks a => NoUnexpectedThunks (StrictMVar m a)
      ) => IOLike m where

instance IOLike IO

{-------------------------------------------------------------------------------
  MonadDelay Wrappers
-------------------------------------------------------------------------------}

-- | In some cases, 'MonadTimer.threadDelay' can't be used for delays over
-- ~35mins, because it uses an 'Int', which can overflow (see
-- 'diffTimeToMicrosecondsAsInt' for more info). This can be safely used to
-- sleep for longer periods of time, so we favor its usage in all cases.
threadDelay :: MonadDelay m => DiffTime -> m ()
threadDelay time
    | time > maxDelay
    = MonadTimer.threadDelay maxDelay *> threadDelay (time - maxDelay)
    | otherwise
    = MonadTimer.threadDelay time
  where
    maxDelay :: DiffTime
    maxDelay = fromIntegral (maxBound :: Int)
