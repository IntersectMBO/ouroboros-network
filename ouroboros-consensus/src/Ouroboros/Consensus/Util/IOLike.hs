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
  , MonadTime(..)
  , DiffTime
  , Time(..)
  , UTCTime
  , addTime
  , diffTime
    -- *** MonadDelay
  , MonadDelay(..)
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
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer

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

class ( MonadAsync    m
      , MonadEventlog m
      , MonadFork     m
      , MonadST       m
      , MonadTime     m
      , MonadDelay    m
      , MonadThread   m
      , MonadThrow    m
      , MonadCatch    m
      , MonadMask     m
      , MonadThrow (STM m)
      , MonadSTMTxExtended (STM m)
      , forall a. NoUnexpectedThunks (m a)
      , forall a. NoUnexpectedThunks a => NoUnexpectedThunks (StrictTVar m a)
      , forall a. NoUnexpectedThunks a => NoUnexpectedThunks (StrictMVar m a)
      ) => IOLike m where

instance IOLike IO
