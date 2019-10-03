{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Ouroboros.Consensus.Util.IOLike (
    IOLike
    -- * Re-exports
    -- *** MonadThrow
  , uninterruptibleCancel
    -- *** MonadSTM
  , STM
  , atomically
  , check
  , retry
    -- ** Queues (TODO: Should we have a strict queue?)
  , TQueue
  , newTQueue
  , readTQueue
  , writeTQueue
    -- ** StrictTVar
  , StrictTVar
  , modifyTVar
  , newTVarM
  , readTVar
  , uncheckedNewTVarM
  , updateTVar
  , writeTVar
    -- ** StrictMVar
  , StrictMVar
  , newEmptyMVar
  , newMVar
  , putMVar
  , readMVar
  , readMVarSTM
  , swapMVar
  , takeMVar
  , tryTakeMVar
  , uncheckedNewEmptyMVar
  , uncheckedNewMVar
  , updateMVar
    -- *** MonadFork
  , ThreadId
  , myThreadId
  , fork -- TODO: Should we hide this?
    -- *** MonadAsync
  , Async
  , async
  , asyncThreadId
  , linkTo
  , wait
  , waitAny
  , waitBoth
  , withAsync
    -- *** MonadST
  , withLiftST
    -- *** MonadTime
  , Time
  , DiffTime
  , addTime
  , diffTime
  , getMonotonicTime
    -- *** MonadTimer
  , threadDelay
  , timeoutAfter
    -- *** Cardano prelude
  , NoUnexpectedThunks(..)
  ) where

import           Cardano.Prelude (NoUnexpectedThunks (..))

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer

import           Ouroboros.Consensus.Util.MonadSTM.NormalForm
import           Ouroboros.Consensus.Util.Orphans ()

{-------------------------------------------------------------------------------
  IOLike
-------------------------------------------------------------------------------}

class ( MonadAsync m
      , MonadFork  m
      , MonadST    m
      , MonadTime  m
      , MonadTimer m
      , MonadThrow m
      , MonadCatch m
      , MonadMask  m
      , MonadThrow (STM m)
      , forall a. NoUnexpectedThunks (m a)
      , forall a. NoUnexpectedThunks a => NoUnexpectedThunks (StrictTVar m a)
      , forall a. NoUnexpectedThunks a => NoUnexpectedThunks (StrictMVar m a)
      ) => IOLike m where

instance IOLike IO
