{-# LANGUAGE FlexibleContexts #-}

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
  , readTVar
  , uncheckedNewTVarM
  , updateTVar
  , writeTVar
    -- *** StrictTMVar
  , StrictTMVar
  , putTMVar
  , readTMVar
  , takeTMVar
  , tryTakeTMVar
  , swapTMVar
  , uncheckedNewEmptyTMVarM
  , uncheckedNewTMVarM
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
      ) => IOLike m where

instance IOLike IO
