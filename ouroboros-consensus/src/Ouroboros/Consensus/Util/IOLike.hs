{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Ouroboros.Consensus.Util.IOLike (
    IOLike

    -- * Re-exports
    -- *** MonadThrow
  , Exception (..)
  , ExitCase (..)
  , MonadCatch (..)
  , MonadMask (..)
  , MonadThrow (..)
  , SomeException
    -- *** MonadSTM
  , module Ouroboros.Consensus.Util.MonadSTM.NormalForm
    -- *** MonadFork, TODO: Should we hide this in favour of MonadAsync?
  , MonadFork (..)
  , MonadThread (..)
  , labelThisThread
    -- *** MonadAsync
  , ExceptionInLinkedThread (..)
  , MonadAsync (..)
  , MonadAsyncSTM (..)
  , link
  , linkTo
    -- *** MonadST
  , MonadST (..)
    -- *** MonadTime
  , DiffTime
  , MonadMonotonicTime (..)
  , Time (..)
  , addTime
  , diffTime
    -- *** MonadDelay
  , MonadDelay (..)
    -- *** MonadEventlog
  , MonadEventlog (..)
    -- *** MonadEvaluate
  , MonadEvaluate (..)
    -- *** NoThunks
  , NoThunks (..)
  ) where

import           NoThunks.Class (NoThunks (..))

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadEventlog
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime hiding (MonadTime (..))
import           Control.Monad.Class.MonadTimer

import           Ouroboros.Consensus.Util.MonadSTM.NormalForm
import           Ouroboros.Consensus.Util.Orphans ()

{-------------------------------------------------------------------------------
  IOLike
-------------------------------------------------------------------------------}

class ( Monad                   m
      , MonadAsync              m
      , MonadEventlog           m
      , MonadFork               m
      , MonadST                 m
      , MonadDelay              m
      , MonadThread             m
      , MonadThrow              m
      , MonadCatch              m
      , MonadMask               m
      , MonadMonotonicTime      m
      , MonadEvaluate           m
      , MonadThrow         (STM m)
      , forall a. NoThunks (m a)
      , forall a. NoThunks a => NoThunks (StrictTVar m a)
      , forall a. NoThunks a => NoThunks (StrictMVar m a)
      ) => IOLike m where

instance IOLike IO where
