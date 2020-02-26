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
    -- *** MonadFork
  , MonadFork(..) -- TODO: Should we hide this in favour of MonadAsync?
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

class ( MonadAsync  m
      , MonadFork   m
      , MonadST     m
      , MonadTime   m
      , MonadDelay  m
      , MonadThread m
      , MonadThrow  m
      , MonadCatch  m
      , MonadMask   m
      , MonadThrow (STM m)
      , forall a. NoUnexpectedThunks (m a)
      , forall a. NoUnexpectedThunks a => NoUnexpectedThunks (StrictTVar m a)
      , forall a. NoUnexpectedThunks a => NoUnexpectedThunks (StrictMVar m a)
      ) => IOLike m where

instance IOLike IO
