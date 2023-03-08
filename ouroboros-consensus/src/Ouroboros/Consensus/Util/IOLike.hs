{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Ouroboros.Consensus.Util.IOLike (
    IOLike (..)
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

import           Cardano.Crypto.KES (KESAlgorithm, SignKeyKES)
import qualified Cardano.Crypto.KES as KES

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadEventlog
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadMVar
import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime hiding (MonadTime (..))
import           Control.Monad.Class.MonadTimer

import           Ouroboros.Consensus.Util.MonadSTM.NormalForm
import           Ouroboros.Consensus.Util.Orphans ()

import           Data.Functor (void)

{-------------------------------------------------------------------------------
  IOLike
-------------------------------------------------------------------------------}

class ( MonadAsync              m
      , MonadMVar               m
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
  -- | Securely forget a KES signing key.
  --
  -- No-op for the IOSim, but 'KES.forgetSignKeyKES' for IO.
  forgetSignKeyKES :: KESAlgorithm v => SignKeyKES v -> m ()

instance IOLike IO where
  forgetSignKeyKES = KES.forgetSignKeyKES

-- | Generalization of 'link' that links an async to an arbitrary thread.
--
-- Non standard (not in 'async' library)
--
linkTo :: (MonadAsync m, MonadFork m, MonadMask m)
       => ThreadId m -> Async m a -> m ()
linkTo tid = linkToOnly tid (not . isCancel)

-- | Generalization of 'linkOnly' that links an async to an arbitrary thread.
--
-- Non standard (not in 'async' library).
--
linkToOnly :: forall m a. (MonadAsync m, MonadFork m, MonadMask m)
           => ThreadId m -> (SomeException -> Bool) -> Async m a -> m ()
linkToOnly tid shouldThrow a = do
    void $ forkRepeat ("linkToOnly " <> show linkedThreadId) $ do
      r <- waitCatch a
      case r of
        Left e | shouldThrow e -> throwTo tid (exceptionInLinkedThread e)
        _otherwise             -> return ()
  where
    linkedThreadId :: ThreadId m
    linkedThreadId = asyncThreadId a

    exceptionInLinkedThread :: SomeException -> ExceptionInLinkedThread
    exceptionInLinkedThread =
        ExceptionInLinkedThread (show linkedThreadId)

isCancel :: SomeException -> Bool
isCancel e
  | Just AsyncCancelled <- fromException e = True
  | otherwise = False

forkRepeat :: (MonadFork m, MonadMask m) => String -> m a -> m (ThreadId m)
forkRepeat label action =
  mask $ \restore ->
    let go = do r <- tryAll (restore action)
                case r of
                  Left _ -> go
                  _      -> return ()
    in forkIO (labelThisThread label >> go)

tryAll :: MonadCatch m => m a -> m (Either SomeException a)
tryAll = try
