{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Ouroboros.Consensus.Util.EarlyExit (
    WithEarlyExit -- opaque
  , withEarlyExit
  , withEarlyExit_
  , exitEarly
    -- * Re-exports
  , lift
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.ST (ST)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Data.Function (on)
import           Data.Proxy

import           Cardano.Prelude (NoUnexpectedThunks(..))

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer

import           Ouroboros.Consensus.Util ((.:))
import           Ouroboros.Consensus.Util.IOLike (IOLike, StrictTVar, StrictMVar)

{-------------------------------------------------------------------------------
  Basic definitions
-------------------------------------------------------------------------------}

newtype WithEarlyExit m a = WithEarlyExit {
      unWithEarlyExit :: MaybeT m a
    }
  deriving ( Functor
           , Applicative
           , Alternative
           , Monad
           , MonadTrans
           , MonadPlus
           )

-- | Internal only
earlyExit :: m (Maybe a) -> WithEarlyExit m a
earlyExit = WithEarlyExit . MaybeT

withEarlyExit :: WithEarlyExit m a -> m (Maybe a)
withEarlyExit = runMaybeT . unWithEarlyExit

withEarlyExit_ :: Functor m => WithEarlyExit m () -> m ()
withEarlyExit_ = fmap collapse . withEarlyExit

collapse :: Maybe () -> ()
collapse Nothing   = ()
collapse (Just ()) = ()

exitEarly :: Applicative m => WithEarlyExit m a
exitEarly = earlyExit $ pure Nothing

instance (forall a'. NoUnexpectedThunks (m a'))
      => NoUnexpectedThunks (WithEarlyExit m a) where
   whnfNoUnexpectedThunks ctxt = whnfNoUnexpectedThunks ctxt . withEarlyExit
   showTypeOf _p = "WithEarlyExit " ++ showTypeOf (Proxy @(m a))

{-------------------------------------------------------------------------------
  Special wrapper for STM

  This is required because MonadSTM requires STM to be injective.
-------------------------------------------------------------------------------}

newtype WrapSTM m a = Wrap { unwrap :: WithEarlyExit (STM m) a }

unwrapSTM :: WrapSTM m a -> STM m (Maybe a)
unwrapSTM = withEarlyExit . unwrap

wrapSTM :: STM m (Maybe a) -> WrapSTM m a
wrapSTM = Wrap . earlyExit

wrapSTM' :: MonadSTM m => STM m a -> WrapSTM m a
wrapSTM' = wrapSTM . fmap Just

deriving instance MonadSTM m => Functor     (WrapSTM m)
deriving instance MonadSTM m => Applicative (WrapSTM m)
deriving instance MonadSTM m => Monad       (WrapSTM m)
deriving instance MonadSTM m => Alternative (WrapSTM m)
deriving instance MonadSTM m => MonadPlus   (WrapSTM m)

-- These two piggy-back on the instances for WithEarlyExit, below
deriving instance (MonadSTM m, MonadCatch (STM m)) => MonadThrow (WrapSTM m)
deriving instance (MonadSTM m, MonadCatch (STM m)) => MonadCatch (WrapSTM m)

{-------------------------------------------------------------------------------
  Instances for io-classes
-------------------------------------------------------------------------------}

instance MonadSTM m => MonadSTM (WithEarlyExit m) where
  type STM     (WithEarlyExit m) = WrapSTM m -- == WithEarlyExit (STM m)
  type TVar    (WithEarlyExit m) = TVar    m
  type TMVar   (WithEarlyExit m) = TMVar   m
  type TQueue  (WithEarlyExit m) = TQueue  m
  type TBQueue (WithEarlyExit m) = TBQueue m

  atomically      = earlyExit . atomically . unwrapSTM

  newTVar         = wrapSTM' .  newTVar
  readTVar        = wrapSTM' .  readTVar
  writeTVar       = wrapSTM' .: writeTVar
  retry           = wrapSTM'    retry
  orElse          = (wrapSTM .: orElse) `on` unwrapSTM
  newTMVar        = wrapSTM' .  newTMVar
  newTMVarM       = lift     .  newTMVarM
  newEmptyTMVar   = wrapSTM'    newEmptyTMVar
  newEmptyTMVarM  = lift        newEmptyTMVarM
  takeTMVar       = wrapSTM' .  takeTMVar
  tryTakeTMVar    = wrapSTM' .  tryTakeTMVar
  putTMVar        = wrapSTM' .: putTMVar
  tryPutTMVar     = wrapSTM' .: tryPutTMVar
  readTMVar       = wrapSTM' .  readTMVar
  tryReadTMVar    = wrapSTM' .  tryReadTMVar
  swapTMVar       = wrapSTM' .: swapTMVar
  isEmptyTMVar    = wrapSTM' .  isEmptyTMVar
  newTQueue       = wrapSTM'    newTQueue
  readTQueue      = wrapSTM' .  readTQueue
  tryReadTQueue   = wrapSTM' .  tryReadTQueue
  writeTQueue     = wrapSTM' .: writeTQueue
  isEmptyTQueue   = wrapSTM' .  isEmptyTQueue
  newTBQueue      = wrapSTM' .  newTBQueue
  readTBQueue     = wrapSTM' .  readTBQueue
  tryReadTBQueue  = wrapSTM' .  tryReadTBQueue
  writeTBQueue    = wrapSTM' .: writeTBQueue
  isEmptyTBQueue  = wrapSTM' .  isEmptyTBQueue
  isFullTBQueue   = wrapSTM' .  isFullTBQueue

instance MonadCatch m => MonadThrow (WithEarlyExit m) where
  throwM = lift . throwM

instance MonadCatch m => MonadCatch (WithEarlyExit m) where
  catch act handler = earlyExit $
      catch (withEarlyExit act) (withEarlyExit . handler)

  generalBracket acquire release use = earlyExit $ do
      -- This is modelled on the case for ErrorT, except that we don't have
      -- to worry about reporting the right error, since we only have @Nothing@
      (mb, mc) <- generalBracket
                    (withEarlyExit acquire)
                    (\mResource exitCase ->
                        case (mResource, exitCase) of
                          (Nothing, _) ->
                            -- resource not acquired
                            return Nothing
                          (Just resource, ExitCaseSuccess (Just b)) ->
                            withEarlyExit $ release resource (ExitCaseSuccess b)
                          (Just resource, ExitCaseException e) ->
                            withEarlyExit $ release resource (ExitCaseException e)
                          (Just resource, _otherwise) ->
                            withEarlyExit $ release resource ExitCaseAbort
                    )
                    (maybe (return Nothing) (withEarlyExit . use))
      return $ (,) <$> mb <*> mc

instance MonadMask m => MonadMask (WithEarlyExit m) where
  mask f = earlyExit $
    mask $ \unmask ->
      withEarlyExit (f (earlyExit . unmask . withEarlyExit))

  uninterruptibleMask f = earlyExit $
    uninterruptibleMask $ \unmask ->
      let unmask' :: forall a. WithEarlyExit m a -> WithEarlyExit m a
          unmask' = earlyExit . unmask . withEarlyExit
      in withEarlyExit (f unmask')

instance MonadThread m => MonadThread (WithEarlyExit m) where
  type ThreadId (WithEarlyExit m) = ThreadId m

  myThreadId  = lift    myThreadId
  labelThread = lift .: labelThread

instance ( MonadMask  m
         , MonadAsync m
         , MonadCatch (STM m)
         ) => MonadAsync (WithEarlyExit m) where
  type Async (WithEarlyExit m) = WithEarlyExit (Async m)

  async            = lift . (fmap earlyExit . async) . withEarlyExit
  asyncThreadId _p = asyncThreadId (Proxy @(WithEarlyExit m))
  cancel        a  = lift $ cancel     (withEarlyExit a)
  cancelWith    a  = lift . cancelWith (withEarlyExit a)
  waitCatchSTM  a  = wrapSTM (commute      <$> waitCatchSTM (withEarlyExit a))
  pollSTM       a  = wrapSTM (fmap commute <$> pollSTM      (withEarlyExit a))

commute :: Either SomeException (Maybe a) -> Maybe (Either SomeException a)
commute (Left e)         = Just (Left e)
commute (Right Nothing)  = Nothing
commute (Right (Just a)) = Just (Right a)

instance MonadFork m => MonadFork (WithEarlyExit m) where
  fork           f = lift $ fork (collapse <$> withEarlyExit f)
  forkWithUnmask f = lift $ forkWithUnmask $ \unmask ->
                       let unmask' :: forall a. WithEarlyExit m a -> WithEarlyExit m a
                           unmask' = earlyExit . unmask . withEarlyExit
                       in collapse <$> withEarlyExit (f unmask')
  throwTo          = lift .: throwTo

instance MonadST m => MonadST (WithEarlyExit m) where
  withLiftST f = lowerLiftST $ \(_proxy :: Proxy s) liftST ->
     let liftST' :: forall a. ST s a -> WithEarlyExit m a
         liftST' = lift . liftST
     in f liftST'
    where
      lowerLiftST :: (forall s. Proxy s -> (forall a. ST s a -> m a) -> b) -> b
      lowerLiftST g = withLiftST $ g Proxy

instance MonadTime m => MonadTime (WithEarlyExit m) where
  getMonotonicTime = lift getMonotonicTime
  getCurrentTime   = lift getCurrentTime

instance (MonadTimer m, MonadFork m) => MonadTimer (WithEarlyExit m) where
  newtype Timeout (WithEarlyExit m) = WrapTimeout { unwrapTimeout :: Timeout m }

  newTimeout    d = lift     $ WrapTimeout <$> newTimeout d
  readTimeout   t = wrapSTM' $ readTimeout   (unwrapTimeout t)
  updateTimeout t = lift     . updateTimeout (unwrapTimeout t)
  cancelTimeout t = lift     $ cancelTimeout (unwrapTimeout t)
  timeout       d = earlyExit . timeout d . withEarlyExit

{-------------------------------------------------------------------------------
  Finally, the consensus IOLike wrapper
-------------------------------------------------------------------------------}

instance ( IOLike m
         , forall a. NoUnexpectedThunks (StrictTVar (WithEarlyExit m) a)
         , forall a. NoUnexpectedThunks (StrictMVar (WithEarlyExit m) a)
           -- The simulator does not currently support @MonadCatch (STM m)@,
           -- making this @IOLike@ instance applicable to @IO@ only. Once that
           -- missing @MonadCatch@ instance is added, @IOLike@ should require
           -- @MonadCatch (STM m)@ intsead of @MonadThrow (STM m)@.
           -- <https://github.com/input-output-hk/ouroboros-network/issues/1461>
         , MonadCatch (STM m)
         ) => IOLike (WithEarlyExit m)
