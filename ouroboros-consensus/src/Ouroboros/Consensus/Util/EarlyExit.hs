{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Ouroboros.Consensus.Util.EarlyExit (
    exitEarly
  , withEarlyExit
  , withEarlyExit_
    -- * Re-exports
  , lift
    -- * opaque
  , WithEarlyExit
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.ST (ST)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Data.Function (on)
import           Data.Proxy
import           NoThunks.Class (NoThunks (..))

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadEventlog
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTimer

import           Ouroboros.Consensus.Util ((.:))
import           Ouroboros.Consensus.Util.IOLike (IOLike (..),
                     MonadMonotonicTime (..), StrictMVar, StrictTVar)

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

instance (forall a'. NoThunks (m a'))
      => NoThunks (WithEarlyExit m a) where
   showTypeOf _p = "WithEarlyExit " ++ showTypeOf (Proxy @(m a))
   wNoThunks ctxt = wNoThunks ctxt . withEarlyExit

{-------------------------------------------------------------------------------
  Instances for io-classes
-------------------------------------------------------------------------------}

instance MonadSTM m => MonadSTM (WithEarlyExit m) where
  type STM (WithEarlyExit m) = WithEarlyExit (STM m)
  atomically                 = earlyExit . atomically . withEarlyExit

  type TVar    (WithEarlyExit m) = TVar    m
  type TMVar   (WithEarlyExit m) = TMVar   m
  type TQueue  (WithEarlyExit m) = TQueue  m
  type TBQueue (WithEarlyExit m) = TBQueue m

  newTVar         = lift .  newTVar
  readTVar        = lift .  readTVar
  writeTVar       = lift .: writeTVar
  retry           = lift    retry
  orElse          = (earlyExit .: orElse) `on` withEarlyExit
  newTMVar        = lift .  newTMVar
  newEmptyTMVar   = lift    newEmptyTMVar
  takeTMVar       = lift .  takeTMVar
  tryTakeTMVar    = lift .  tryTakeTMVar
  putTMVar        = lift .: putTMVar
  tryPutTMVar     = lift .: tryPutTMVar
  readTMVar       = lift .  readTMVar
  tryReadTMVar    = lift .  tryReadTMVar
  swapTMVar       = lift .: swapTMVar
  isEmptyTMVar    = lift .  isEmptyTMVar
  newTQueue       = lift    newTQueue
  readTQueue      = lift .  readTQueue
  tryReadTQueue   = lift .  tryReadTQueue
  peekTQueue      = lift .  peekTQueue
  tryPeekTQueue   = lift .  tryPeekTQueue
  writeTQueue     = lift .: writeTQueue
  isEmptyTQueue   = lift .  isEmptyTQueue
  newTBQueue      = lift .  newTBQueue
  readTBQueue     = lift .  readTBQueue
  tryReadTBQueue  = lift .  tryReadTBQueue
  peekTBQueue     = lift .  peekTBQueue
  tryPeekTBQueue  = lift .  tryPeekTBQueue
  flushTBQueue    = lift .  flushTBQueue
  writeTBQueue    = lift .: writeTBQueue
  lengthTBQueue   = lift .  lengthTBQueue
  isEmptyTBQueue  = lift .  isEmptyTBQueue
  isFullTBQueue   = lift .  isFullTBQueue

  newTMVarIO      = lift . newTMVarIO
  newEmptyTMVarIO = lift   newEmptyTMVarIO

instance MonadCatch m => MonadThrow (WithEarlyExit m) where
  throwIO = lift . throwIO

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

instance (MonadMask m, MonadAsync m, MonadCatch (STM m))
      => MonadAsync (WithEarlyExit m) where
  type Async (WithEarlyExit m) = WithEarlyExit (Async m)

  async            = lift . (fmap earlyExit . async) . withEarlyExit
  asyncThreadId    = asyncThreadId
  cancel        a  = lift $ cancel     (withEarlyExit a)
  cancelWith    a  = lift . cancelWith (withEarlyExit a)

  waitCatchSTM a = earlyExit (commute      <$> waitCatchSTM (withEarlyExit a))
  pollSTM      a = earlyExit (fmap commute <$> pollSTM      (withEarlyExit a))

  asyncWithUnmask f = earlyExit $ fmap (Just . earlyExit) $
    asyncWithUnmask $ \unmask ->
      withEarlyExit (f (earlyExit . unmask . withEarlyExit))

commute :: Either SomeException (Maybe a) -> Maybe (Either SomeException a)
commute (Left e)         = Just (Left e)
commute (Right Nothing)  = Nothing
commute (Right (Just a)) = Just (Right a)

instance MonadFork m => MonadFork (WithEarlyExit m) where
  forkIO           f = lift $ forkIO (collapse <$> withEarlyExit f)
  forkIOWithUnmask f = lift $ forkIOWithUnmask $ \unmask ->
                         let unmask' :: forall a. WithEarlyExit m a -> WithEarlyExit m a
                             unmask' = earlyExit . unmask . withEarlyExit
                         in collapse <$> withEarlyExit (f unmask')
  throwTo            = lift .: throwTo

instance MonadST m => MonadST (WithEarlyExit m) where
  withLiftST f = lowerLiftST $ \(_proxy :: Proxy s) liftST ->
     let liftST' :: forall a. ST s a -> WithEarlyExit m a
         liftST' = lift . liftST
     in f liftST'
    where
      lowerLiftST :: (forall s. Proxy s -> (forall a. ST s a -> m a) -> b) -> b
      lowerLiftST g = withLiftST $ g Proxy

instance MonadMonotonicTime m => MonadMonotonicTime (WithEarlyExit m) where
  getMonotonicTime = lift getMonotonicTime

instance MonadDelay m => MonadDelay (WithEarlyExit m) where
  threadDelay = lift . threadDelay

instance (MonadEvaluate m, MonadCatch m) => MonadEvaluate (WithEarlyExit m) where
  evaluate  = lift . evaluate

instance MonadEventlog m => MonadEventlog (WithEarlyExit m) where
  traceEventIO  = lift . traceEventIO
  traceMarkerIO = lift . traceMarkerIO

{-------------------------------------------------------------------------------
  Finally, the consensus IOLike wrapper
-------------------------------------------------------------------------------}

instance ( IOLike m
         , forall a. NoThunks (StrictTVar (WithEarlyExit m) a)
         , forall a. NoThunks (StrictMVar (WithEarlyExit m) a)
           -- The simulator does not currently support @MonadCatch (STM m)@,
           -- making this @IOLike@ instance applicable to @IO@ only. Once that
           -- missing @MonadCatch@ instance is added, @IOLike@ should require
           -- @MonadCatch (STM m)@ intsead of @MonadThrow (STM m)@.
           -- <https://github.com/input-output-hk/ouroboros-network/issues/1461>
         , MonadCatch (STM m)
         ) => IOLike (WithEarlyExit m) where
  forgetSignKeyKES = lift . forgetSignKeyKES
