{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE QuantifiedConstraints  #-}

module Control.Monad.Class.MonadAsync
  ( MonadAsync (..)
  , AsyncCancelled(..)
  ) where

import           Prelude hiding (read)

import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Exception (SomeException)
import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.Async (AsyncCancelled(..))

class ( MonadSTM m
      , forall a. Eq  (Async m a)
      , forall a. Ord (Async m a)
      ) => MonadAsync m where

  {-# MINIMAL async, asyncThreadId, cancel, cancelWith, waitCatchSTM, pollSTM #-}

  -- | An asynchronous action
  type Async m :: * -> *

  async                 :: m a -> m (Async m a)
  asyncThreadId         :: proxy m -> Async m a -> ThreadId m
  withAsync             :: m a -> (Async m a -> m b) -> m b

  wait                  :: Async m a -> m a
  poll                  :: Async m a -> m (Maybe (Either SomeException a))
  waitCatch             :: Async m a -> m (Either SomeException a)
  cancel                :: Async m a -> m ()
  cancelWith            :: Exception e => Async m a -> e -> m ()
  uninterruptibleCancel :: Async m a -> m ()

  waitSTM               :: Async m a -> STM m a
  pollSTM               :: Async m a -> STM m (Maybe (Either SomeException a))
  waitCatchSTM          :: Async m a -> STM m (Either SomeException a)

  waitAny               :: [Async m a] -> m (Async m a, a)
  waitAnyCatch          :: [Async m a] -> m (Async m a, Either SomeException a)
  waitAnyCancel         :: [Async m a] -> m (Async m a, a)
  waitAnyCatchCancel    :: [Async m a] -> m (Async m a, Either SomeException a)
  waitEither            :: Async m a -> Async m b -> m (Either a b)

  -- | Note, IO-based implementations should override the default
  -- implementation. See the @async@ package implementation and comments.
  -- <http://hackage.haskell.org/package/async-2.2.1/docs/src/Control.Concurrent.Async.html#waitEitherCatch>
  waitEitherCatch       :: Async m a -> Async m b -> m (Either (Either SomeException a)
                                                               (Either SomeException b))
  waitEitherCancel      :: Async m a -> Async m b -> m (Either a b)
  waitEitherCatchCancel :: Async m a -> Async m b -> m (Either (Either SomeException a)
                                                               (Either SomeException b))
  waitEither_           :: Async m a -> Async m b -> m ()
  waitBoth              :: Async m a -> Async m b -> m (a, b)

  waitAnySTM            :: [Async m a] -> STM m (Async m a, a)
  waitAnyCatchSTM       :: [Async m a] -> STM m (Async m a, Either SomeException a)
  waitEitherSTM         :: Async m a -> Async m b -> STM m (Either a b)
  waitEitherSTM_        :: Async m a -> Async m b -> STM m ()
  waitEitherCatchSTM    :: Async m a -> Async m b
                        -> STM m (Either (Either SomeException a)
                                         (Either SomeException b))
  waitBothSTM           :: Async m a -> Async m b -> STM m (a, b)

  race                  :: m a -> m b -> m (Either a b)
  race_                 :: m a -> m b -> m ()
  concurrently          :: m a -> m b -> m (a,b)

  -- default implementations
  default withAsync     :: MonadMask m => m a -> (Async m a -> m b) -> m b
  default uninterruptibleCancel
                        :: MonadMask m => Async m a -> m ()
  default waitSTM       :: MonadThrow (STM m) => Async m a -> STM m a
  default waitAnyCancel         :: MonadThrow m => [Async m a] -> m (Async m a, a)
  default waitAnyCatchCancel    :: MonadThrow m => [Async m a]
                                -> m (Async m a, Either SomeException a)
  default waitEitherCancel      :: MonadThrow m => Async m a -> Async m b
                                -> m (Either a b)
  default waitEitherCatchCancel :: MonadThrow m => Async m a -> Async m b
                                -> m (Either (Either SomeException a)
                                             (Either SomeException b))

  default waitAnySTM     :: MonadThrow (STM m) => [Async m a] -> STM m (Async m a, a)
  default waitEitherSTM  :: MonadThrow (STM m) => Async m a -> Async m b -> STM m (Either a b)
  default waitEitherSTM_ :: MonadThrow (STM m) => Async m a -> Async m b -> STM m ()
  default waitBothSTM    :: MonadThrow (STM m) => Async m a -> Async m b -> STM m (a, b)


  withAsync action inner = mask $ \restore -> do
                             a <- async (restore action)
                             restore (inner a)
                               `finally` uninterruptibleCancel a

  wait      = atomically . waitSTM
  poll      = atomically . pollSTM
  waitCatch = atomically . waitCatchSTM

  uninterruptibleCancel      = uninterruptibleMask_ . cancel
  waitSTM action             = waitCatchSTM action >>= either throwM return

  waitAny                    = atomically . waitAnySTM
  waitAnyCatch               = atomically . waitAnyCatchSTM
  waitEither      left right = atomically (waitEitherSTM left right)
  waitEither_     left right = atomically (waitEitherSTM_ left right)
  waitEitherCatch left right = atomically (waitEitherCatchSTM left right)
  waitBoth        left right = atomically (waitBothSTM left right)

  waitAnyCancel asyncs =
    waitAny asyncs `finally` mapM_ cancel asyncs

  waitAnyCatchCancel asyncs =
    waitAnyCatch asyncs `finally` mapM_ cancel asyncs

  waitEitherCancel left right =
    waitEither left right `finally` (cancel left >> cancel right)

  waitEitherCatchCancel left right =
    waitEitherCatch left right `finally` (cancel left >> cancel right)

  -- Our MonadSTM does not cover orElse, so these all use low level versions
  waitAnySTM []     = retry
  waitAnySTM (a:as) = do
    mr <- pollSTM a
    case mr of
      Nothing        -> waitAnySTM as
      Just (Left  e) -> throwM e
      Just (Right r) -> return (a, r)
{-
    foldr orElse retry $
      map (\a -> do r <- waitSTM a; return (a, r)) asyncs
-}

  waitAnyCatchSTM []     = retry
  waitAnyCatchSTM (a:as) = do
    mr <- pollSTM a
    case mr of
      Nothing -> waitAnyCatchSTM as
      Just r  -> return (a, r)
{-
    foldr orElse retry $
      map (\a -> do r <- waitCatchSTM a; return (a, r)) asyncs
-}

  waitEitherSTM left right = do
    ml <- pollSTM left
    mr <- pollSTM right
    case (ml, mr) of
      (Just (Left  e), _) -> throwM e
      (Just (Right l), _) -> return (Left l)
      (_, Just (Left  e)) -> throwM e
      (_, Just (Right r)) -> return (Right r)
      (Nothing,  Nothing) -> retry
{-
    (Left  <$> waitSTM left)
      `orElse`
    (Right <$> waitSTM right)
-}


  waitEitherSTM_ left right = do
    ml <- pollSTM left
    mr <- pollSTM right
    case (ml, mr) of
      (Just (Left  e), _) -> throwM e
      (Just (Right _), _) -> return ()
      (_, Just (Left  e)) -> throwM e
      (_, Just (Right _)) -> return ()
      (Nothing,  Nothing) -> retry
{-
      (void $ waitSTM left)
        `orElse`
      (void $ waitSTM right)
-}

  waitEitherCatchSTM left right = do
    ml <- pollSTM left
    mr <- pollSTM right
    case (ml, mr) of
      (Just l,  _      ) -> return (Left l)
      (_,       Just r ) -> return (Right r)
      (Nothing, Nothing) -> retry
{-
      (Left  <$> waitCatchSTM left)
        `orElse`
      (Right <$> waitCatchSTM right)
-}

  waitBothSTM left right = do
    ml <- pollSTM left
    mr <- pollSTM right
    case (ml, mr) of
      (Just (Left  e), _)              -> throwM e
      (_,              Just (Left  e)) -> throwM e
      (Just (Right l), Just (Right r)) -> return (l, r)
      (_,  _)                          -> retry
{-
      a <- waitSTM left
             `orElse`
           (waitSTM right >> retry)
      b <- waitSTM right
      return (a,b)
-}

  race            left right = withAsync left  $ \a ->
                               withAsync right $ \b ->
                                 waitEither a b

  race_           left right = withAsync left  $ \a ->
                               withAsync right $ \b ->
                                 waitEither_ a b

  concurrently    left right = withAsync left  $ \a ->
                               withAsync right $ \b ->
                                 waitBoth a b

--
-- Instance for IO uses the existing async library implementations
--

instance MonadAsync IO where

  type Async IO = Async.Async

  async                 = Async.async
  asyncThreadId         = \_proxy -> Async.asyncThreadId
  withAsync             = Async.withAsync

  wait                  = Async.wait
  poll                  = Async.poll
  waitCatch             = Async.waitCatch
  cancel                = Async.cancel
  cancelWith            = Async.cancelWith
  uninterruptibleCancel = Async.uninterruptibleCancel

  waitSTM               = Async.waitSTM
  pollSTM               = Async.pollSTM
  waitCatchSTM          = Async.waitCatchSTM

  waitAny               = Async.waitAny
  waitAnyCatch          = Async.waitAnyCatch
  waitAnyCancel         = Async.waitAnyCancel
  waitAnyCatchCancel    = Async.waitAnyCatchCancel
  waitEither            = Async.waitEither
  waitEitherCatch       = Async.waitEitherCatch
  waitEitherCancel      = Async.waitEitherCancel
  waitEitherCatchCancel = Async.waitEitherCatchCancel
  waitEither_           = Async.waitEither_
  waitBoth              = Async.waitBoth

  waitAnySTM            = Async.waitAnySTM
  waitAnyCatchSTM       = Async.waitAnyCatchSTM
  waitEitherSTM         = Async.waitEitherSTM
  waitEitherSTM_        = Async.waitEitherSTM_
  waitEitherCatchSTM    = Async.waitEitherCatchSTM
  waitBothSTM           = Async.waitBothSTM

  race                  = Async.race
  race_                 = Async.race_
  concurrently          = Async.concurrently
