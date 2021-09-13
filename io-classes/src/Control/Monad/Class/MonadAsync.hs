{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Control.Monad.Class.MonadAsync
  ( MonadAsync (..)
  , MonadAsyncSTM (..)
  , AsyncCancelled(..)
  , ExceptionInLinkedThread(..)
  , link
  , linkTo
  , linkOnly
  , linkToOnly

  , mapConcurrently, forConcurrently
  , mapConcurrently_, forConcurrently_
  , replicateConcurrently, replicateConcurrently_
  , Concurrently (..)
  ) where

import           Prelude hiding (read)

import           Control.Applicative (Alternative (..), liftA2)
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadTimer
import           Control.Monad.Class.MonadThrow

import           Control.Concurrent.Async (AsyncCancelled (..))
import qualified Control.Concurrent.Async as Async
import qualified Control.Exception as E
import           Control.Monad.Reader
import qualified Control.Monad.STM as STM

import           Data.Foldable (fold)
import           Data.Kind (Type)
import           Data.Proxy

class (Functor async, MonadSTMTx stm) => MonadAsyncSTM async stm where
  {-# MINIMAL waitCatchSTM, pollSTM #-}

  waitSTM      :: async a -> stm a
  pollSTM      :: async a -> stm (Maybe (Either SomeException a))
  waitCatchSTM :: async a -> stm (Either SomeException a)

  default waitSTM :: MonadThrow stm => async a -> stm a
  waitSTM action = waitCatchSTM action >>= either throwSTM return

  waitAnySTM            :: [async a] -> stm (async a, a)
  waitAnyCatchSTM       :: [async a] -> stm (async a, Either SomeException a)
  waitEitherSTM         :: async a -> async b -> stm (Either a b)
  waitEitherSTM_        :: async a -> async b -> stm ()
  waitEitherCatchSTM    :: async a -> async b
                        -> stm (Either (Either SomeException a)
                                         (Either SomeException b))
  waitBothSTM           :: async a -> async b -> stm (a, b)

  default waitAnySTM     :: MonadThrow stm => [async a] -> stm (async a, a)
  default waitEitherSTM  :: MonadThrow stm => async a -> async b -> stm (Either a b)
  default waitEitherSTM_ :: MonadThrow stm => async a -> async b -> stm ()
  default waitBothSTM    :: MonadThrow stm => async a -> async b -> stm (a, b)

  waitAnySTM as =
    foldr orElse retry $
      map (\a -> do r <- waitSTM a; return (a, r)) as

  waitAnyCatchSTM as =
    foldr orElse retry $
      map (\a -> do r <- waitCatchSTM a; return (a, r)) as

  waitEitherSTM left right =
    (Left  <$> waitSTM left)
      `orElse`
    (Right <$> waitSTM right)

  waitEitherSTM_ left right =
      (void $ waitSTM left)
        `orElse`
      (void $ waitSTM right)

  waitEitherCatchSTM left right =
      (Left  <$> waitCatchSTM left)
        `orElse`
      (Right <$> waitCatchSTM right)

  waitBothSTM left right = do
      a <- waitSTM left
             `orElse`
           (waitSTM right >> retry)
      b <- waitSTM right
      return (a,b)

class ( MonadSTM m
      , MonadThread m
      , MonadAsyncSTM (Async m) (STM m)
      ) => MonadAsync m where

  {-# MINIMAL async, asyncThreadId, cancel, cancelWith, asyncWithUnmask #-}

  -- | An asynchronous action
  type Async m :: Type -> Type

  async                 :: m a -> m (Async m a)
  asyncThreadId         :: Proxy m -> Async m a -> ThreadId m
  withAsync             :: m a -> (Async m a -> m b) -> m b

  wait                  :: Async m a -> m a
  poll                  :: Async m a -> m (Maybe (Either SomeException a))
  waitCatch             :: Async m a -> m (Either SomeException a)
  cancel                :: Async m a -> m ()
  cancelWith            :: Exception e => Async m a -> e -> m ()
  uninterruptibleCancel :: Async m a -> m ()

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

  race                  :: m a -> m b -> m (Either a b)
  race_                 :: m a -> m b -> m ()
  concurrently          :: m a -> m b -> m (a,b)
  concurrently_         :: m a -> m b -> m ()

  asyncWithUnmask       :: ((forall b . m b -> m b) ->  m a) -> m (Async m a)

  -- default implementations
  default withAsync     :: MonadMask m => m a -> (Async m a -> m b) -> m b
  default uninterruptibleCancel
                        :: MonadMask m => Async m a -> m ()
  default waitAnyCancel         :: MonadThrow m => [Async m a] -> m (Async m a, a)
  default waitAnyCatchCancel    :: MonadThrow m => [Async m a]
                                -> m (Async m a, Either SomeException a)
  default waitEitherCancel      :: MonadThrow m => Async m a -> Async m b
                                -> m (Either a b)
  default waitEitherCatchCancel :: MonadThrow m => Async m a -> Async m b
                                -> m (Either (Either SomeException a)
                                             (Either SomeException b))

  withAsync action inner = mask $ \restore -> do
                             a <- async (restore action)
                             restore (inner a)
                               `finally` uninterruptibleCancel a

  wait      = atomically . waitSTM
  poll      = atomically . pollSTM
  waitCatch = atomically . waitCatchSTM

  uninterruptibleCancel      = uninterruptibleMask_ . cancel

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

  race            left right = withAsync left  $ \a ->
                               withAsync right $ \b ->
                                 waitEither a b

  race_           left right = withAsync left  $ \a ->
                               withAsync right $ \b ->
                                 waitEither_ a b

  concurrently    left right = withAsync left  $ \a ->
                               withAsync right $ \b ->
                                 waitBoth a b

  concurrently_   left right = void $ concurrently left right

-- | Similar to 'Async.Concurrently' but which works for any 'MonadAsync'
-- instance.
--
newtype Concurrently m a = Concurrently { runConcurrently :: m a }

instance Functor m => Functor (Concurrently m) where
    fmap f (Concurrently ma) = Concurrently (fmap f ma)

instance ( Applicative m
         , MonadAsync m
         ) => Applicative (Concurrently m) where
    pure = Concurrently . pure

    Concurrently fn <*> Concurrently as =
      Concurrently $
        (\(f, a) -> f a)
        `fmap`
        concurrently fn as

instance ( Alternative m
         , MonadAsync  m
         , MonadTimer  m
         ) => Alternative (Concurrently m) where
    empty = Concurrently $ forever (threadDelay 86400)
    Concurrently as <|> Concurrently bs =
      Concurrently $ either id id <$> as `race` bs

instance ( Semigroup  a
         , MonadAsync m
         ) => Semigroup (Concurrently m a) where
    (<>) = liftA2 (<>)

instance ( Monoid a
         , MonadAsync m
         ) => Monoid (Concurrently m a) where
    mempty = pure mempty


mapConcurrently :: (Traversable t, MonadAsync m) => (a -> m b) -> t a -> m (t b)
mapConcurrently f = runConcurrently . traverse (Concurrently . f)

forConcurrently :: (Traversable t, MonadAsync m) => t a -> (a -> m b) -> m (t b)
forConcurrently = flip mapConcurrently

mapConcurrently_ :: (Foldable f, MonadAsync m) => (a -> m b) -> f a -> m ()
mapConcurrently_ f = runConcurrently . foldMap (Concurrently . void . f)

forConcurrently_ :: (Foldable f, MonadAsync m) => f a -> (a -> m b) -> m ()
forConcurrently_ = flip mapConcurrently_

replicateConcurrently :: MonadAsync m => Int -> m a -> m [a]
replicateConcurrently cnt = runConcurrently . sequenceA . replicate cnt . Concurrently

replicateConcurrently_ :: MonadAsync m => Int -> m a -> m ()
replicateConcurrently_ cnt = runConcurrently . fold . replicate cnt . Concurrently . void


--
-- Instance for IO uses the existing async library implementations
--

instance MonadAsyncSTM Async.Async STM.STM where
  waitSTM            = Async.waitSTM
  pollSTM            = Async.pollSTM
  waitCatchSTM       = Async.waitCatchSTM
  waitAnySTM         = Async.waitAnySTM
  waitAnyCatchSTM    = Async.waitAnyCatchSTM
  waitEitherSTM      = Async.waitEitherSTM
  waitEitherSTM_     = Async.waitEitherSTM_
  waitEitherCatchSTM = Async.waitEitherCatchSTM
  waitBothSTM        = Async.waitBothSTM

instance MonadAsync IO where

  type Async IO         = Async.Async

  async                 = Async.async
  asyncThreadId         = \_proxy -> Async.asyncThreadId
  withAsync             = Async.withAsync

  wait                  = Async.wait
  poll                  = Async.poll
  waitCatch             = Async.waitCatch
  cancel                = Async.cancel
  cancelWith            = Async.cancelWith
  uninterruptibleCancel = Async.uninterruptibleCancel

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

  race                  = Async.race
  race_                 = Async.race_
  concurrently          = Async.concurrently
  concurrently_         = Async.concurrently_

  asyncWithUnmask       = Async.asyncWithUnmask

--
-- Lift to ReaderT
--

(.:) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(f .: g) x y = f (g x y)

instance MonadAsync m => MonadAsync (ReaderT r m) where
  type Async (ReaderT r m) = Async m

  asyncThreadId _ = asyncThreadId (Proxy @m)

  async     (ReaderT ma)   = ReaderT $ \r -> async (ma r)
  withAsync (ReaderT ma) f = ReaderT $ \r -> withAsync (ma r) $ \a -> runReaderT (f a) r
  asyncWithUnmask        f = ReaderT $ \r ->
                              asyncWithUnmask $ \unmask ->
                                runReaderT (f (liftF unmask)) r
    where
      liftF :: (m a -> m a) ->  ReaderT r m a -> ReaderT r m a
      liftF g (ReaderT r) = ReaderT (g . r)

  race         (ReaderT ma) (ReaderT mb) = ReaderT $ \r -> race         (ma r) (mb r)
  race_        (ReaderT ma) (ReaderT mb) = ReaderT $ \r -> race_        (ma r) (mb r)
  concurrently (ReaderT ma) (ReaderT mb) = ReaderT $ \r -> concurrently (ma r) (mb r)

  wait                  = lift .  wait
  poll                  = lift .  poll
  waitCatch             = lift .  waitCatch
  cancel                = lift .  cancel
  uninterruptibleCancel = lift .  uninterruptibleCancel
  cancelWith            = lift .: cancelWith
  waitAny               = lift .  waitAny
  waitAnyCatch          = lift .  waitAnyCatch
  waitAnyCancel         = lift .  waitAnyCancel
  waitAnyCatchCancel    = lift .  waitAnyCatchCancel
  waitEither            = lift .: waitEither
  waitEitherCatch       = lift .: waitEitherCatch
  waitEitherCancel      = lift .: waitEitherCancel
  waitEitherCatchCancel = lift .: waitEitherCatchCancel
  waitEither_           = lift .: waitEither_
  waitBoth              = lift .: waitBoth

--
-- Linking
--
-- Adapted from "Control.Concurrent.Async"
--
-- We don't use the implementation of linking from 'Control.Concurrent.Async'
-- directly because:
--
-- 1. We need a generalized form of linking that links an async to an arbitrary
--    thread ('linkTo')
-- 2. If we /did/ use the real implementation, then the mock implementation and
--    the real implementation would not be able to throw the same exception,
--    because the exception type used by the real implementation is
--
-- > data ExceptionInLinkedThread =
-- >   forall a . ExceptionInLinkedThread (Async a) SomeException
--
--    containing a reference to the real 'Async' type.
--

-- | Exception from child thread re-raised in parent thread
--
-- We record the thread ID of the child thread as a 'String'. This avoids
-- an @m@ parameter in the type, which is important: 'ExceptionInLinkedThread'
-- must be an instance of 'Exception', requiring it to be 'Typeable'; if @m@
-- appeared in the type, we would require @m@ to be 'Typeable', which does not
-- work with with the simulator, as it would require a 'Typeable' constraint
-- on the @s@ parameter of 'IOSim'.
data ExceptionInLinkedThread = ExceptionInLinkedThread String SomeException

instance Show ExceptionInLinkedThread where
  showsPrec p (ExceptionInLinkedThread a e) =
    showParen (p >= 11) $
      showString "ExceptionInLinkedThread " .
      showsPrec 11 a .
      showString " " .
      showsPrec 11 e

instance Exception ExceptionInLinkedThread where
  fromException = E.asyncExceptionFromException
  toException = E.asyncExceptionToException

-- | Generalizion of 'link' that links an async to an arbitrary thread.
linkTo :: (MonadAsync m, MonadFork m, MonadMask m)
       => ThreadId m -> Async m a -> m ()
linkTo tid = linkToOnly tid (not . isCancel)

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
    linkedThreadId = asyncThreadId (Proxy @m) a

    exceptionInLinkedThread :: SomeException -> ExceptionInLinkedThread
    exceptionInLinkedThread =
        ExceptionInLinkedThread (show linkedThreadId)

link :: (MonadAsync m, MonadFork m, MonadMask m)
     => Async m a -> m ()
link = linkOnly (not . isCancel)

linkOnly :: forall m a. (MonadAsync m, MonadFork m, MonadMask m)
         => (SomeException -> Bool) -> Async m a -> m ()
linkOnly shouldThrow a = do
    me <- myThreadId
    linkToOnly me shouldThrow a

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
