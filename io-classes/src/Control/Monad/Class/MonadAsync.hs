{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE QuantifiedConstraints  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Control.Monad.Class.MonadAsync
  ( MonadAsync (..)
  , AsyncCancelled (..)
  , ExceptionInLinkedThread (..)
  , link
  , linkTo
  , linkOnly
  , linkToOnly
  , mapConcurrently
  , forConcurrently
  , mapConcurrently_
  , forConcurrently_
  , replicateConcurrently
  , replicateConcurrently_
  , Concurrently (..)
  ) where

import           Prelude hiding (read)

import           Control.Applicative (Alternative (..), liftA2)
import           Control.Monad (forever)
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTimer

import           Control.Concurrent.Async (AsyncCancelled (..))
import qualified Control.Concurrent.Async as Async
import qualified Control.Exception as E

import           Data.Foldable (fold)
import           Data.Functor (void)
import           Data.Kind (Type)

class ( MonadSTM m
      , MonadThread m
      ) => MonadAsync m where

  {-# MINIMAL async, asyncThreadId, cancel, cancelWith, asyncWithUnmask,
              waitCatchSTM, pollSTM #-}

  -- | An asynchronous action
  type Async m          = (async :: Type -> Type) | async -> m

  async                 :: m a -> m (Async m a)
  asyncThreadId         :: Async m a -> ThreadId m
  withAsync             :: m a -> (Async m a -> m b) -> m b

  waitSTM               :: Async m a -> STM m a
  pollSTM               :: Async m a -> STM m (Maybe (Either SomeException a))
  waitCatchSTM          :: Async m a -> STM m (Either SomeException a)

  default waitSTM :: MonadThrow (STM m) => Async m a -> STM m a
  waitSTM action = waitCatchSTM action >>= either throwSTM return

  waitAnySTM            :: [Async m a] -> STM m (Async m a, a)
  waitAnyCatchSTM       :: [Async m a] -> STM m (Async m a, Either SomeException a)
  waitEitherSTM         :: Async m a -> Async m b -> STM m (Either a b)
  waitEitherSTM_        :: Async m a -> Async m b -> STM m ()
  waitEitherCatchSTM    :: Async m a -> Async m b
                        -> STM m (Either (Either SomeException a)
                                         (Either SomeException b))
  waitBothSTM           :: Async m a -> Async m b -> STM m (a, b)

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

  default waitAnySTM     :: MonadThrow (STM m) => [Async m a] -> STM m (Async m a, a)
  default waitEitherSTM  :: MonadThrow (STM m) => Async m a -> Async m b -> STM m (Either a b)
  default waitEitherSTM_ :: MonadThrow (STM m) => Async m a -> Async m b -> STM m ()
  default waitBothSTM    :: MonadThrow (STM m) => Async m a -> Async m b -> STM m (a, b)

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

instance MonadAsync IO where

  type Async IO         = Async.Async

  async                 = Async.async
  asyncThreadId         = Async.asyncThreadId
  withAsync             = Async.withAsync

  waitSTM               = Async.waitSTM
  pollSTM               = Async.pollSTM
  waitCatchSTM          = Async.waitCatchSTM

  waitAnySTM            = Async.waitAnySTM
  waitAnyCatchSTM       = Async.waitAnyCatchSTM
  waitEitherSTM         = Async.waitEitherSTM
  waitEitherSTM_        = Async.waitEitherSTM_
  waitEitherCatchSTM    = Async.waitEitherCatchSTM
  waitBothSTM           = Async.waitBothSTM

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
    linkedThreadId = asyncThreadId a

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
