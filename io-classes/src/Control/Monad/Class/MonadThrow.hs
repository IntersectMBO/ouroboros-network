{-# LANGUAGE DefaultSignatures         #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeFamilies              #-}

module Control.Monad.Class.MonadThrow
  ( MonadThrow(..)
  , MonadCatch(..)
  , MonadMask(..)
  , MonadEvaluate(..)
  , Exception(..)
  , SomeException
  , ExitCase(..)
  , Handler(..)
  , catches
  -- * Deprecated interfaces
  , throwM
  ) where

import           Control.Exception (Exception (..), SomeException)
import qualified Control.Exception as IO
import           Control.Monad (liftM)
import           Control.Monad.Except (ExceptT (..), lift, runExceptT)
import           Control.Monad.Reader (ReaderT (..), runReaderT)
import           Control.Monad.STM (STM)
import qualified Control.Monad.STM as STM

-- | Throwing exceptions, and resource handling in the presence of exceptions.
--
-- Does not include the ability to respond to exceptions.
--
class Monad m => MonadThrow m where

  {-# MINIMAL throwIO #-}
  throwIO :: Exception e => e -> m a

  bracket  :: m a -> (a -> m b) -> (a -> m c) -> m c
  bracket_ :: m a -> m b -> m c -> m c
  finally  :: m a -> m b -> m a

  default bracket :: MonadCatch m => m a -> (a -> m b) -> (a -> m c) -> m c

  bracket before after =
    liftM fst .
      generalBracket
        before
        (\a _exitCase -> after a)

  bracket_ before after thing = bracket before (const after) (const thing)

  a `finally` sequel =
    bracket_ (return ()) sequel a

throwM :: (MonadThrow m, Exception e) => e -> m a
throwM = throwIO
{-# DEPRECATED throwM "Use throwIO" #-}

-- | Catching exceptions.
--
-- Covers standard utilities to respond to exceptions.
--
class MonadThrow m => MonadCatch m where

  {-# MINIMAL catch #-}

  catch      :: Exception e => m a -> (e -> m a) -> m a
  catchJust  :: Exception e => (e -> Maybe b) -> m a -> (b -> m a) -> m a

  try        :: Exception e => m a -> m (Either e a)
  tryJust    :: Exception e => (e -> Maybe b) -> m a -> m (Either b a)

  handle     :: Exception e => (e -> m a) -> m a -> m a
  handleJust :: Exception e => (e -> Maybe b) -> (b -> m a) -> m a -> m a

  onException    :: m a -> m b -> m a
  bracketOnError :: m a -> (a -> m b) -> (a -> m c) -> m c

  -- | General form of bracket
  --
  -- See <http://hackage.haskell.org/package/exceptions-0.10.0/docs/Control-Monad-Catch.html#v:generalBracket>
  -- for discussion and motivation.
  generalBracket :: m a -> (a -> ExitCase b -> m c) -> (a -> m b) -> m (b, c)

  default generalBracket
                 :: MonadMask m
                 => m a -> (a -> ExitCase b -> m c) -> (a -> m b) -> m (b, c)

  catchJust p a handler =
      catch a handler'
    where
      handler' e = case p e of
                     Nothing -> throwIO e
                     Just b  -> handler b

  try a = catch (Right `fmap` a) (return . Left)

  tryJust p a = do
    r <- try a
    case r of
      Right v -> return (Right v)
      Left  e -> case p e of
                   Nothing -> throwIO e
                   Just b  -> return (Left b)

  handle       = flip catch
  handleJust p = flip (catchJust p)

  onException action what =
    action `catch` \e -> do
              _ <- what
              throwIO (e :: SomeException)

  bracketOnError acquire release = liftM fst . generalBracket
    acquire
    (\a exitCase -> case exitCase of
      ExitCaseSuccess _ -> return ()
      _ -> do
        _ <- release a
        return ())

  generalBracket acquire release use =
    mask $ \unmasked -> do
      resource <- acquire
      b <- unmasked (use resource) `catch` \e -> do
        _ <- release resource (ExitCaseException e)
        throwIO e
      c <- release resource (ExitCaseSuccess b)
      return (b, c)


-- | The default handler type for 'catches', whcih is a generalisation of
-- 'IO.Handler'.
--
data Handler m a = forall e. Exception e => Handler (e -> m a)

deriving instance (Functor m) => Functor (Handler m)

-- | Like 'catches' but for 'MonadCatch' rather than only 'IO'.
--
catches :: forall m a. MonadCatch m
         => m a -> [Handler m a] -> m a
catches ma handlers = ma `catch` catchesHandler handlers
{-# SPECIALISE catches :: IO a -> [Handler IO a] -> IO a #-}

-- | Used in the default 'catches' implementation.
--
catchesHandler :: MonadCatch m
               => [Handler m a]
               -> SomeException
               -> m a
catchesHandler handlers e = foldr tryHandler (throwIO e) handlers
    where tryHandler (Handler handler) res
              = case fromException e of
                Just e' -> handler e'
                Nothing -> res
{-# SPECIALISE catchesHandler :: [Handler IO a] -> SomeException -> IO a #-}


-- | Used in 'generalBracket'
--
-- See @exceptions@ package for discussion and motivation.
data ExitCase a
  = ExitCaseSuccess a
  | ExitCaseException SomeException
  | ExitCaseAbort
  deriving (Show, Functor)

-- | Support for safely working in the presence of asynchronous exceptions.
--
-- This is typically not needed directly as the utilities in 'MonadThrow' and
-- 'MonadCatch' cover most use cases.
--
class MonadCatch m => MonadMask m where

  {-# MINIMAL mask, uninterruptibleMask #-}
  mask, uninterruptibleMask :: ((forall a. m a -> m a) -> m b) -> m b

  mask_, uninterruptibleMask_ :: m a -> m a
  mask_                action = mask                $ \_ -> action
  uninterruptibleMask_ action = uninterruptibleMask $ \_ -> action


-- | Monads which can 'evaluate'.
--
class MonadThrow m => MonadEvaluate m where
    evaluate :: a -> m a

--
-- Instance for IO uses the existing base library implementations
--

instance MonadThrow IO where

  throwIO  = IO.throwIO

  bracket  = IO.bracket
  bracket_ = IO.bracket_
  finally  = IO.finally


instance MonadCatch IO where

  catch      = IO.catch

  catchJust  = IO.catchJust
  try        = IO.try
  tryJust    = IO.tryJust
  handle     = IO.handle
  handleJust = IO.handleJust
  onException    = IO.onException
  bracketOnError = IO.bracketOnError
  -- use default implementation of 'generalBracket' (base does not define one)


instance MonadMask IO where

  mask  = IO.mask
  mask_ = IO.mask_

  uninterruptibleMask  = IO.uninterruptibleMask
  uninterruptibleMask_ = IO.uninterruptibleMask_

instance MonadEvaluate IO where
  evaluate = IO.evaluate

--
-- Instance for STM uses STM primitives and default implementations
--

instance MonadThrow STM where
  throwIO = STM.throwSTM

instance MonadCatch STM where
  catch  = STM.catchSTM

  generalBracket acquire release use = do
    resource <- acquire
    b <- use resource `catch` \e -> do
      _ <- release resource (ExitCaseException e)
      throwIO e
    c <- release resource (ExitCaseSuccess b)
    return (b, c)


--
-- Instances for ReaderT
--

instance MonadThrow m => MonadThrow (ReaderT r m) where
  throwIO = lift . throwIO
  bracket acquire release use = ReaderT $ \env ->
    bracket
      (      runReaderT acquire     env)
      (\a -> runReaderT (release a) env)
      (\a -> runReaderT (use a)     env)

instance MonadCatch m => MonadCatch (ReaderT r m) where
  catch act handler = ReaderT $ \env ->
    catch
      (      runReaderT act         env)
      (\e -> runReaderT (handler e) env)

  generalBracket acquire release use = ReaderT $ \env ->
    generalBracket
      (        runReaderT acquire       env)
      (\a e -> runReaderT (release a e) env)
      (\a   -> runReaderT (use a)       env)

instance MonadMask m => MonadMask (ReaderT r m) where
  mask a = ReaderT $ \e -> mask $ \u -> runReaderT (a $ q u) e
    where q :: (m a -> m a) -> ReaderT e m a -> ReaderT e m a
          q u (ReaderT b) = ReaderT (u . b)
  uninterruptibleMask a =
    ReaderT $ \e -> uninterruptibleMask $ \u -> runReaderT (a $ q u) e
      where q :: (m a -> m a) -> ReaderT e m a -> ReaderT e m a
            q u (ReaderT b) = ReaderT (u . b)

instance MonadEvaluate m => MonadEvaluate (ReaderT r m) where
  evaluate = lift . evaluate

--
-- Instances for ExceptT
--
-- These all follow the @exceptions@ package to the letter
--

instance MonadCatch m => MonadThrow (ExceptT e m) where
  throwIO = lift . throwIO

instance MonadCatch m => MonadCatch (ExceptT e m) where
  catch (ExceptT m) f = ExceptT $ catch m (runExceptT . f)

  generalBracket acquire release use = ExceptT $ do
    (eb, ec) <- generalBracket
      (runExceptT acquire)
      (\eresource exitCase -> case eresource of
        Left e -> return (Left e) -- nothing to release, acquire didn't succeed
        Right resource -> case exitCase of
          ExitCaseSuccess (Right b) -> runExceptT (release resource (ExitCaseSuccess b))
          ExitCaseException e       -> runExceptT (release resource (ExitCaseException e))
          _                         -> runExceptT (release resource ExitCaseAbort))
      (either (return . Left) (runExceptT . use))
    return $ do
      -- The order in which we perform those two 'Either' effects determines
      -- which error will win if they are both 'Left's. We want the error from
      -- 'release' to win.
      c <- ec
      b <- eb
      return (b, c)

instance MonadMask m => MonadMask (ExceptT e m) where
  mask f = ExceptT $ mask $ \u -> runExceptT $ f (q u)
    where
      q :: (m (Either e a) -> m (Either e a))
        -> ExceptT e m a -> ExceptT e m a
      q u (ExceptT b) = ExceptT (u b)
  uninterruptibleMask f = ExceptT $ uninterruptibleMask $ \u -> runExceptT $ f (q u)
    where
      q :: (m (Either e a) -> m (Either e a))
        -> ExceptT e m a -> ExceptT e m a
      q u (ExceptT b) = ExceptT (u b)
