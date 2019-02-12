{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE DefaultSignatures #-}

module Control.Monad.Class.MonadThrow
  ( MonadThrow(..)
  , MonadCatch(..)
  , MonadMask(..)
  , Exception(..)
  , SomeException
  ) where

import           Control.Exception (Exception(..), SomeException)
import qualified Control.Exception as IO


-- | Throwing exceptions, and resource handling in the presence of exceptions.
--
-- Does not include the ability to respond to exceptions.
--
class Monad m => MonadThrow m where

  {-# MINIMAL throwM #-}
  throwM :: Exception e => e -> m a 

  bracket  :: m a -> (a -> m b) -> (a -> m c) -> m c
  bracket_ :: m a -> m b -> m c -> m c 
  finally  :: m a -> m b -> m a

  default bracket :: MonadMask m => m a -> (a -> m b) -> (a -> m c) -> m c
  default finally :: MonadMask m => m a -> m b -> m a

  bracket before after thing =
    mask $ \restore -> do
      a <- before
      r <- restore (thing a) `onException` after a
      _ <- after a
      return r

  bracket_ before after thing = bracket before (const after) (const thing)

  a `finally` sequel =
    mask $ \restore -> do
      r <- restore a `onException` sequel
      _ <- sequel
      return r


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

  default bracketOnError
                 :: MonadMask m => m a -> (a -> m b) -> (a -> m c) -> m c

  catchJust p a handler =
      catch a handler'
    where
      handler' e = case p e of
                     Nothing -> throwM e
                     Just b  -> handler b

  try a = catch (Right `fmap` a) (return . Left)

  tryJust p a = do
    r <- try a
    case r of
      Right v -> return (Right v)
      Left  e -> case p e of
                   Nothing -> throwM e
                   Just b  -> return (Left b)

  handle       = flip catch
  handleJust p = flip (catchJust p)

  onException action what =
    action `catch` \e -> do
              _ <- what
              throwM (e :: SomeException)

  bracketOnError before after thing =
    mask $ \restore -> do
      a <- before
      restore (thing a) `onException` after a


-- | Support for safely working in the presence of asynchronous exceptions.
--
-- This is typically not needed directly as the utilities in 'MonadThrow' and
-- 'MonadCatch' cover most use cases.
--
class MonadCatch m => MonadMask m where

  {-# MINIMAL mask #-}
  mask :: ((forall a. m a -> m a) -> m b) -> m b

  mask_ :: m a -> m a
  mask_ action = mask $ \_ -> action


--
-- Instance for IO uses the existing base library implementations
--

instance MonadThrow IO where

  throwM   = IO.throwIO

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


instance MonadMask IO where

  mask  = IO.mask
  mask_ = IO.mask_

