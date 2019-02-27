{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Error handling
--
-- Intended for qualified import
--
-- > import Ouroboros.Storage.Util.ErrorHandling (ErrorHandling(..))
-- > import qualified Ouroboros.Storage.Util.ErrorHandling as EH
module Ouroboros.Storage.Util.ErrorHandling (
    ErrorHandling(..)
  , try
  , onException
  , monadError
  , exceptions
  , exceptT
  , monadCatch
  , embed
  , liftErrNewtype
  , liftErrReader
  , liftErrState
  ) where

import           Control.Exception (Exception)
import qualified Control.Exception as E
import           Control.Monad.Except (ExceptT, MonadError)
import qualified Control.Monad.Except as M
import           Control.Monad.Reader (ReaderT (..), runReaderT)
import           Control.Monad.State (StateT (..), runStateT)
import           Data.Type.Coercion

import           Control.Monad.Class.MonadThrow (MonadCatch)
import qualified Control.Monad.Class.MonadThrow as C

-- | Reification of the 'MonadError' class
--
-- Unlike 'MonadError', it is perfectly fine to have multiple 'ErrorHandling'
-- objects available in a single context, as the caller can decide explicitly
-- which to call. Moreover, being regular records, they can have additional
-- information in the closure, if necessary (logging handles, for instance).
data ErrorHandling e m = ErrorHandling {
      throwError :: forall a. e -> m a
    , catchError :: forall a. m a -> (e -> m a) -> m a
    }

try :: Monad m => ErrorHandling e m -> m a -> m (Either e a)
try ErrorHandling{..} act = (Right <$> act) `catchError` (return . Left)

-- | Like @finally@, but only performs the final action if there was an
-- exception raised by the computation.
onException :: Monad m => ErrorHandling e m -> m a -> m b -> m a
onException ErrorHandling{..} act onEx = act `catchError` \e -> do
    _ <- onEx
    throwError e

monadError :: MonadError e m => ErrorHandling e m
monadError = ErrorHandling {
      throwError = M.throwError
    , catchError = M.catchError
    }

exceptT :: Monad m => ErrorHandling e (ExceptT e m)
exceptT = monadError

monadCatch :: (MonadCatch m, Exception e) => ErrorHandling e m
monadCatch = ErrorHandling {
      throwError = C.throwM
    , catchError = C.catch
    }

exceptions :: Exception e => ErrorHandling e IO
exceptions = ErrorHandling {
      throwError = E.throwIO
    , catchError = E.catch
    }

-- | Embed one kind of error in another
embed :: (e' -> e)
      -> (e -> Maybe e')
      -> ErrorHandling e m -> ErrorHandling e' m
embed intro elim ErrorHandling{..} = ErrorHandling{
      throwError = \e -> throwError (intro e)
    , catchError = \act handler -> catchError act $ \e ->
                     case elim e of
                       Nothing -> throwError e
                       Just e' -> handler e'
    }

-- | Lift for a newtype
--
-- TODO: This would be much nicer with QuantifiedConstraints.
liftErrNewtype :: forall e m m'.
                  (forall a. Coercion (m a) (m' a))
               -> ErrorHandling e m -> ErrorHandling e m'
liftErrNewtype c ErrorHandling{..} = ErrorHandling {
      throwError = \err         -> to $ throwError err
    , catchError = \act handler -> to $ catchError (from act) (\e -> from $ handler e)
    }
  where
    to :: forall a. m a -> m' a
    to = coerceWith c

    from :: forall a. m' a -> m a
    from = coerceWith (sym c)

-- | Lift to 'ReaderT'
liftErrReader :: proxy env -> ErrorHandling e m -> ErrorHandling e (ReaderT env m)
liftErrReader _ ErrorHandling{..} = ErrorHandling{
      throwError = \err         -> ReaderT $ \_env ->
                                     throwError err
    , catchError = \act handler -> ReaderT $ \env ->
                                     catchError (runReaderT act env) $ \e ->
                                       runReaderT (handler e) env
    }

-- | Lift to 'StateT'
liftErrState :: proxy st -> ErrorHandling e m -> ErrorHandling e (StateT st m)
liftErrState _ ErrorHandling{..} = ErrorHandling{
      throwError = \err         -> StateT $ \_st ->
                                     throwError err
    , catchError = \act handler -> StateT $ \st ->
                                     catchError (runStateT act st) $ \e ->
                                       runStateT (handler e) st
    }
