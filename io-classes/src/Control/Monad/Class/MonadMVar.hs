{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
module Control.Monad.Class.MonadMVar
  ( MonadMVar (..)
  , MVarDefault
  , newEmptyMVarDefault
  , newMVarDefault
  , putMVarDefault
  , takeMVarDefault
  , readMVarDefault
  , tryTakeMVarDefault
  , tryPutMVarDefault
  , isEmptyMVarDefault
  ) where

import           Control.Exception (assert)
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import qualified Control.Concurrent.MVar as IO

import           Control.Monad.Trans (lift)
import           Control.Monad.Reader (ReaderT (..))

import           Data.Kind (Type)
import           Deque.Strict (Deque)
import qualified Deque.Strict as Deque


class Monad m => MonadMVar m where
  {-# MINIMAL newEmptyMVar, takeMVar, putMVar, tryTakeMVar, tryPutMVar, isEmptyMVar #-}

  type MVar m = (mvar :: Type -> Type) | mvar -> m
  
  newEmptyMVar      :: m (MVar m a)
  takeMVar          :: MVar m a -> m a
  putMVar           :: MVar m a -> a -> m ()
  tryTakeMVar       :: MVar m a -> m (Maybe a)
  tryPutMVar        :: MVar m a -> a -> m Bool
  isEmptyMVar       :: MVar m a -> m Bool

  -- methods with a default implementation
  newMVar           :: a -> m (MVar m a)
  readMVar          :: MVar m a -> m a
  swapMVar          :: MVar m a -> a -> m a
  withMVar          :: MVar m a -> (a -> m b) -> m b
  withMVarMasked    :: MVar m a -> (a -> m b) -> m b
  modifyMVar_       :: MVar m a -> (a -> m a) -> m ()
  modifyMVar        :: MVar m a -> (a -> m (a, b)) -> m b
  modifyMVarMasked_ :: MVar m a -> (a -> m a) -> m ()
  modifyMVarMasked  :: MVar m a -> (a -> m (a,b)) -> m b

  default newMVar :: a -> m (MVar m a)
  newMVar a = do 
    v <- newEmptyMVar 
    putMVar v a
    return v
  {-# INLINE newMVar #-}

  default readMVar :: MVar m a -> m a
  readMVar v = do
    a <- takeMVar v
    putMVar v a
    return a
  {-# INLINE readMVar #-}

  default swapMVar :: MonadMask m => MVar m a -> a -> m a
  swapMVar mvar new =
    mask_ $ do
      old <- takeMVar mvar
      putMVar mvar new
      return old
  {-# INLINE swapMVar #-}

  default withMVar :: MonadMask m => MVar m a -> (a -> m b) -> m b
  withMVar m io =
    mask $ \restore -> do
      a <- takeMVar m
      b <- restore (io a) `onException` putMVar m a
      putMVar m a
      return b
  {-# INLINE withMVar #-}

  default withMVarMasked :: MonadMask m => MVar m a -> (a -> m b) -> m b
  withMVarMasked m io =
    mask_ $ do
      a <- takeMVar m
      b <- io a `onException` putMVar m a
      putMVar m a
      return b
  {-# INLINE withMVarMasked #-}

  default modifyMVar_ :: MonadMask m => MVar m a -> (a -> m a) -> m ()
  modifyMVar_ m io =
    mask $ \restore -> do
      a  <- takeMVar m
      a' <- restore (io a) `onException` putMVar m a
      putMVar m a'
  {-# INLINE modifyMVar_ #-}

  default modifyMVar :: (MonadMask m, MonadEvaluate m)
                     => MVar m a -> (a -> m (a,b)) -> m b
  modifyMVar m io =
    mask $ \restore -> do
      a      <- takeMVar m
      (a',b) <- restore (io a >>= evaluate) `onException` putMVar m a
      putMVar m a'
      return b
  {-# INLINE modifyMVar #-}

  default modifyMVarMasked_ :: MonadMask m => MVar m a -> (a -> m a) -> m ()
  modifyMVarMasked_ m io =
    mask_ $ do
      a  <- takeMVar m
      a' <- io a `onException` putMVar m a
      putMVar m a'
  {-# INLINE modifyMVarMasked_ #-}

  default modifyMVarMasked :: (MonadMask m, MonadEvaluate m)
                           => MVar m a -> (a -> m (a,b)) -> m b
  modifyMVarMasked m io =
    mask_ $ do
      a      <- takeMVar m
      (a',b) <- (io a >>= evaluate) `onException` putMVar m a
      putMVar m a'
      return b
  {-# INLINE modifyMVarMasked #-}


instance MonadMVar IO where
    type MVar IO      = IO.MVar
    newEmptyMVar      = IO.newEmptyMVar
    newMVar           = IO.newMVar
    takeMVar          = IO.takeMVar
    putMVar           = IO.putMVar
    readMVar          = IO.readMVar
    swapMVar          = IO.swapMVar
    tryTakeMVar       = IO.tryTakeMVar
    tryPutMVar        = IO.tryPutMVar
    isEmptyMVar       = IO.isEmptyMVar
    withMVar          = IO.withMVar
    withMVarMasked    = IO.withMVarMasked
    modifyMVar_       = IO.modifyMVar_
    modifyMVar        = IO.modifyMVar
    modifyMVarMasked_ = IO.modifyMVarMasked_
    modifyMVarMasked  = IO.modifyMVarMasked


-- | A default 'MVar' implementation based on `TVar`'s.  An 'MVar' provides
-- fairness guarantees.
--
data MVarDefault m a = MVar (TVar m (Deque (ThreadId m)))
                            -- ^ threads blocked on reading from the MVar
                            (TVar m (Deque (ThreadId m)))
                            -- ^ threads blocked to write to the MVar
                            (TMVar m a)
                            -- ^ the value

newEmptyMVarDefault :: MonadSTM m => m (MVarDefault m a)
newEmptyMVarDefault = atomically $ MVar <$> newTVar mempty
                                        <*> newTVar mempty
                                        <*> newEmptyTMVar

newMVarDefault :: MonadSTM m => a -> m (MVarDefault m a)
newMVarDefault a = atomically $ MVar <$> newTVar mempty
                                     <*> newTVar mempty
                                     <*> newTMVar a


-- | Make a fair access to the resource guarded by a queue of 'ThreadId's
--
queue :: ( MonadFork m
         , MonadMask m
         , MonadSTM  m
         )
      => TVar m (Deque (ThreadId m))
      -> STM m a -- ^ action to perform when it is our turn.
      -> STM m a -- ^ action to perform when it is not our turn
      -> m a
queue v act wait = mask $ \unmask -> do
    tid <- myThreadId
    -- a non blocking stm action
    atomically $ do
      tids <- readTVar v
      case Deque.uncons tids of
        Nothing -> do
          writeTVar v (Deque.snoc tid tids)
        Just (tid', tids') | tid == tid' ->
          return ()
                           | tid `elem` tids' ->
          return ()
                           | otherwise -> do
          writeTVar v (Deque.snoc tid tids)

    unmask
      (atomically $ do
        tids <- readTVar v
        assert (tid `elem` tids) $
          case Deque.uncons tids of
            Nothing -> error "Control.Monad.Class.MonadMVar.queue: invariant violation"
            Just (tid', tids')
              | tid == tid' -> writeTVar v tids'
                            >> act
              | otherwise   -> wait
      ) `onException`
         atomically (modifyTVar v (Deque.filter (/= tid)))


putMVarDefault :: ( MonadFork m
                  , MonadMask m
                  , MonadSTM  m
                  )
               => MVarDefault m a -> a -> m ()
putMVarDefault (MVar _reading writing v) a =
    queue writing (putTMVar v a) retry


takeMVarDefault :: ( MonadFork m
                   , MonadMask m
                   , MonadSTM  m
                   )
                => MVarDefault m a
                -> m a
takeMVarDefault (MVar reading _writing v) =
    queue reading (takeTMVar v) retry


readMVarDefault :: ( MonadFork m
                   , MonadMask m
                   , MonadSTM  m
                   )
                => MVarDefault m a
                -> m a
readMVarDefault (MVar reading _writing v) =
    queue reading (readTMVar v) retry


tryTakeMVarDefault :: ( MonadFork m
                      , MonadMask m
                      , MonadSTM  m
                      )
                   => MVarDefault m a
                   -> m (Maybe a)
tryTakeMVarDefault (MVar reading _writing v) =
    queue reading (tryTakeTMVar v) (return Nothing)


tryPutMVarDefault :: ( MonadFork m
                     , MonadMask m
                     , MonadSTM  m
                     )
                  => MVarDefault m a -> a -> m Bool
tryPutMVarDefault (MVar _reading writing v) a =
    queue writing (tryPutTMVar v a) (return False)


isEmptyMVarDefault :: MonadSTM  m
                   => MVarDefault m a -> m Bool
isEmptyMVarDefault (MVar _reading _writing v) =
    atomically $ isEmptyTMVar v


--
-- ReaderT instance
--

newtype WrappedMVar r (m :: Type -> Type) a = WrappedMVar { unwrapMVar :: MVar m a }

instance ( MonadMask m
         , MonadMVar m
         , MonadEvaluate m
         ) => MonadMVar (ReaderT r m) where
    type MVar (ReaderT r m) = WrappedMVar r m
    newEmptyMVar = WrappedMVar <$> lift newEmptyMVar
    newMVar      = fmap WrappedMVar . lift . newMVar
    takeMVar     = lift .   takeMVar    . unwrapMVar
    putMVar      = lift .: (putMVar     . unwrapMVar)
    readMVar     = lift .   readMVar    . unwrapMVar
    swapMVar     = lift .: (swapMVar    . unwrapMVar)
    tryTakeMVar  = lift .   tryTakeMVar . unwrapMVar
    tryPutMVar   = lift .: (tryPutMVar  . unwrapMVar)
    isEmptyMVar  = lift .   isEmptyMVar . unwrapMVar
    withMVar (WrappedMVar v) f = ReaderT $ \r ->
      withMVar v (\a -> runReaderT (f a) r)
    withMVarMasked (WrappedMVar v) f = ReaderT $ \r ->
      withMVarMasked v (\a -> runReaderT (f a) r)
    modifyMVar_ (WrappedMVar v) f = ReaderT $ \r ->
      modifyMVar_ v (\a -> runReaderT (f a) r)
    modifyMVar (WrappedMVar v) f = ReaderT $ \r ->
      modifyMVar v (\a -> runReaderT (f a) r)
    modifyMVarMasked_ (WrappedMVar v) f = ReaderT $ \r ->
      modifyMVarMasked_ v (\a -> runReaderT (f a) r)
    modifyMVarMasked (WrappedMVar v) f = ReaderT $ \r ->
      modifyMVarMasked v (\a -> runReaderT (f a) r)




--
-- Utilities
--

(.:) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(f .: g) x y = f (g x y)
