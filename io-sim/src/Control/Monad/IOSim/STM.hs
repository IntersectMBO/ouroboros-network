{-# LANGUAGE BangPatterns #-}

-- | 'io-sim' implementation of 'TQueue' and 'TBQueue'.  Unlike the default
-- implementation available in 'io-classes' they are using a single 'TVar',
-- which simplifies the implementation of 'traceTQueue' and 'traceTBQueue'
-- methods.
--
module Control.Monad.IOSim.STM where

import           Control.Monad.Class.MonadSTM (MonadSTM (..),
                   MonadInspectSTM (..), MonadLabelledSTM (..),
                   MonadTraceSTM (..), TraceValue (..))

import           Numeric.Natural (Natural)

--
-- Default TQueue implementation in terms of 'Seq' (used by sim)
--

newtype TQueueDefault m a = TQueue (TVar m ([a], [a]))
 
labelTQueueDefault
  :: MonadLabelledSTM m
  => TQueueDefault m a -> String -> STM m ()
labelTQueueDefault (TQueue queue) label =  labelTVar queue label

traceTQueueDefault
  :: MonadTraceSTM m
  => proxy m
  -> TQueueDefault m a
  -> (Maybe [a] -> [a] -> InspectMonad m TraceValue)
  -> STM m ()
traceTQueueDefault p (TQueue queue) f =
    traceTVar p queue
              (\mas as -> f (g <$> mas) (g as))
  where
    g (xs, ys) = xs ++ reverse ys

newTQueueDefault :: MonadSTM m => STM m (TQueueDefault m a)
newTQueueDefault = TQueue <$> newTVar ([], [])

writeTQueueDefault :: MonadSTM m => TQueueDefault m a -> a -> STM m ()
writeTQueueDefault (TQueue queue) a = do
    (xs, ys) <- readTVar queue
    writeTVar queue $! (xs, a : ys)

readTQueueDefault :: MonadSTM m => TQueueDefault m a -> STM m a
readTQueueDefault queue = maybe retry return =<< tryReadTQueueDefault queue

tryReadTQueueDefault :: MonadSTM m => TQueueDefault m a -> STM m (Maybe a)
tryReadTQueueDefault (TQueue queue) = do
  (xs, ys) <- readTVar queue
  case xs of
    (x:xs') -> do
      writeTVar queue $! (xs', ys)
      return (Just x)
    [] ->
      case reverse ys of
        []     -> return Nothing
        (z:zs) -> do
          writeTVar queue $! (zs, [])
          return (Just z)

isEmptyTQueueDefault :: MonadSTM m => TQueueDefault m a -> STM m Bool
isEmptyTQueueDefault (TQueue queue) = do
  (xs, ys) <- readTVar queue
  return $ case xs of
    _:_ -> False
    []  -> case ys of
             [] -> True
             _  -> False

peekTQueueDefault :: MonadSTM m => TQueueDefault m a -> STM m a
peekTQueueDefault (TQueue queue) = do
    (xs, _) <- readTVar queue
    case xs of
      x :_ -> return x
      []   -> retry

tryPeekTQueueDefault :: MonadSTM m => TQueueDefault m a -> STM m (Maybe a)
tryPeekTQueueDefault (TQueue queue) = do
    (xs, _) <- readTVar queue
    return $ case xs of
      x :_ -> Just x
      []   -> Nothing

--
-- Default TBQueue implementation in terms of 'Seq' (used by sim)
--

data TBQueueDefault m a = TBQueue
  !(TVar m ([a], Natural, [a], Natural))
  !Natural

labelTBQueueDefault
  :: MonadLabelledSTM m
  => TBQueueDefault m a -> String -> STM m ()
labelTBQueueDefault (TBQueue queue _size) label = labelTVar queue label

traceTBQueueDefault
  :: MonadTraceSTM m
  => proxy m
  -> TBQueueDefault m a
  -> (Maybe [a] -> [a] -> InspectMonad m TraceValue)
  -> STM m ()
traceTBQueueDefault p (TBQueue queue _size) f =
    traceTVar p queue (\mas as -> f (g <$> mas) (g as))
  where
    g (xs, _, ys, _) = xs ++ reverse ys


newTBQueueDefault :: MonadSTM m => Natural -> STM m (TBQueueDefault m a)
newTBQueueDefault size | size >= fromIntegral (maxBound :: Int)
                       = error "newTBQueueDefault: size larger than Int"
newTBQueueDefault size =
  flip TBQueue size <$> (newTVar $! ([], 0, [], size))

readTBQueueDefault :: MonadSTM m => TBQueueDefault m a -> STM m a
readTBQueueDefault queue = maybe retry return =<< tryReadTBQueueDefault queue

tryReadTBQueueDefault :: MonadSTM m => TBQueueDefault m a -> STM m (Maybe a)
tryReadTBQueueDefault (TBQueue queue _size) = do
  (xs, r, ys, w) <- readTVar queue
  let !r' = r + 1
  case xs of
    (x:xs') -> do
      writeTVar queue $! (xs', r', ys, w)
      return (Just x)
    [] ->
      case reverse ys of
        [] -> do
          writeTVar queue $! (xs, r', ys, w)
          return Nothing

        -- NB. lazy: we want the transaction to be
        -- short, otherwise it will conflict
        (z:zs) -> do
           writeTVar queue $! (zs, r', [], w)
           return (Just z)

peekTBQueueDefault :: MonadSTM m => TBQueueDefault m a -> STM m a
peekTBQueueDefault queue = maybe retry return =<< tryPeekTBQueueDefault queue

tryPeekTBQueueDefault :: MonadSTM m => TBQueueDefault m a -> STM m (Maybe a)
tryPeekTBQueueDefault (TBQueue queue _size) = do
    (xs, _, _, _) <- readTVar queue
    return $ case xs of
      (x:_) -> Just x
      _     -> Nothing

writeTBQueueDefault :: MonadSTM m => TBQueueDefault m a -> a -> STM m ()
writeTBQueueDefault (TBQueue queue _size) a = do
  (xs, r, ys, w) <- readTVar queue
  if (w > 0)
    then do let !w' = w - 1
            writeTVar queue $! (xs, r, a:ys, w')
    else do
          if (r > 0)
            then let !w' = r - 1 in
                 writeTVar queue (xs, 0, a:ys, w')
            else retry

isEmptyTBQueueDefault :: MonadSTM m => TBQueueDefault m a -> STM m Bool
isEmptyTBQueueDefault (TBQueue queue _size) = do
  (xs, _, _, _) <- readTVar queue
  case xs of
    _:_ -> return False
    []  -> return True

isFullTBQueueDefault :: MonadSTM m => TBQueueDefault m a -> STM m Bool
isFullTBQueueDefault (TBQueue queue _size) = do
  (_, r, _, w) <- readTVar queue
  return $ 
    if (w > 0)
    then False
    else if (r > 0)
         then False
         else True

lengthTBQueueDefault :: MonadSTM m => TBQueueDefault m a -> STM m Natural
lengthTBQueueDefault (TBQueue queue size) = do
  (_, r, _, w) <- readTVar queue
  return $! size - r - w

flushTBQueueDefault :: MonadSTM m => TBQueueDefault m a -> STM m [a]
flushTBQueueDefault (TBQueue queue size) = do
  (xs, _, ys, _) <- readTVar queue
  if null xs && null ys
    then return []
    else do
      writeTVar queue $! ([], 0, [], size)
      return (xs ++ reverse ys)
