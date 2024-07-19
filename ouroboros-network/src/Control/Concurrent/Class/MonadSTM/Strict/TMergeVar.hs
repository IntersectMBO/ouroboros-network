{-# LANGUAGE BangPatterns #-}

-- | STM TMergeVar mini-abstraction
--
module Control.Concurrent.Class.MonadSTM.Strict.TMergeVar
  ( TMergeVar
  , newTMergeVar
  , writeTMergeVar
  , takeTMergeVar
  , tryReadTMergeVar
  ) where

import Control.Concurrent.Class.MonadSTM.Strict

-- | The 'TMergeVar' is like a 'TMVar' in that we take it, leaving it empty.
-- Unlike an ordinary 'TMVar' with a blocking \'put\' operation, it has a
-- non-blocking combining write operation: if a value is already present then
-- the values are combined using the 'Semigroup' operator.
--
-- This is used much like a 'TMVar' as a one-place queue between threads but
-- with the property that we can \"improve\" the current value (if any).
--
newtype TMergeVar m a = TMergeVar (StrictTMVar m a)

newTMergeVar :: MonadSTM m => STM m (TMergeVar m a)
newTMergeVar = TMergeVar <$> newEmptyTMVar

-- | Merge the current value with the given one and store it, return the updated
-- value.
--
writeTMergeVar :: (MonadSTM m, Semigroup a) => TMergeVar m a -> a -> STM m a
writeTMergeVar (TMergeVar v) x = do
    mx0 <- tryTakeTMVar v
    case mx0 of
      Nothing -> x  <$ putTMVar v x
      Just x0 -> x' <$ putTMVar v x' where !x' = x0 <> x

takeTMergeVar :: MonadSTM m => TMergeVar m a -> STM m a
takeTMergeVar (TMergeVar v) = takeTMVar v

tryReadTMergeVar :: MonadSTM m
                 => TMergeVar m a
                 -> STM m (Maybe a)
tryReadTMergeVar (TMergeVar v) = tryReadTMVar v
