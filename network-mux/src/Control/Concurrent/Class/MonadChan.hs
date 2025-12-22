{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies   #-}

module Control.Concurrent.Class.MonadChan
  ( MonadChan (..) 
    -- * ChanM
  , ChanM
  , readChanM
  , tryReadChanM
  , writeChanM
  )
  where

import Control.Concurrent.Class.MonadMVar.Strict (MonadMVar, StrictMVar)
import Control.Concurrent.Class.MonadMVar.Strict qualified as MVar
import Control.Monad.Class.MonadSay
import Control.Monad.Class.MonadThrow
import Data.Kind (Type)


class (MonadSay m, MonadMVar m) => MonadChan m where
  type Chan m :: Type -> Type
  newChan     :: m (Chan m a)
  readChan    :: Chan m a -> m a
  tryReadChan :: Chan m a -> m (Maybe a)
  writeChan   :: Chan m a -> a -> m ()
  isEmpty     :: Chan m a -> m Bool


-- NOTE: This implementatio is not bounded!
-- TODO: Do we need a bounded queue for `Egress`, which is not that a trivial
-- extension of `Chan`/ChanM`?
-- We are planning to add egress rate limiting:
-- https://github.com/IntersectMBO/ouroboros-network/issues/5263, at the bearer
-- level, so a bounded queue might not be needed - the back-pressure will come
-- directly from the bearer.
--
data ChanM m a
  = Chan {
      readVar  :: !(StrictMVar m (Stream m a)),
      writeVar :: !(StrictMVar m (Stream m a))
    }

type Stream m a = StrictMVar m (Item m a)

data Item m a = Item !a !(Stream m a) 

newChanM
  :: MonadMVar m
  => m (ChanM m a)
{-# SPECIALISE newChanM :: IO (ChanM IO a) #-}

newChanM = do
  hole     <- MVar.newEmptyMVar
  readVar  <- MVar.newMVar hole
  writeVar <- MVar.newMVar hole
  return (Chan readVar writeVar)


readChanM
  :: MonadMVar m
  => ChanM m a
  -> m a
{-# SPECIALISE readChanM :: ChanM IO a -> IO a #-}

readChanM Chan { readVar } =
  MVar.modifyMVar readVar $ \read_end -> do
    Item val new_read_end <- MVar.readMVar read_end
    return (new_read_end, val)


tryReadChanM
  :: (MonadMVar m, MonadMask m)
  => ChanM m a
  -> m (Maybe a)
{-# SPECIALISE tryReadChanM :: ChanM IO a -> IO (Maybe a) #-}

-- TODO: make it async-exception safe
tryReadChanM Chan { readVar } = do
  bracketOnError
    (MVar.tryTakeMVar readVar)
    (\case
        Nothing       -> return ()
        Just read_end -> MVar.putMVar readVar read_end
    )
    $ \case
        Nothing -> return Nothing
        Just read_end ->
          -- Mask exceptions to avoid asynchronous interruption, all actions
          -- below are non-blocking, so everything is safe.
          mask_ $ do
            MVar.tryTakeMVar read_end >>= \case
              Nothing -> do
                -- `putMVar` is non-blocking here because `readVar` was taken
                MVar.putMVar readVar read_end
                return Nothing
              Just (Item a new_read_end) -> do
                -- `putMVar` is non-blocking here because `readVar` was taken
                MVar.putMVar readVar new_read_end
                return (Just a)


writeChanM
  :: MonadMask m
  => MonadMVar m
  => ChanM m a
  -> a
  -> m ()
{-# SPECIALISE writeChan :: ChanM IO a -> a -> IO () #-}

writeChanM Chan { writeVar } a = do
  new_hole <- MVar.newEmptyMVar
  mask_ $ do
    old_hole <- MVar.takeMVar writeVar
    MVar.putMVar old_hole (Item a new_hole)
    MVar.putMVar writeVar new_hole


-- | Check if `ChanM` is non-empty.  It might block but only for a short period
-- of time when some other threads is writing to an empty `ChanM`.
--
isEmptyM
  :: MonadMVar m
  => ChanM m a
  -> m Bool
isEmptyM Chan { readVar } = do
  a <- MVar.readMVar readVar >>= MVar.tryReadMVar
  return $ case a of
    Just {} -> False
    Nothing -> True


-- tryReadAllChanM :: MonadMVar m => ChanM m a -> m [a]
-- tryReadAllChanM Chan { readVar } = go []
--   where
--     go acc = do
--       MVar.tryTa


instance MonadChan IO where
  type Chan IO = ChanM IO
  newChan      = newChanM
  readChan     = readChanM
  tryReadChan  = tryReadChanM
  writeChan    = writeChanM
  isEmpty      = isEmptyM

