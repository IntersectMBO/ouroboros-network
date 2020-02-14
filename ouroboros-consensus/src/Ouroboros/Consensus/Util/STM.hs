{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Ouroboros.Consensus.Util.STM (
    -- * Misc
    blockUntilChanged
  , onEachChange
  , runWhenJust
  , blockUntilJust
  , blockUntilAllJust
  , Fingerprint (..)
  , WithFingerprint (..)
    -- * Simulate various monad stacks in STM
  , Sim(..)
  , simId
  , simStateT
  , simOuroborosStateT
  , simChaChaT
  ) where

import           Control.Monad.State
import           Data.Coerce
import           Data.Void
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           GHC.Stack

import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.Random
import           Ouroboros.Consensus.Util.ResourceRegistry

{-------------------------------------------------------------------------------
  Misc
-------------------------------------------------------------------------------}

-- | Wait until the TVar changed
blockUntilChanged :: forall stm a b. (MonadSTMTx stm, Eq b)
                  => (a -> b) -> b -> stm a -> stm (a, b)
blockUntilChanged f b getA = do
    a <- getA
    let b' = f a
    if b' == b
      then retry
      else return (a, b')

-- | Spawn a new thread that runs an action each time an STM value changes.
--
-- NOTE: STM does not guarantee that 'onEachChange' will /literally/ be called
-- on /every/ change: when the system is under heavy load, some updates may
-- be missed.
--
-- The thread will be linked to the registry.
onEachChange :: forall m a b. (IOLike m, Eq b, HasCallStack)
             => ResourceRegistry m
             -> (a -> b)  -- ^ Obtain a fingerprint
             -> Maybe b   -- ^ Optional initial fingerprint
                          -- If 'Nothing', the action is executed once
                          -- immediately to obtain the initial fingerprint.
             -> STM m a
             -> (a -> m ())
             -> m (Thread m Void)
onEachChange registry f mbInitB getA notify =
    forkLinkedThread registry body
  where
    body :: m Void
    body = do
        initB <- case mbInitB of
          Just initB -> return initB
          Nothing    -> do
            a <- atomically getA
            notify a
            return $ f a
        loop initB

    loop :: b -> m Void
    loop b = do
      (a, b') <- atomically $ blockUntilChanged f b getA
      notify a
      loop b'

-- | Spawn a new thread that waits for an STM value to become 'Just'
--
-- The thread will be linked to the registry.
runWhenJust :: IOLike m
            => ResourceRegistry m
            -> STM m (Maybe a)
            -> (a -> m ())
            -> m ()
runWhenJust registry getMaybeA action =
    void $ forkLinkedThread registry $
      action =<< atomically (blockUntilJust getMaybeA)

blockUntilJust :: MonadSTMTx stm => stm (Maybe a) -> stm a
blockUntilJust getMaybeA = do
    ma <- getMaybeA
    case ma of
      Nothing -> retry
      Just a  -> return a

blockUntilAllJust :: MonadSTMTx stm => [stm (Maybe a)] -> stm [a]
blockUntilAllJust = mapM blockUntilJust

-- | Simple type that can be used to indicate something in a @TVar@ is
-- changed.
newtype Fingerprint = Fingerprint Word64
  deriving stock    (Show, Eq, Generic)
  deriving newtype  (Enum)
  deriving anyclass (NoUnexpectedThunks)

-- | Store a value together with its fingerprint.
data WithFingerprint a = WithFingerprint
  { forgetFingerprint :: !a
  , getFingerprint    :: !Fingerprint
  } deriving (Show, Eq, Functor, Generic, NoUnexpectedThunks)

{-------------------------------------------------------------------------------
  Simulate monad stacks
-------------------------------------------------------------------------------}

newtype Sim n m = Sim { runSim :: forall a. n a -> STM m a }

simId :: Sim (STM m) m
simId = Sim id

simStateT :: IOLike m => StrictTVar m st -> Sim n m -> Sim (StateT st n) m
simStateT stVar (Sim k) = Sim $ \(StateT f) -> do
    st       <- readTVar stVar
    (a, st') <- k (f st)
    writeTVar stVar st'
    return a

simOuroborosStateT :: IOLike m
                   => StrictTVar m s
                   -> Sim n m
                   -> Sim (NodeStateT_ s n) m
simOuroborosStateT stVar (Sim k) = Sim $ \n -> do
    st       <- readTVar stVar
    (a, st') <- k (runNodeStateT n st)
    writeTVar stVar st'
    return a

simChaChaT :: (IOLike m, Coercible a ChaChaDRG)
           => StrictTVar m a
           -> Sim n m
           -> Sim (ChaChaT n) m
simChaChaT stVar (Sim k) = Sim $ \n -> do
    st       <- readTVar stVar
    (a, st') <- k (runChaChaT n (coerce st))
    writeTVar stVar (coerce st')
    return a
