{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Util.Random (
    -- * Producing values in MonadRandom
    generateElement
    -- * Connecting MonadRandom to Gen
  , Seed (..)
  , withSeed
  , seedToChaCha
  , nullSeed
    -- * Adding DRNG to a monad stack
  , ChaChaT -- opaque
  , runChaChaT
  , simMonadRandom
    -- * Convenience re-exports
  , MonadRandom (..)
  , MonadTrans(..)
  , ChaChaDRG
  ) where

import           Codec.Serialise (Serialise)
import           Control.Monad.Reader
import           Control.Monad.Trans (MonadTrans (..))
import           Crypto.Number.Generate (generateBetween)
import           Crypto.Random (ChaChaDRG, MonadPseudoRandom, MonadRandom (..),
                     drgNewTest, randomBytesGenerate, withDRG)
import           Data.List (genericLength)
import           Data.Tuple (swap)
import           Data.Word (Word64)

import           Cardano.Prelude (NoUnexpectedThunks, OnlyCheckIsWHNF (..))

import           Ouroboros.Consensus.Util.IOLike

{-------------------------------------------------------------------------------
  Producing values in MonadRandom
-------------------------------------------------------------------------------}

generateElement :: MonadRandom m => [a] -> m (Maybe a)
generateElement [] = return Nothing
generateElement xs = do
    i <- fromIntegral <$> generateBetween 0 (genericLength xs - 1)
    return $ Just $ xs !! i

{-------------------------------------------------------------------------------
  Connecting MonadRandom to Gen
-------------------------------------------------------------------------------}

newtype Seed = Seed {getSeed :: (Word64, Word64, Word64, Word64, Word64)}
    deriving (Show, Eq, Ord, Serialise)

withSeed :: Seed -> MonadPseudoRandom ChaChaDRG a -> a
withSeed s = fst . withDRG (drgNewTest $ getSeed s)

seedToChaCha :: Seed -> ChaChaDRG
seedToChaCha = drgNewTest . getSeed

nullSeed :: Seed
nullSeed = Seed (0,0,0,0,0)

{-------------------------------------------------------------------------------
  Adding DRNG to a monad stack
-------------------------------------------------------------------------------}

deriving via OnlyCheckIsWHNF "ChaChaDRG" ChaChaDRG
         instance NoUnexpectedThunks ChaChaDRG

-- | Add DRNG to a monad stack
newtype ChaChaT m a = ChaChaT (ReaderT (StrictTVar m ChaChaDRG) m a)
  deriving (Functor, Applicative, Monad, MonadThrow, MonadCatch, MonadMask)

instance MonadTrans ChaChaT where
  lift = ChaChaT . lift

instance MonadSTM m => MonadRandom (ChaChaT m) where
  getRandomBytes n = ChaChaT $ ReaderT $ \varPRNG ->
      atomically $ updateTVar varPRNG $ swap . randomBytesGenerate n

instance MonadSTM m => MonadSTM (ChaChaT m) where
  type STM (ChaChaT m) = STM m
  atomically = lift . atomically

-- | Run the 'MonadPseudoRandomT' monad
--
-- This is the analogue of cryptonite's 'withDRG'.
runChaChaT :: MonadSTM m => ChaChaT m a -> ChaChaDRG -> m (a, ChaChaDRG)
runChaChaT ma prng = do
    varPRNG <- newTVarM prng
    a       <- simMonadRandom varPRNG ma
    prng'   <- atomically $ readTVar varPRNG
    return (a, prng')

simMonadRandom :: StrictTVar m ChaChaDRG -> ChaChaT m a -> m a
simMonadRandom varPRNG (ChaChaT ma) = runReaderT ma varPRNG
