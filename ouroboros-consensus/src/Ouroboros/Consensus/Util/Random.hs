{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Ouroboros.Consensus.Util.Random (
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
      -- * Run MonadRandom computations
    , RunMonadRandom (..)
    , runMonadRandomIO
      -- * Convenience re-exports
    , MonadRandom (..)
    , ChaChaDRG
    )
    where

import           Codec.Serialise (Serialise)
import           Control.Monad.State
import           Crypto.Number.Generate (generateBetween)
import           Crypto.Random (ChaChaDRG, MonadPseudoRandom, MonadRandom (..),
                     drgNewTest, randomBytesGenerate, withDRG)
import           Data.List (genericLength)
import           Data.Word (Word64)

import           Control.Monad.Class.MonadSTM

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

-- | Add DRNG to a monad stack
newtype ChaChaT m a = ChaChaT { unChaChaT :: StateT ChaChaDRG m a }
  deriving (Functor, Applicative, Monad, MonadTrans)

instance Monad m => MonadRandom (ChaChaT m) where
  getRandomBytes = ChaChaT . state . randomBytesGenerate

instance MonadSTM m => MonadSTM (ChaChaT m) where
  type STM (ChaChaT m) = STM m
  atomically = lift . atomically

-- | Run the 'MonadPseudoRandomT' monad
--
-- This is the analogue of cryptonite's 'withDRG'.
runChaChaT :: ChaChaT m a -> ChaChaDRG -> m (a, ChaChaDRG)
runChaChaT = runStateT  . unChaChaT

{-------------------------------------------------------------------------------
  Run MonadRandom computations
-------------------------------------------------------------------------------}

newtype RunMonadRandom m = RunMonadRandom
  { runMonadRandom :: forall a. MonadSTM m
                   => (forall n. ( MonadRandom n
                                 , MonadSTM    n
                                 , STM         n ~ STM m
                                 ) => n a)
                   -> m a
  }

-- | Use the 'MonadRandom' instance for 'IO'.
runMonadRandomIO :: RunMonadRandom IO
runMonadRandomIO = RunMonadRandom id
