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
    , simMonadRandom
      -- * Convenience re-exports
    , MonadRandom (..)
    , MonadTrans(..)
    , ChaChaDRG
    )
    where

import           Codec.Serialise (Serialise)
import           Control.Monad.State
import           Control.Monad.Trans (MonadTrans (..))
import           Crypto.Number.Generate (generateBetween)
import           Crypto.Random (ChaChaDRG, MonadPseudoRandom, MonadRandom (..),
                     drgNew, drgNewTest, randomBytesGenerate, withDRG)
import           Data.List (genericLength)
import           Data.Word (Word64)

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

-- | Simulate 'MonadRandom' when it's not actually available
--
-- We store a 'ChaChaDRG' in a TVar, and whenever we need to run a computation
-- requiring 'MonadRandom', we split the 'ChaChaDRG', store a split half back
-- in the TVar and run the computation using the other half. Each random
-- computation will use a different DRG, even concurrent ones.
--
-- Remember that will only be used in the tests code, in the real
-- implementation we will use 'runMonadRandomIO'.
simMonadRandom
  :: MonadSTM m
  => StrictTVar m ChaChaDRG
  -> ChaChaT m a
  -> m a
simMonadRandom varRNG n = do
    -- We can't run @n@ atomically, so to make sure we never run
    -- computations with the same RNG, we first split it,
    -- /atomically/, and then run the computation with one half of
    -- the RNG.
    rng <- atomically $ do
      rng <- readTVar varRNG
      ((split1, split2), _rng') <- runChaChaT fakeSplitDRG rng
      writeTVar varRNG split1
      return split2
    fst <$> runChaChaT n rng
  where
    -- The 'ChaChaDRG' is not splittable. We fake splitting it by using the
    -- 'ChaChaT' 'MonadRandom' instance to generate two new 'ChaChaDRG's. If
    -- this turns out to be problematic, there is actually no need to store a
    -- 'ChaChaDRG' in the 'TVar'; we should instead use a splittable PRNG, and
    -- then use in combination with 'drgNew' or 'drgNewTest' to produce a
    -- 'ChaChaDRG'.
    fakeSplitDRG :: MonadRandom m => m (ChaChaDRG, ChaChaDRG)
    fakeSplitDRG = (,) <$> drgNew <*> drgNew
