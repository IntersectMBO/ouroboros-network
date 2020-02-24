module Test.Util.Random
  ( runMonadRandomWithTVar
  ) where

import           Control.Monad.Trans (lift)

import           Crypto.Random (MonadRandom, drgNew)

import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.Random

-- | A 'RunMonadRandom' record to use in cases where we don't have access to
-- the system random number generator, e.g., in the tests.
--
-- We store a 'ChaChaDRG' in a TVar, and whenever we need to run a computation
-- requiring 'MonadRandom', we split the 'ChaChaDRG', store a split half back
-- in the TVar and run the computation using the other half. Each random
-- computation will use a different DRG, even concurrent ones.
--
-- Remember that will only be used in the tests code, in the real
-- implementation we will use 'runMonadRandomIO'.
runMonadRandomWithTVar
  :: MonadSTM m
  => StrictTVar m ChaChaDRG
  -> RunMonadRandom m
runMonadRandomWithTVar varRNG = RunMonadRandom $ \n -> do
    -- We can't run @n@ atomically, so to make sure we never run
    -- computations with the same RNG, we first split it,
    -- /atomically/, and then run the computation with one half of
    -- the RNG.
    rng <- atomically $ do
      rng <- readTVar varRNG
      ((split1, split2), _rng') <- runChaChaT fakeSplitDRG rng
      writeTVar varRNG split1
      return split2
    fst <$> runChaChaT (n lift) rng
  where
    -- The 'ChaChaDRG' is not splittable. We fake splitting it by using the
    -- 'ChaChaT' 'MonadRandom' instance to generate two new 'ChaChaDRG's. If
    -- this turns out to be problematic, there is actually no need to store a
    -- 'ChaChaDRG' in the 'TVar'; we should instead use a splittable PRNG, and
    -- then use in combination with 'drgNew' or 'drgNewTest' to produce a
    -- 'ChaChaDRG'.
    fakeSplitDRG :: MonadRandom m => m (ChaChaDRG, ChaChaDRG)
    fakeSplitDRG = (,) <$> drgNew <*> drgNew
