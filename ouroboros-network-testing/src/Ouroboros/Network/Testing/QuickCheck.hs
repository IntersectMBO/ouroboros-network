{-# LANGUAGE RankNTypes #-}

module Ouroboros.Network.Testing.QuickCheck (
    runSimGen
  , monadicSim
  ) where

import           Test.QuickCheck
import           Test.QuickCheck.Gen.Unsafe (Capture (..), capture)
import           Test.QuickCheck.Monadic

import           Control.Monad.IOSim

-- | 'SimM' analogue of 'runSTGen'
--
-- > runSTGen  :: (forall s. Gen (ST   s a)) -> Gen a
-- > runSimGen :: (forall s. Gen (SimM s a)) -> Gen a
runSimGen :: (forall s. Gen (SimM s a)) -> Gen a
runSimGen f = do
    Capture eval <- capture
    return $ runSimOrThrow (eval f)

-- | 'SimM' analogue of 'monadicST'
--
-- > monadicST  :: Testable a => (forall s. PropertyM (ST   s) a) -> Property
-- > monadicSim :: Testable a => (forall s. PropertyM (SimM s) a) -> Property
monadicSim :: Testable a => (forall s. PropertyM (SimM s) a) -> Property
monadicSim m = property (runSimGen (monadic' m))
