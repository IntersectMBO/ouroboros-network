{-# LANGUAGE RankNTypes #-}

module Ouroboros.Network.Testing.QuickCheck
  ( runSimGen
  , monadicSim
  ) where

import           Test.QuickCheck
import           Test.QuickCheck.Gen.Unsafe (Capture (..), capture)
import           Test.QuickCheck.Monadic

import           Control.Monad.IOSim

-- | 'IOSim' analogue of 'runSTGen'
--
-- > runSTGen  :: (forall s. Gen (ST    s a)) -> Gen a
-- > runSimGen :: (forall s. Gen (IOSim s a)) -> Gen a
runSimGen :: (forall s. Gen (IOSim s a)) -> Gen a
runSimGen f = do
    Capture eval <- capture
    return $ runSimOrThrow (eval f)

-- | 'IOSim' analogue of 'monadicST'
--
-- > monadicST  :: Testable a => (forall s. PropertyM (ST    s) a) -> Property
-- > monadicSim :: Testable a => (forall s. PropertyM (IOSim s) a) -> Property
monadicSim :: Testable a => (forall s. PropertyM (IOSim s) a) -> Property
monadicSim m = property (runSimGen (monadic' m))
