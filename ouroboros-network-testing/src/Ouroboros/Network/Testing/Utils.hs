{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE DerivingStrategies         #-}

module Ouroboros.Network.Testing.Utils where

import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadSay
import           Control.Tracer (Tracer (..))

import           Data.Ratio

import           Test.QuickCheck

import           Debug.Trace (traceShowM)


newtype Delay = Delay { getDelay :: DiffTime }
  deriving Show
  deriving newtype (Eq, Ord, Num)


genDelayWithPrecision :: Integer -> Gen DiffTime
genDelayWithPrecision precision = 
    sized $ \n -> do
      b <- chooseInteger (1, precision)
      a <- chooseInteger (0, toInteger n * b)
      return (fromRational (a % b))

-- | This needs to be small, as we are using real time limits in block-fetch
-- examples.
--
instance Arbitrary Delay where
    arbitrary = Delay <$> genDelayWithPrecision 10
    shrink (Delay delay) | delay >= 0.1 = [ Delay (delay - 0.1) ]
                         | otherwise    = []


newtype SmallDelay = SmallDelay { getSmallDelay :: DiffTime }
  deriving Show
  deriving newtype (Eq, Ord, Num)

instance Arbitrary SmallDelay where
    arbitrary = resize 5 $ SmallDelay . getDelay <$> suchThat arbitrary (\(Delay d ) -> d < 5)
    shrink (SmallDelay delay) | delay >= 0.1 = [ SmallDelay (delay - 0.1) ]
                              | otherwise    = []

--
-- Debugging tools
--

debugTracer :: ( Show a, Applicative m) => Tracer m a
debugTracer = Tracer traceShowM

sayTracer :: ( Show a, MonadSay m) => Tracer m a
sayTracer = Tracer (say . show)

