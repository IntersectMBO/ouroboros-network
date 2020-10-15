{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.Testing.Utils where

import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadSay
import           Control.Tracer (Tracer (..))

import           Test.QuickCheck

import           Debug.Trace (traceShowM)


newtype Delay = Delay { getDelay :: DiffTime }
  deriving Show


-- | This needs to be small, as we are using real time limits in block-fetch
-- examples.
--
instance Arbitrary Delay where
    arbitrary = do
      Positive (delay :: Float) <- resize 5 arbitrary
      return (Delay (realToFrac delay))
    shrink (Delay delay) | delay >= 0.1 = [ Delay (delay - 0.1) ]
                         | otherwise    = []


--
-- Debugging tools
--

debugTracer :: ( Show a, Applicative m) => Tracer m a
debugTracer = Tracer traceShowM

sayTracer :: ( Show a, MonadSay m) => Tracer m a
sayTracer = Tracer (say . show)

