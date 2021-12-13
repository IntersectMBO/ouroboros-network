{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Util.Orphans.IOLike () where

import           Control.Monad.IOSim
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.IOLike.MonadTimeOnlyForTracer
import           Test.Util.Orphans.NoThunks ()

instance IOLike (IOSim s) where
  forgetSignKeyKES = const $ return ()

instance MonadTimeOnlyForTracer (IOSim s) where
  getCurrentTimeOnlyForTracer = defaultgetCurrentTimeOnlyForTracer
