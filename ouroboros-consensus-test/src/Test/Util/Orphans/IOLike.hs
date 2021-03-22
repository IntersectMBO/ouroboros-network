{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Util.Orphans.IOLike (

  ) where

import           Control.Monad.IOSim
import           Ouroboros.Consensus.Util.IOLike
import           Test.Util.Orphans.NoThunks ()

instance IOLike (IOSim s) where
  forgetSignKeyKES = const $ return ()
