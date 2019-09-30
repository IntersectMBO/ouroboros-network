{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Util.Orphans.IOLike () where

import           Control.Monad.IOSim
import           Ouroboros.Consensus.Util.IOLike

instance IOLike (SimM s)
