{-# OPTIONS_GHC -fno-warn-orphans #-}
module Ouroboros.Consensus.Util.Orphans () where

import           Ouroboros.Consensus.Util
import           Ouroboros.Network.Node (NodeId (..))

instance Condense NodeId where
  condense (CoreId  i) = "c" ++ show i
  condense (RelayId i) = "r" ++ show i
