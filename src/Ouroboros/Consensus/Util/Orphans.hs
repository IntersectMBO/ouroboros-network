{-# OPTIONS_GHC -fno-warn-orphans #-}
module Ouroboros.Consensus.Util.Orphans () where

import           Ouroboros.Network.Chain (Chain (..))
import           Ouroboros.Network.Node (NodeId (..))

import           Ouroboros.Consensus.Util


instance Condense NodeId where
  condense (CoreId  i) = "c" ++ show i
  condense (RelayId i) = "r" ++ show i

instance Condense block => Condense (Chain block) where
    condense Genesis   = "Genesis"
    condense (cs :> b) = condense cs <> " :> " <> condense b
