{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Ouroboros.Consensus.Protocol.ModChainSel (
    ChainSelection (..)
  , ModChainSel
    -- * Type family instances
  , NodeConfig (..)
  , Payload (..)
  , NodeState
  , ChainState
  , IsLeader
  , LedgerView
  , ValidationErr
  , SupportedBlock
  ) where

import           Data.Proxy (Proxy (..))

import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util
import           Ouroboros.Network.Block (Slot, HasHeader)
import           Ouroboros.Network.Chain (Chain)
import           Ouroboros.Network.Serialise

class OuroborosTag (Protocol c) => ChainSelection c where

    type Protocol c :: *

    selectChain' :: (Eq b, HasHeader b, SupportedBlock (Protocol c) b)
                 => proxy c
                 -> NodeConfig (Protocol c)
                 -> Slot
                 -> Chain b
                 -> [Chain b]
                 -> Chain b

data ModChainSel p c

instance ( OuroborosTag p
         , ChainSelection c
         , p ~ Protocol c
         ) => OuroborosTag (ModChainSel p c) where

    newtype NodeConfig (ModChainSel p c) = McsNodeConfig (NodeConfig p)
    newtype Payload (ModChainSel p c) ph = McsPayload (Payload p ph)

    type NodeState (ModChainSel p c) = NodeState p
    type ChainState (ModChainSel p c) = ChainState p
    type IsLeader (ModChainSel p c) = IsLeader p
    type LedgerView (ModChainSel p c) = LedgerView p
    type ValidationErr (ModChainSel p c) = ValidationErr p
    type SupportedBlock (ModChainSel p c) = SupportedBlock p

    mkPayload       (McsNodeConfig cfg) proof ph = McsPayload <$> mkPayload cfg proof ph

    checkIsLeader   (McsNodeConfig cfg) = checkIsLeader   cfg
    applyChainState (McsNodeConfig cfg) = applyChainState cfg

    selectChain     (McsNodeConfig cfg) = selectChain' (Proxy :: Proxy c) cfg

deriving instance (OuroborosTag p, Show      (Payload p ph)) => Show      (Payload (ModChainSel p c) ph)
deriving instance (OuroborosTag p, Eq        (Payload p ph)) => Eq        (Payload (ModChainSel p c) ph)
deriving instance (OuroborosTag p, Ord       (Payload p ph)) => Ord       (Payload (ModChainSel p c) ph)
deriving instance (OuroborosTag p, Condense  (Payload p ph)) => Condense  (Payload (ModChainSel p c) ph)
deriving instance (OuroborosTag p, Serialise (Payload p ph)) => Serialise (Payload (ModChainSel p c) ph)
