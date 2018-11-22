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

module Ouroboros.Consensus.Protocol.ExtNodeConfig (
    ExtNodeConfig
    -- * Type family instances
  , NodeConfig(..)
  , Payload(..)
  ) where

import           GHC.Generics (Generic)

import           Ouroboros.Network.Serialise

import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util

-- | Extension of protocol @p@ by additional static node configuration @cfg@.
data ExtNodeConfig cfg p

instance OuroborosTag p => OuroborosTag (ExtNodeConfig cfg p) where

  --
  -- Most types remain the same
  --

  newtype Payload (ExtNodeConfig cfg p) ph = EncPayload {
        encPayloadP :: Payload p ph
      }
    deriving (Generic)

  type ChainState     (ExtNodeConfig cfg p) = ChainState     p
  type NodeState      (ExtNodeConfig cfg p) = NodeState      p
  type LedgerView     (ExtNodeConfig cfg p) = LedgerView     p
  type ValidationErr  (ExtNodeConfig cfg p) = ValidationErr  p
  type IsLeader       (ExtNodeConfig cfg p) = IsLeader       p
  type SupportedBlock (ExtNodeConfig cfg p) = SupportedBlock p

  --
  -- Only type that changes is the node config
  --

  data NodeConfig (ExtNodeConfig cfg p) = EncNodeConfig {
        encNodeConfigP   :: NodeConfig p
      , encNodeConfigExt :: cfg
      }

  --
  -- Propagate changes
  --

  mkPayload (EncNodeConfig cfg _) proof ph =
      EncPayload <$> mkPayload cfg proof ph

  selectChain     (EncNodeConfig cfg _) = selectChain     cfg
  checkIsLeader   (EncNodeConfig cfg _) = checkIsLeader   cfg
  applyChainState (EncNodeConfig cfg _) = applyChainState cfg

deriving instance (OuroborosTag p, Eq       ph) => Eq       (Payload (ExtNodeConfig cfg p) ph)
deriving instance (OuroborosTag p, Ord      ph) => Ord      (Payload (ExtNodeConfig cfg p) ph)
deriving instance (OuroborosTag p, Show     ph) => Show     (Payload (ExtNodeConfig cfg p) ph)
deriving instance (OuroborosTag p, Condense ph) => Condense (Payload (ExtNodeConfig cfg p) ph)

instance (OuroborosTag p, Serialise ph) => Serialise (Payload (ExtNodeConfig cfg p) ph) where
  -- use Generic instance
