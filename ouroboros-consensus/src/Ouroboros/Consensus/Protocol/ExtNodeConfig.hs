{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Ouroboros.Consensus.Protocol.ExtNodeConfig (
    ExtNodeConfig
    -- * Type family instances
  , NodeConfig(..)
  , mapExtNodeConfig
  ) where

import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Consensus.Protocol.Abstract

-- | Extension of protocol @p@ by additional static node configuration @cfg@.
data ExtNodeConfig cfg p

data instance NodeConfig (ExtNodeConfig cfg p) = EncNodeConfig {
      encNodeConfigP   :: NodeConfig p
    , encNodeConfigExt :: cfg
    }
  deriving (Generic)

instance ( Typeable cfg
         , OuroborosTag p
         , NoUnexpectedThunks cfg
         ) => OuroborosTag (ExtNodeConfig cfg p) where

  --
  -- Most types remain the same
  --

  type ChainState      (ExtNodeConfig cfg p) = ChainState      p
  type NodeState       (ExtNodeConfig cfg p) = NodeState       p
  type LedgerView      (ExtNodeConfig cfg p) = LedgerView      p
  type ValidationErr   (ExtNodeConfig cfg p) = ValidationErr   p
  type IsLeader        (ExtNodeConfig cfg p) = IsLeader        p
  type SupportedHeader (ExtNodeConfig cfg p) = SupportedHeader p

  --
  -- Propagate changes
  --

  preferCandidate       (EncNodeConfig cfg _) = preferCandidate       cfg
  compareCandidates     (EncNodeConfig cfg _) = compareCandidates     cfg
  checkIsLeader         (EncNodeConfig cfg _) = checkIsLeader         cfg
  applyChainState       (EncNodeConfig cfg _) = applyChainState       cfg
  rewindChainState      (EncNodeConfig cfg _) = rewindChainState      cfg
  protocolSecurityParam (EncNodeConfig cfg _) = protocolSecurityParam cfg

instance (OuroborosTag p, NoUnexpectedThunks cfg)
      => NoUnexpectedThunks (NodeConfig (ExtNodeConfig cfg p))
  -- use generic instance

mapExtNodeConfig :: (a -> b)
                 -> NodeConfig (ExtNodeConfig a p)
                 -> NodeConfig (ExtNodeConfig b p)
mapExtNodeConfig f EncNodeConfig{..} = EncNodeConfig {
      encNodeConfigP   =   encNodeConfigP
    , encNodeConfigExt = f encNodeConfigExt
    }
