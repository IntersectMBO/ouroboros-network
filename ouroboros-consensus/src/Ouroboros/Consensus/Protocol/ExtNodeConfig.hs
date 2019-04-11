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

import           Codec.Serialise (Serialise)
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)

import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util.Condense

-- | Extension of protocol @p@ by additional static node configuration @cfg@.
data ExtNodeConfig cfg p

instance (Typeable cfg, OuroborosTag p) => OuroborosTag (ExtNodeConfig cfg p) where

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

  preferCandidate       (EncNodeConfig cfg _) = preferCandidate       cfg
  compareCandidates     (EncNodeConfig cfg _) = compareCandidates     cfg
  checkIsLeader         (EncNodeConfig cfg _) = checkIsLeader         cfg
  applyChainState       (EncNodeConfig cfg _) = applyChainState       cfg
  protocolSecurityParam (EncNodeConfig cfg _) = protocolSecurityParam cfg

deriving instance Eq       (Payload p ph) => Eq       (Payload (ExtNodeConfig cfg p) ph)
deriving instance Ord      (Payload p ph) => Ord      (Payload (ExtNodeConfig cfg p) ph)
deriving instance Show     (Payload p ph) => Show     (Payload (ExtNodeConfig cfg p) ph)
deriving instance Condense (Payload p ph) => Condense (Payload (ExtNodeConfig cfg p) ph)

instance Serialise (Payload p ph) => Serialise (Payload (ExtNodeConfig cfg p) ph) where
  -- use Generic instance
