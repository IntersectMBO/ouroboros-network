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
  , OuroborosNodeConfig(..)
  , OuroborosPayload(..)
  ) where

import           GHC.Generics (Generic)

import           Ouroboros.Network.Serialise

import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util

-- | Extension of protocol @p@ by additional static node configuration data
-- @config@.
data ExtNodeConfig cfg p

instance OuroborosTag p => OuroborosTag (ExtNodeConfig cfg p) where

  --
  -- Most types remain the same
  --

  newtype OuroborosPayload (ExtNodeConfig cfg p) ph = EncOuroborosPayload {
        encPayloadP :: OuroborosPayload p ph
      }
    deriving (Generic)

  type OuroborosChainState      (ExtNodeConfig cfg p)    = OuroborosChainState      p
  type OuroborosNodeState       (ExtNodeConfig cfg p)    = OuroborosNodeState       p
  type OuroborosLedgerView      (ExtNodeConfig cfg p)    = OuroborosLedgerView      p
  type OuroborosValidationError (ExtNodeConfig cfg p)    = OuroborosValidationError p
  type ProofIsLeader            (ExtNodeConfig cfg p)    = ProofIsLeader            p
  type SupportedBlock           (ExtNodeConfig cfg p)    = SupportedBlock           p

  --
  -- Only type that changes is the node config
  --

  data OuroborosNodeConfig (ExtNodeConfig cfg p) = ENCNodeConfig {
        encNodeConfigP   :: OuroborosNodeConfig p
      , encNodeConfigExt :: cfg
      }

  --
  -- Propagate changes
  --

  mkOuroborosPayload cfg proof ph =
      EncOuroborosPayload <$>
        mkOuroborosPayload
          (encNodeConfigP cfg)
          proof
          ph

  selectChain = selectChain . encNodeConfigP

  checkIsLeader cfg slot l cs =
      checkIsLeader
        (encNodeConfigP cfg)
        slot
        l
        cs

  applyOuroborosChainState cfg b l cs =
      applyOuroborosChainState
        (encNodeConfigP cfg)
        b
        l
        cs

deriving instance (OuroborosTag p, Eq       ph) => Eq       (OuroborosPayload (ExtNodeConfig cfg p) ph)
deriving instance (OuroborosTag p, Show     ph) => Show     (OuroborosPayload (ExtNodeConfig cfg p) ph)
deriving instance (OuroborosTag p, Condense ph) => Condense (OuroborosPayload (ExtNodeConfig cfg p) ph)

instance (OuroborosTag p, Serialise ph) => Serialise (OuroborosPayload (ExtNodeConfig cfg p) ph) where
  -- use Generic instance
