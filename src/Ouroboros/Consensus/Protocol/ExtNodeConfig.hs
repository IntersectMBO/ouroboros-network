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
  , OuroborosChainState(..)
  , OuroborosLedgerView(..)
  , OuroborosNodeConfig(..)
  , OuroborosNodeState(..)
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
  -- Most types don't change
  --

  newtype OuroborosPayload (ExtNodeConfig cfg p) ph = ENCPayload {
        encPayloadP :: OuroborosPayload p ph
      }
    deriving (Generic)

  newtype OuroborosChainState (ExtNodeConfig cfg p) = ENCChainState {
        encChainStateP :: OuroborosChainState p
      }

  newtype OuroborosNodeState (ExtNodeConfig cfg p) = ENCNodeState {
        encNodeStateP :: OuroborosNodeState p
      }

  newtype ProofIsLeader (ExtNodeConfig cfg p) = ENCProofIsLeader {
        encProofIsLeaderP :: ProofIsLeader p
      }

  newtype OuroborosLedgerView (ExtNodeConfig cfg p) = ENCLedgerView {
        encLedgerViewP :: OuroborosLedgerView p
      }

  type OuroborosValidationError (ExtNodeConfig cfg p) = OuroborosValidationError p
  type SupportedBlock           (ExtNodeConfig cfg p) = SupportedBlock           p

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

  mkOuroborosPayload cfg proof ph = liftState $
      ENCPayload <$>
        mkOuroborosPayload
          (encNodeConfigP cfg)
          (encProofIsLeaderP proof)
          ph

  selectChain = selectChain . encNodeConfigP

  checkIsLeader cfg slot l cs = liftState $
    fmap ENCProofIsLeader <$>
      checkIsLeader
        (encNodeConfigP cfg)
        slot
        (encLedgerViewP l)
        (encChainStateP cs)

  applyOuroborosChainState cfg b l cs =
    ENCChainState <$>
      applyOuroborosChainState
        (encNodeConfigP cfg)
        b
        (encLedgerViewP l)
        (encChainStateP cs)

deriving instance OuroborosTag p => Show (OuroborosChainState (ExtNodeConfig cfg p))
deriving instance (OuroborosTag p, Eq ph) => Eq (OuroborosPayload (ExtNodeConfig cfg p) ph)
deriving instance (OuroborosTag p, Show ph) => Show (OuroborosPayload (ExtNodeConfig cfg p) ph)

deriving instance (OuroborosTag p, Condense ph) => Condense (OuroborosPayload (ExtNodeConfig cfg p) ph)

instance (OuroborosTag p, Serialise ph) => Serialise (OuroborosPayload (ExtNodeConfig cfg p) ph) where
  -- use Generic instance
