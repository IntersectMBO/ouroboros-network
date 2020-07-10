{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

module Ouroboros.Consensus.Protocol.ModChainSel (
    ModChainSel
    -- * Type family instances
  , ConsensusConfig (..)
  ) where

import           Data.Proxy (Proxy (..))
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Consensus.Protocol.Abstract

data ModChainSel p s

data instance ConsensusConfig (ModChainSel p s) = McsConsensusConfig {
      mcsConfigS :: ChainSelConfig  s
    , mcsConfigP :: ConsensusConfig p
    }
  deriving (Generic)

instance ChainSelection s => ChainSelection (ModChainSel p s) where
  type ChainSelConfig (ModChainSel p s) = ChainSelConfig s
  type SelectView     (ModChainSel p s) = SelectView     s

  preferCandidate   _ = preferCandidate   (Proxy @s)
  compareCandidates _ = compareCandidates (Proxy @s)

instance HasChainIndepState p => HasChainIndepState (ModChainSel p s) where
  type ChainIndepStateConfig (ModChainSel p s) = ChainIndepStateConfig p
  type ChainIndepState       (ModChainSel p s) = ChainIndepState       p

  updateChainIndepState _proxy = updateChainIndepState (Proxy @p)

instance (Typeable p, Typeable s, ConsensusProtocol p, ChainSelection s)
      => ConsensusProtocol (ModChainSel p s) where
    type ChainDepState (ModChainSel p s) = ChainDepState   p
    type IsLeader      (ModChainSel p s) = IsLeader        p
    type CanBeLeader   (ModChainSel p s) = CanBeLeader     p
    type CannotLead    (ModChainSel p s) = CannotLead      p
    type LedgerView    (ModChainSel p s) = LedgerView      p
    type ValidationErr (ModChainSel p s) = ValidationErr   p
    type ValidateView  (ModChainSel p s) = ValidateView    p

    checkIsLeader cfg canBeLeader slot chainDepState chainIndepState =
      castLeaderCheck $
        checkIsLeader
          (mcsConfigP cfg)
          canBeLeader
          slot
          chainDepState
          chainIndepState

    tickChainDepState     = tickChainDepState     . mcsConfigP
    updateChainDepState   = updateChainDepState   . mcsConfigP
    protocolSecurityParam = protocolSecurityParam . mcsConfigP

    rewindChainDepState _proxy = rewindChainDepState (Proxy @p)

    chainSelConfig = mcsConfigS

instance (ConsensusProtocol p, ChainSelection s)
      => NoUnexpectedThunks (ConsensusConfig (ModChainSel p s))
