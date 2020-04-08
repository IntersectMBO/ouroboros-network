{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

module Ouroboros.Consensus.Protocol.ModChainSel (
    ChainSelection (..)
  , ModChainSel
    -- * Type family instances
  , ConsensusConfig (..)
  ) where

import           Data.Proxy (Proxy (..))
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Consensus.Protocol.Abstract

-- | Redefine the chain selection part of 'ConsensusProtocol'
class ConsensusProtocol p => ChainSelection p s where
  type family SelectView' p :: *

  preferCandidate'   :: proxy s
                     -> ConsensusConfig p
                     -> SelectView'     p
                     -> SelectView'     p
                     -> Bool

  compareCandidates' :: proxy s
                     -> ConsensusConfig p
                     -> SelectView'     p
                     -> SelectView'     p
                     -> Ordering

data ModChainSel p s

newtype instance ConsensusConfig (ModChainSel p s) = McsConsensusConfig {
      unMcsConsensusConfig :: ConsensusConfig p
    }
  deriving (Generic)

instance (Typeable p, Typeable s, ChainSelection p s)
      => ConsensusProtocol (ModChainSel p s) where
    type ConsensusState (ModChainSel p s) = ConsensusState p
    type IsLeader       (ModChainSel p s) = IsLeader       p
    type LedgerView     (ModChainSel p s) = LedgerView     p
    type ValidationErr  (ModChainSel p s) = ValidationErr  p
    type ValidateView   (ModChainSel p s) = ValidateView   p
    type SelectView     (ModChainSel p s) = SelectView'    p

    checkIfCanBeLeader    (McsConsensusConfig cfg) = checkIfCanBeLeader    cfg
    checkIsLeader         (McsConsensusConfig cfg) = checkIsLeader         cfg
    updateConsensusState  (McsConsensusConfig cfg) = updateConsensusState  cfg
    rewindConsensusState  (McsConsensusConfig cfg) = rewindConsensusState  cfg
    protocolSecurityParam (McsConsensusConfig cfg) = protocolSecurityParam cfg

    preferCandidate   (McsConsensusConfig cfg) = preferCandidate'   (Proxy :: Proxy s) cfg
    compareCandidates (McsConsensusConfig cfg) = compareCandidates' (Proxy :: Proxy s) cfg

instance ConsensusProtocol p
      => NoUnexpectedThunks (ConsensusConfig (ModChainSel p s))
  -- use generic instance
