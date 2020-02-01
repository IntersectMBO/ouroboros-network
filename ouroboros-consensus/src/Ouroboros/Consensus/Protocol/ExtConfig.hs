{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module Ouroboros.Consensus.Protocol.ExtConfig (
    ExtConfig
    -- * Type families
  , NodeConfig(..)
  ) where

import           Data.Typeable
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks (..))

import           Ouroboros.Consensus.Protocol.Abstract

data ExtConfig p ext

data instance NodeConfig (ExtConfig p ext) = ExtNodeConfig {
      extNodeConfig  :: ext
    , extNodeConfigP :: NodeConfig p
    }
  deriving (Generic)

instance ( NoUnexpectedThunks (NodeConfig p)
         , NoUnexpectedThunks ext
         )
      => NoUnexpectedThunks (NodeConfig (ExtConfig p ext)) where
  -- Use generic instance

instance ( OuroborosTag p
         , Typeable ext
         , NoUnexpectedThunks ext
         ) => OuroborosTag (ExtConfig p ext) where
  type NodeState     (ExtConfig p ext) = NodeState     p
  type ChainState    (ExtConfig p ext) = ChainState    p
  type IsLeader      (ExtConfig p ext) = IsLeader      p
  type LedgerView    (ExtConfig p ext) = LedgerView    p
  type ValidationErr (ExtConfig p ext) = ValidationErr p
  type ValidateView  (ExtConfig p ext) = ValidateView  p
  type SelectView    (ExtConfig p ext) = SelectView    p

  preferCandidate       ExtNodeConfig{..} = preferCandidate       extNodeConfigP
  compareCandidates     ExtNodeConfig{..} = compareCandidates     extNodeConfigP
  checkIsLeader         ExtNodeConfig{..} = checkIsLeader         extNodeConfigP
  applyChainState       ExtNodeConfig{..} = applyChainState       extNodeConfigP
  protocolSecurityParam ExtNodeConfig{..} = protocolSecurityParam extNodeConfigP
  protocolSlotLengths   ExtNodeConfig{..} = protocolSlotLengths   extNodeConfigP
  rewindChainState      ExtNodeConfig{..} = rewindChainState      extNodeConfigP
