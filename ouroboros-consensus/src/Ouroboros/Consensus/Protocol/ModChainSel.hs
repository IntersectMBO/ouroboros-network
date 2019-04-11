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
{-# LANGUAGE UndecidableInstances       #-}

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

import           Codec.Serialise (Serialise)
import           Data.Proxy (Proxy (..))
import           Data.Typeable (Typeable)

import           Ouroboros.Network.Block (HasHeader)
import           Ouroboros.Network.Chain (Chain)

import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util.Condense

class OuroborosTag p => ChainSelection p s where

  preferCandidate' :: HasHeader b
                   => proxy s
                   -> NodeConfig p
                   -> Chain b      -- ^ Our chain
                   -> Chain b      -- ^ Candidate
                   -> Bool

  compareCandidates' :: HasHeader b
                     => proxy s
                     -> NodeConfig p
                     -> Chain b -> Chain b -> Ordering

data ModChainSel p s

instance (Typeable p, Typeable s, ChainSelection p s) => OuroborosTag (ModChainSel p s) where

    newtype NodeConfig     (ModChainSel p s)    = McsNodeConfig (NodeConfig p)
    newtype Payload        (ModChainSel p s) ph = McsPayload (Payload p ph)

    type    NodeState      (ModChainSel p s)    = NodeState p
    type    ChainState     (ModChainSel p s)    = ChainState p
    type    IsLeader       (ModChainSel p s)    = IsLeader p
    type    LedgerView     (ModChainSel p s)    = LedgerView p
    type    ValidationErr  (ModChainSel p s)    = ValidationErr p
    type    SupportedBlock (ModChainSel p s)    = SupportedBlock p

    mkPayload toEnc        (McsNodeConfig cfg) proof ph = McsPayload <$> mkPayload toEnc cfg proof ph

    checkIsLeader         (McsNodeConfig cfg) = checkIsLeader         cfg
    applyChainState toEnc (McsNodeConfig cfg) = applyChainState toEnc cfg
    protocolSecurityParam (McsNodeConfig cfg) = protocolSecurityParam cfg

    preferCandidate   (McsNodeConfig cfg) = preferCandidate'   (Proxy :: Proxy s) cfg
    compareCandidates (McsNodeConfig cfg) = compareCandidates' (Proxy :: Proxy s) cfg

deriving instance Show      (Payload p ph) => Show      (Payload (ModChainSel p s) ph)
deriving instance Eq        (Payload p ph) => Eq        (Payload (ModChainSel p s) ph)
deriving instance Ord       (Payload p ph) => Ord       (Payload (ModChainSel p s) ph)
deriving instance Condense  (Payload p ph) => Condense  (Payload (ModChainSel p s) ph)
deriving instance Serialise (Payload p ph) => Serialise (Payload (ModChainSel p s) ph)
