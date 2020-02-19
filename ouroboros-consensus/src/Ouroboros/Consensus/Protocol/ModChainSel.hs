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
  , NodeConfig (..)
  ) where

import           Data.Proxy (Proxy (..))
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Consensus.Protocol.Abstract

-- | Redefine the chain selection part of 'OuroborosTag'
class OuroborosTag p => ChainSelection p s where
  type family SelectView' p :: *

  preferCandidate'   :: proxy s
                     -> NodeConfig p -> SelectView' p -> SelectView' p -> Bool

  compareCandidates' :: proxy s
                     -> NodeConfig p -> SelectView' p -> SelectView' p -> Ordering

data ModChainSel p s

newtype instance NodeConfig (ModChainSel p s) = McsNodeConfig (NodeConfig p)
  deriving (Generic)

instance (Typeable p, Typeable s, ChainSelection p s) => OuroborosTag (ModChainSel p s) where
    type NodeState     (ModChainSel p s) = NodeState     p
    type ChainState    (ModChainSel p s) = ChainState    p
    type IsLeader      (ModChainSel p s) = IsLeader      p
    type LedgerView    (ModChainSel p s) = LedgerView    p
    type ValidationErr (ModChainSel p s) = ValidationErr p
    type ValidateView  (ModChainSel p s) = ValidateView  p
    type SelectView    (ModChainSel p s) = SelectView'   p

    checkIsLeader         (McsNodeConfig cfg) = checkIsLeader         cfg
    applyChainState       (McsNodeConfig cfg) = applyChainState       cfg
    rewindChainState      (McsNodeConfig cfg) = rewindChainState      cfg
    protocolSecurityParam (McsNodeConfig cfg) = protocolSecurityParam cfg

    preferCandidate   (McsNodeConfig cfg) = preferCandidate'   (Proxy :: Proxy s) cfg
    compareCandidates (McsNodeConfig cfg) = compareCandidates' (Proxy :: Proxy s) cfg

instance OuroborosTag p => NoUnexpectedThunks (NodeConfig (ModChainSel p s))
  -- use generic instance
