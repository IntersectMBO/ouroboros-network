{-# LANGUAGE ConstraintKinds            #-}
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
  , NodeState
  , ChainState
  , IsLeader
  , LedgerView
  , ValidationErr
  , SupportedBlock
  ) where

import           Data.Proxy (Proxy (..))
import           Data.Typeable (Typeable)

import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import           Ouroboros.Network.Block (HasHeader)

import           Ouroboros.Consensus.Protocol.Abstract

class OuroborosTag p => ChainSelection p s where
  preferCandidate' :: HasHeader b
                   => proxy s
                   -> NodeConfig p
                   -> AnchoredFragment b      -- ^ Our chain
                   -> AnchoredFragment b      -- ^ Candidate
                   -> Bool
  compareCandidates' :: HasHeader b
                     => proxy s
                     -> NodeConfig p
                     -> AnchoredFragment b -> AnchoredFragment b -> Ordering

data ModChainSel p s

instance (Typeable p, Typeable s, ChainSelection p s) => OuroborosTag (ModChainSel p s) where

    newtype NodeConfig     (ModChainSel p s)    = McsNodeConfig (NodeConfig p)
    type    NodeState      (ModChainSel p s)    = NodeState p
    type    ChainState     (ModChainSel p s)    = ChainState p
    type    IsLeader       (ModChainSel p s)    = IsLeader p
    type    LedgerView     (ModChainSel p s)    = LedgerView p
    type    ValidationErr  (ModChainSel p s)    = ValidationErr p
    type    SupportedBlock (ModChainSel p s)    = SupportedBlock p

    checkIsLeader         (McsNodeConfig cfg) = checkIsLeader         cfg
    applyChainState       (McsNodeConfig cfg) = applyChainState       cfg
    rewindChainState      (McsNodeConfig cfg) = rewindChainState      cfg
    protocolSecurityParam (McsNodeConfig cfg) = protocolSecurityParam cfg

    preferCandidate   (McsNodeConfig cfg) = preferCandidate'   (Proxy :: Proxy s) cfg
    compareCandidates (McsNodeConfig cfg) = compareCandidates' (Proxy :: Proxy s) cfg
