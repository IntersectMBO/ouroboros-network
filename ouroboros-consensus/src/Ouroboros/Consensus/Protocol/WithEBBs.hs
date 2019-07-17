{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE UndecidableSuperClasses    #-}
{-# LANGUAGE ViewPatterns               #-}

module Ouroboros.Consensus.Protocol.WithEBBs
  ( WithEBBs
  , HeaderSupportsWithEBB(..)
  , NodeConfig(..)
  )
where

import Ouroboros.Consensus.Protocol.Abstract

class ( SupportedHeader p (HeaderOfBlock hoe)
      ) => HeaderSupportsWithEBB p hoe where

  type HeaderOfBlock hoe :: *
  type HeaderOfEBB hoe :: *

  eitherHeaderOrEbb :: proxy p -> hoe -> Either (HeaderOfEBB hoe) (HeaderOfBlock hoe)

-- | Extension of protocol @p@ to support addition 'epoch boundary blocks'; that
-- is, blocks which do not contribute to the general running of the protocol,
-- but do add to the hash chain.
--
-- This is entirely a legacy concern, and should go away after the Byron era.
data WithEBBs p

instance (OuroborosTag p) => OuroborosTag (WithEBBs p) where
  --
  -- Most types remain the same
  --

  type ChainState      (WithEBBs p) = ChainState     p
  type NodeState       (WithEBBs p) = NodeState      p
  type LedgerView      (WithEBBs p) = LedgerView     p
  type ValidationErr   (WithEBBs p) = ValidationErr  p
  type IsLeader        (WithEBBs p) = IsLeader       p

  type SupportedHeader (WithEBBs p) = HeaderSupportsWithEBB p

  newtype NodeConfig   (WithEBBs p) = WithEBBNodeConfig {unWithEBBNodeConfig :: NodeConfig p}

  preferCandidate       (WithEBBNodeConfig cfg) = preferCandidate       cfg
  compareCandidates     (WithEBBNodeConfig cfg) = compareCandidates     cfg
  checkIsLeader         (WithEBBNodeConfig cfg) = checkIsLeader         cfg
  rewindChainState      (WithEBBNodeConfig cfg) = rewindChainState      cfg
  protocolSecurityParam (WithEBBNodeConfig cfg) = protocolSecurityParam cfg

  applyChainState (WithEBBNodeConfig cfg) lv b cs = case
    eitherHeaderOrEbb cfg b of
      Right hdr -> applyChainState @p cfg lv hdr cs
      -- Currently the hash chain is maintained in the ledger state, so we just
      -- ignore the EBB for protocol concerns.
      Left _ -> return cs
