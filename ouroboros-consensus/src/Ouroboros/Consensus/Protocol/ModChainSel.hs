{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

module Ouroboros.Consensus.Protocol.ModChainSel (
    ModChainSel
    -- * Type family instances
  , ConsensusConfig (..)
  ) where

import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)
import           Ouroboros.Consensus.Protocol.Abstract

data ModChainSel p s

newtype instance ConsensusConfig (ModChainSel p s) = McsConsensusConfig {
      mcsConfigP :: ConsensusConfig p
    }
  deriving (Generic)

instance ( ConsensusProtocol p
         , Ord  s
         , Show s
         , Typeable s
         , NoThunks s
         ) => ConsensusProtocol (ModChainSel p s) where
    type SelectView    (ModChainSel p s) = s

    type ChainDepState (ModChainSel p s) = ChainDepState p
    type IsLeader      (ModChainSel p s) = IsLeader      p
    type CanBeLeader   (ModChainSel p s) = CanBeLeader   p
    type LedgerView    (ModChainSel p s) = LedgerView    p
    type ValidationErr (ModChainSel p s) = ValidationErr p
    type ValidateView  (ModChainSel p s) = ValidateView  p

    checkIsLeader         = checkIsLeader         . mcsConfigP
    tickChainDepState     = tickChainDepState     . mcsConfigP
    updateChainDepState   = updateChainDepState   . mcsConfigP
    reupdateChainDepState = reupdateChainDepState . mcsConfigP
    protocolSecurityParam = protocolSecurityParam . mcsConfigP

instance ConsensusProtocol p => NoThunks (ConsensusConfig (ModChainSel p s))
