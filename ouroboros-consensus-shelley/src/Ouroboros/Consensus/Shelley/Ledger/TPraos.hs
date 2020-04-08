{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- TODO where to put this?
module Ouroboros.Consensus.Shelley.Ledger.TPraos () where

import           Ouroboros.Network.Block (blockNo)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Node.State
import           Ouroboros.Consensus.Protocol.Signed

import qualified Shelley.Spec.Ledger.BlockChain as SL

import           Ouroboros.Consensus.Shelley.Ledger.Block
import           Ouroboros.Consensus.Shelley.Ledger.Config ()
import           Ouroboros.Consensus.Shelley.Protocol

{-------------------------------------------------------------------------------
  Support for Transitional Praos consensus algorithm
-------------------------------------------------------------------------------}

type instance NodeState     (ShelleyBlock c) = TPraosNodeState c
type instance BlockProtocol (ShelleyBlock c) = TPraos c

instance TPraosCrypto c => BlockSupportsProtocol (ShelleyBlock c) where
  validateView _cfg (ShelleyHeader hdr _) = hdr

  -- TODO check the certificate number
  selectView _ hdr = blockNo hdr

-- TODO correct place for these two?
type instance Signed (Header (ShelleyBlock c)) = SL.BHBody c

instance Crypto c => SignedHeader (Header (ShelleyBlock c)) where
  headerSigned = SL.bhbody . shelleyHeaderRaw
