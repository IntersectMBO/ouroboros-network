{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- TODO where to put this?
module Ouroboros.Consensus.Shelley.Ledger.TPraos () where

import           Cardano.Crypto.VRF.Class (certifiedOutput)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Protocol.Signed
import           Ouroboros.Consensus.Shelley.Ledger.Block
import           Ouroboros.Consensus.Shelley.Ledger.Config ()
import           Ouroboros.Consensus.Shelley.Protocol

import qualified Shelley.Spec.Ledger.BlockChain as SL
import qualified Shelley.Spec.Ledger.OCert as SL

{-------------------------------------------------------------------------------
  Support for Transitional Praos consensus algorithm
-------------------------------------------------------------------------------}

type instance BlockProtocol (ShelleyBlock c) = TPraos c

instance TPraosCrypto c => BlockSupportsProtocol (ShelleyBlock c) where
  validateView _cfg (ShelleyHeader hdr _) = hdr

  selectView _ (ShelleyHeader hdr _) =
    ChainSelectView
      { csvChainLength = SL.bheaderBlockNo . SL.bhbody $ hdr
      , csvLeaderVRF =
              certifiedOutput
            . SL.bheaderL
            . SL.bhbody
            $ hdr
      , csvIssuer = SL.bheaderVk . SL.bhbody $ hdr
      , csvIssueNo = SL.ocertN . SL.bheaderOCert . SL.bhbody $ hdr
      }

-- TODO correct place for these two?
type instance Signed (Header (ShelleyBlock c)) = SL.BHBody c

instance Crypto c => SignedHeader (Header (ShelleyBlock c)) where
  headerSigned = SL.bhbody . shelleyHeaderRaw
