{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- TODO where to put this?
module Ouroboros.Consensus.Shelley.Ledger.TPraos () where

import           Cardano.Crypto.VRF (certifiedOutput)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Protocol.Signed

import qualified Shelley.Spec.Ledger.API as SL
import qualified Shelley.Spec.Ledger.BlockChain as SL

import           Ouroboros.Consensus.Shelley.Ledger.Block
import           Ouroboros.Consensus.Shelley.Ledger.Config
import           Ouroboros.Consensus.Shelley.Protocol

{-------------------------------------------------------------------------------
  Support for Transitional Praos consensus algorithm
-------------------------------------------------------------------------------}

type instance BlockProtocol (ShelleyBlock era) = TPraos era

instance TPraosCrypto era => BlockSupportsProtocol (ShelleyBlock era) where
  validateView _cfg (ShelleyHeader hdr _) = hdr

  selectView cfg hdr@(ShelleyHeader shdr _) = TPraosChainSelectView {
        csvChainLength = blockNo hdr
      , csvSlotNo      = blockSlot hdr
      , csvSelfIssued  = selfIssued
      , csvIssuer      = SL.bheaderVk hdrBody
      , csvIssueNo     = SL.ocertN . SL.bheaderOCert $ hdrBody
      , csvLeaderVRF   = certifiedOutput . SL.bheaderL $ hdrBody
      }
    where
      hdrBody :: SL.BHBody era
      hdrBody = SL.bhbody shdr

      selfIssued :: SelfIssued
      selfIssued = case shelleyBlockIssuerVKey cfg of
        NotABlockIssuer
          -> NotSelfIssued
        BlockIssuerVKey vkey
          | vkey == SL.bheaderVk hdrBody
          -> SelfIssued
          | otherwise
          -> NotSelfIssued

-- TODO correct place for these two?
type instance Signed (Header (ShelleyBlock era)) = SL.BHBody era

instance Era era => SignedHeader (Header (ShelleyBlock era)) where
  headerSigned = SL.bhbody . shelleyHeaderRaw
