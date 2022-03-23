{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Shelley.Ledger.TPraos () where

import           Cardano.Crypto.VRF (certifiedOutput)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Protocol.Signed

import qualified Cardano.Protocol.TPraos.BHeader as SL
import qualified Cardano.Protocol.TPraos.OCert as SL

import qualified Cardano.Protocol.TPraos.API as SL
import           Ouroboros.Consensus.Protocol.TPraos
import           Ouroboros.Consensus.Shelley.Eras (EraCrypto)
import           Ouroboros.Consensus.Shelley.Ledger.Block
import           Ouroboros.Consensus.Shelley.Ledger.Config ()

{-------------------------------------------------------------------------------
  Support for Transitional Praos consensus algorithm
-------------------------------------------------------------------------------}

type instance BlockProtocol (ShelleyBlock era) = TPraos (EraCrypto era)

instance (SL.PraosCrypto (EraCrypto era), ShelleyBasedEra era)
  => BlockSupportsProtocol (ShelleyBlock era) where
  validateView _cfg (ShelleyHeader hdr _) = hdr

  selectView _ hdr@(ShelleyHeader shdr _) = TPraosChainSelectView {
        csvChainLength = blockNo hdr
      , csvSlotNo      = blockSlot hdr
      , csvIssuer      = SL.bheaderVk hdrBody
      , csvIssueNo     = SL.ocertN . SL.bheaderOCert $ hdrBody
      , csvLeaderVRF   = certifiedOutput . SL.bheaderL $ hdrBody
      }
    where
      hdrBody :: SL.BHBody (EraCrypto era)
      hdrBody = SL.bhbody shdr

-- TODO correct place for these two?
type instance Signed (Header (ShelleyBlock era)) = SL.BHBody (EraCrypto era)

instance ShelleyBasedEra era => SignedHeader (Header (ShelleyBlock era)) where
  headerSigned = SL.bhbody . shelleyHeaderRaw
