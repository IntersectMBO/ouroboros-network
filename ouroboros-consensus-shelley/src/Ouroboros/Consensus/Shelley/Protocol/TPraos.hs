{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Shelley.Protocol.TPraos () where

import qualified Cardano.Crypto.KES as SL
import           Cardano.Crypto.VRF (certifiedOutput)
import           Cardano.Ledger.Chain (ChainPredicateFailure)
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Protocol.TPraos.API as SL
import qualified Cardano.Protocol.TPraos.BHeader as SL
import           Cardano.Protocol.TPraos.OCert (ocertKESPeriod, ocertVkHot)
import qualified Cardano.Protocol.TPraos.OCert as SL
import           Cardano.Slotting.Slot (unSlotNo)
import           Data.Either (isRight)
import           Ouroboros.Consensus.Protocol.Signed (Signed,
                     SignedHeader (headerSigned))
import           Ouroboros.Consensus.Protocol.TPraos
                     (MaxMajorProtVer (MaxMajorProtVer), PraosCrypto, TPraos,
                     TPraosCannotForge, TPraosFields (..), TPraosToSign (..),
                     Ticked (TickedPraosLedgerView), forgeTPraosFields,
                     tpraosMaxMajorPV, tpraosParams, tpraosSlotsPerKESPeriod)
import           Ouroboros.Consensus.Shelley.Protocol.Abstract (ProtoCrypto,
                     ProtocolHeaderSupportsEnvelope (..),
                     ProtocolHeaderSupportsKES (..),
                     ProtocolHeaderSupportsLedger (..),
                     ProtocolHeaderSupportsProtocol (..),
                     ShelleyProtocol, ShelleyProtocolHeader,
                     pattern ShelleyHash, protocolHeaderView)

type instance ProtoCrypto (TPraos c) = c

type instance ShelleyProtocolHeader (TPraos c) = SL.BHeader c

instance PraosCrypto c => ProtocolHeaderSupportsEnvelope (TPraos c) where
  pHeaderHash = ShelleyHash . SL.unHashHeader . SL.bhHash
  pHeaderPrevHash = SL.bheaderPrev . SL.bhbody
  pHeaderBodyHash = SL.bhash . SL.bhbody
  pHeaderSlot = SL.bheaderSlotNo . SL.bhbody
  pHeaderBlock = SL.bheaderBlockNo . SL.bhbody
  pHeaderSize = fromIntegral . SL.bHeaderSize
  pHeaderBlockSize = SL.bsize . SL.bhbody

  type EnvelopeCheckError _ = ChainPredicateFailure

  envelopeChecks cfg (TickedPraosLedgerView lv) hdr =
    SL.chainChecks
      maxPV
      (SL.lvChainChecks lv)
      (SL.makeHeaderView $ protocolHeaderView @(TPraos c) hdr)
    where
      MaxMajorProtVer maxPV = tpraosMaxMajorPV $ tpraosParams cfg

instance PraosCrypto c => ProtocolHeaderSupportsKES (TPraos c) where
  configSlotsPerKESPeriod cfg = tpraosSlotsPerKESPeriod $ tpraosParams cfg
  verifyHeaderIntegrity slotsPerKESPeriod hdr =
    isRight $ SL.verifySignedKES () ocertVkHot t hdrBody hdrSignature
    where
      SL.BHeader hdrBody hdrSignature = hdr
      SL.OCert
        { ocertVkHot,
          ocertKESPeriod = SL.KESPeriod startOfKesPeriod
        } = SL.bheaderOCert hdrBody

      currentKesPeriod =
        fromIntegral $
          unSlotNo (SL.bheaderSlotNo $ SL.bhbody hdr) `div` slotsPerKESPeriod

      t
        | currentKesPeriod >= startOfKesPeriod =
          currentKesPeriod - startOfKesPeriod
        | otherwise =
          0
  mkHeader hotKey canBeLeader isLeader curSlot curNo prevHash bbHash actualBodySize protVer = do
    TPraosFields {tpraosSignature, tpraosToSign} <-
      forgeTPraosFields hotKey canBeLeader isLeader mkBhBody
    pure $ SL.BHeader tpraosToSign tpraosSignature
    where
      mkBhBody toSign =
        SL.BHBody
          { SL.bheaderPrev = prevHash,
            SL.bheaderVk = tpraosToSignIssuerVK,
            SL.bheaderVrfVk = tpraosToSignVrfVK,
            SL.bheaderSlotNo = curSlot,
            SL.bheaderBlockNo = curNo,
            SL.bheaderEta = tpraosToSignEta,
            SL.bheaderL = tpraosToSignLeader,
            SL.bsize = fromIntegral actualBodySize,
            SL.bhash = bbHash,
            SL.bheaderOCert = tpraosToSignOCert,
            SL.bprotver = protVer
          }
        where
          TPraosToSign
            { tpraosToSignIssuerVK,
              tpraosToSignVrfVK,
              tpraosToSignEta,
              tpraosToSignLeader,
              tpraosToSignOCert
            } = toSign

instance PraosCrypto c => ProtocolHeaderSupportsProtocol (TPraos c) where
  type CannotForgeError (TPraos c) = TPraosCannotForge c

  protocolHeaderView = id
  pHeaderIssuer = SL.bheaderVk . SL.bhbody
  pHeaderIssueNo = SL.ocertN . SL.bheaderOCert . SL.bhbody

  -- As this is the leader VRF value, which is used for slot election in the
  -- first place, it gives an advantage to smaller pools in a multi-leader slot.
  -- This was not an intentional decision, see
  -- https://github.com/input-output-hk/ouroboros-network/issues/4051 for a more
  -- detailed discussion.
  pTieBreakVRFValue = certifiedOutput . SL.bheaderL . SL.bhbody

instance PraosCrypto c => ProtocolHeaderSupportsLedger (TPraos c) where
  mkHeaderView = SL.makeHeaderView

type instance Signed (SL.BHeader c) = SL.BHBody c

instance PraosCrypto c => SignedHeader (SL.BHeader c) where
  headerSigned = SL.bhbody

instance PraosCrypto c => ShelleyProtocol (TPraos c)
