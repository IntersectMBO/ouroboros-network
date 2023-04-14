{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies   #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- See https://gitlab.haskell.org/ghc/ghc/-/issues/14630. GHC currently warns
-- (erroneously) about name shadowing for record field selectors defined by
-- pattern synonyms.
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Ouroboros.Consensus.Shelley.Protocol.Praos (PraosEnvelopeError (..)) where

import qualified Cardano.Crypto.KES as KES
import           Cardano.Crypto.VRF (certifiedOutput)
import           Cardano.Ledger.BaseTypes (ProtVer (ProtVer), Version)
import           Cardano.Ledger.BHeaderView
import           Cardano.Ledger.Keys (hashKey)
import           Cardano.Ledger.Slot (SlotNo (unSlotNo))
import           Cardano.Protocol.TPraos.OCert
                     (OCert (ocertKESPeriod, ocertVkHot))
import qualified Cardano.Protocol.TPraos.OCert as SL
import           Control.Monad (unless)
import           Control.Monad.Except (throwError)
import           Data.Either (isRight)
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)
import           Numeric.Natural (Natural)
import           Ouroboros.Consensus.Protocol.Praos
import           Ouroboros.Consensus.Protocol.Praos.Common
                     (MaxMajorProtVer (MaxMajorProtVer))
import           Ouroboros.Consensus.Protocol.Praos.Header (Header (..),
                     HeaderBody (..), headerHash, headerSize)
import           Ouroboros.Consensus.Protocol.Praos.Views
import           Ouroboros.Consensus.Protocol.Signed
import           Ouroboros.Consensus.Shelley.Protocol.Abstract (ProtoCrypto,
                     ProtocolHeaderSupportsEnvelope (..),
                     ProtocolHeaderSupportsKES (..),
                     ProtocolHeaderSupportsLedger (..),
                     ProtocolHeaderSupportsProtocol (..),
                     ShelleyHash (ShelleyHash), ShelleyProtocol,
                     ShelleyProtocolHeader)


type instance ProtoCrypto (Praos c) = c

type instance ShelleyProtocolHeader (Praos c) = Header c

data PraosEnvelopeError
  = ObsoleteNode Version Version
  | HeaderSizeTooLarge Natural Natural
  | BlockSizeTooLarge Natural Natural
  deriving (Eq, Generic, Show)

instance NoThunks PraosEnvelopeError

instance PraosCrypto c => ProtocolHeaderSupportsEnvelope (Praos c) where
  pHeaderHash hdr = ShelleyHash $ headerHash hdr
  pHeaderPrevHash (Header body _) = hbPrev body
  pHeaderBodyHash (Header body _) = hbBodyHash body
  pHeaderSlot (Header body _) = hbSlotNo body
  pHeaderBlock (Header body _) = hbBlockNo body
  pHeaderSize hdr = fromIntegral $ headerSize hdr
  pHeaderBlockSize (Header body _) = fromIntegral $ hbBodySize body

  type EnvelopeCheckError _ = PraosEnvelopeError

  envelopeChecks cfg (TickedPraosLedgerView lv) hdr = do
    unless (m <= maxpv) $ throwError (ObsoleteNode m maxpv)
    unless (fromIntegral (bhviewHSize bhv) <= maxHeaderSize) $
      throwError $
        HeaderSizeTooLarge (fromIntegral $ bhviewHSize bhv) maxHeaderSize
    unless (bhviewBSize bhv <= maxBodySize) $
      throwError $
        BlockSizeTooLarge (bhviewBSize bhv) maxBodySize
    where
      pp = praosParams cfg
      (MaxMajorProtVer maxpv) = praosMaxMajorPV pp
      (ProtVer m _) = lvProtocolVersion lv
      maxHeaderSize = lvMaxHeaderSize lv
      maxBodySize = lvMaxBodySize lv
      bhv = mkHeaderView hdr

instance PraosCrypto c => ProtocolHeaderSupportsKES (Praos c) where
  configSlotsPerKESPeriod cfg = praosSlotsPerKESPeriod $ praosParams cfg
  verifyHeaderIntegrity slotsPerKESPeriod header =
    isRight $ KES.verifySignedKES () ocertVkHot t headerBody headerSig
    where
      Header {headerBody, headerSig} = header
      SL.OCert
        { ocertVkHot,
          ocertKESPeriod = SL.KESPeriod startOfKesPeriod
        } = hbOCert headerBody

      currentKesPeriod =
        fromIntegral $
          unSlotNo (hbSlotNo headerBody) `div` slotsPerKESPeriod

      t
        | currentKesPeriod >= startOfKesPeriod =
            currentKesPeriod - startOfKesPeriod
        | otherwise =
            0
  mkHeader hk cbl il slotNo blockNo prevHash bbHash sz protVer = do
    PraosFields {praosSignature, praosToSign} <- forgePraosFields hk cbl il mkBhBodyBytes
    pure $ Header praosToSign praosSignature
    where
      mkBhBodyBytes
        PraosToSign
          { praosToSignIssuerVK,
            praosToSignVrfVK,
            praosToSignVrfRes,
            praosToSignOCert
          } =
          HeaderBody
            { hbBlockNo = blockNo,
              hbSlotNo = slotNo,
              hbPrev = prevHash,
              hbVk = praosToSignIssuerVK,
              hbVrfVk = praosToSignVrfVK,
              hbVrfRes = praosToSignVrfRes,
              hbBodySize = fromIntegral sz,
              hbBodyHash = bbHash,
              hbOCert = praosToSignOCert,
              hbProtVer = protVer
            }

instance PraosCrypto c => ProtocolHeaderSupportsProtocol (Praos c) where
  type CannotForgeError (Praos c) = PraosCannotForge c
  protocolHeaderView Header {headerBody, headerSig} =
    HeaderView
      { hvPrevHash = hbPrev headerBody,
        hvVK = hbVk headerBody,
        hvVrfVK = hbVrfVk headerBody,
        hvVrfRes = hbVrfRes headerBody,
        hvOCert = hbOCert headerBody,
        hvSlotNo = hbSlotNo headerBody,
        hvSigned = headerBody,
        hvSignature = headerSig
      }
  pHeaderIssuer = hbVk . headerBody
  pHeaderIssueNo = SL.ocertN . hbOCert . headerBody

  -- This is the "unified" VRF value, prior to range extension which yields e.g.
  -- the leader VRF value used for slot election.
  --
  -- In the future, we might want to use a dedicated range-extended VRF value
  -- here instead.
  pTieBreakVRFValue = certifiedOutput . hbVrfRes . headerBody

instance PraosCrypto c => ProtocolHeaderSupportsLedger (Praos c) where
  mkHeaderView hdr@Header {headerBody} =
    BHeaderView
      { bhviewID = hashKey $ hbVk headerBody,
        bhviewBSize = fromIntegral $ hbBodySize headerBody,
        bhviewHSize = headerSize hdr,
        bhviewBHash = hbBodyHash headerBody,
        bhviewSlot = hbSlotNo headerBody
      }

type instance Signed (Header c) = HeaderBody c
instance PraosCrypto c => SignedHeader (Header c) where
  headerSigned = headerBody

instance PraosCrypto c => ShelleyProtocol (Praos c)
