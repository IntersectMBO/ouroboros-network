{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Cardano.Node (
    protocolInfoCardano
  , ProtocolParamsTransition (..)
  , ProtocolParamsAllegra (..)
  , ProtocolParamsMary (..)
  , protocolClientInfoCardano
  , CardanoHardForkConstraints
  , MaxMajorProtVer (..)
  , TriggerHardFork (..)
    -- * SupportedNetworkProtocolVersion
  , pattern CardanoNodeToNodeVersion1
  , pattern CardanoNodeToNodeVersion2
  , pattern CardanoNodeToNodeVersion3
  , pattern CardanoNodeToNodeVersion4
  , pattern CardanoNodeToClientVersion1
  , pattern CardanoNodeToClientVersion2
  , pattern CardanoNodeToClientVersion3
  , pattern CardanoNodeToClientVersion4
  , pattern CardanoNodeToClientVersion5
  , pattern CardanoNodeToClientVersion6
  ) where

import qualified Codec.CBOR.Decoding as CBOR
import           Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as CBOR
import           Control.Exception (assert)
import qualified Data.ByteString.Short as Short
import           Data.Functor.These (These1 (..))
import qualified Data.Map.Strict as Map
import           Data.SOP.Strict hiding (shape, shift)
import           Data.Word (Word16)

import           Cardano.Binary (DecoderError (..), enforceSize)
import           Cardano.Chain.Slotting (EpochSlots)
import           Cardano.Prelude (cborError)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Storage.Serialisation
import           Ouroboros.Consensus.Util.Assert
import           Ouroboros.Consensus.Util.Counting
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.OptNP (OptNP (..))
import qualified Ouroboros.Consensus.Util.OptNP as OptNP
import           Ouroboros.Consensus.Util.SOP (Index (..))

import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.Embed.Nary
import           Ouroboros.Consensus.HardFork.Combinator.Serialisation

import           Ouroboros.Consensus.Byron.Ledger (ByronBlock)
import qualified Ouroboros.Consensus.Byron.Ledger as Byron
import qualified Ouroboros.Consensus.Byron.Ledger.Conversions as Byron
import           Ouroboros.Consensus.Byron.Ledger.NetworkProtocolVersion
import           Ouroboros.Consensus.Byron.Node

import qualified Cardano.Ledger.Era as SL
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)
import qualified Ouroboros.Consensus.Shelley.Ledger as Shelley
import           Ouroboros.Consensus.Shelley.Ledger.NetworkProtocolVersion
import           Ouroboros.Consensus.Shelley.Node
import           Ouroboros.Consensus.Shelley.Protocol (TPraosParams (..))
import qualified Ouroboros.Consensus.Shelley.Protocol as Shelley
import qualified Shelley.Spec.Ledger.API as SL

import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Cardano.CanHardFork
import           Ouroboros.Consensus.Cardano.ShelleyBased

{-------------------------------------------------------------------------------
  SerialiseHFC
-------------------------------------------------------------------------------}

instance SerialiseConstraintsHFC ByronBlock
instance ShelleyBasedEra era => SerialiseConstraintsHFC (ShelleyBlock era)

-- | Important: we need to maintain binary compatibility with Byron blocks, as
-- they are already stored on disk.
--
-- We also want to be able to efficiently detect (without having to peek far
-- ahead) whether we're dealing with a Byron or Shelley block, so that we can
-- invoke the right decoder. We plan to have a few more hard forks after
-- Shelley (Goguen, Basho, Voltaire), so we want a future-proof envelope for
-- distinguishing the different block types, i.e., a byte indicating the era.
--
-- Byron does not provide such an envelope. However, a Byron block is a CBOR
-- 2-tuple with the first element being a tag ('Word': 0 = EBB; 1 = regular
-- block) and the second being the payload. We can easily extend this encoding
-- format with support for Shelley, Goguen, etc.
--
-- We encode a 'CardanoBlock' as the same CBOR 2-tuple as a Byron block, but
-- we use the tags after 1 for the hard forks after Byron:
--
-- 0. Byron EBB
-- 1. Byron regular block
-- 2. Shelley block
-- 3. Allegra block
-- 4. Mary block
-- 5. Goguen block
-- 6. etc.
--
-- For more details, see:
-- <https://github.com/input-output-hk/ouroboros-network/pull/1175#issuecomment-558147194>
instance CardanoHardForkConstraints c => SerialiseHFC (CardanoEras c) where
  encodeDiskHfcBlock (CardanoCodecConfig ccfgByron ccfgShelley ccfgAllegra ccfgMary ccfgAlonzo) = \case
      -- We are backwards compatible with Byron and thus use the exact same
      -- encoding.
      BlockByron   blockByron   ->                encodeDisk ccfgByron blockByron
      -- For Shelley and later eras, we need to prepend the hard fork envelope.
      BlockShelley blockShelley -> prependTag 2 $ encodeDisk ccfgShelley blockShelley
      BlockAllegra blockAllegra -> prependTag 3 $ encodeDisk ccfgAllegra blockAllegra
      BlockMary    blockMary    -> prependTag 4 $ encodeDisk ccfgMary    blockMary
      BlockAlonzo  blockAlonzo  -> prependTag 5 $ encodeDisk ccfgAlonzo  blockAlonzo
  decodeDiskHfcBlock (CardanoCodecConfig ccfgByron ccfgShelley ccfgAllegra ccfgMary ccfgAlonzo) = do
      enforceSize "CardanoBlock" 2
      CBOR.decodeWord >>= \case
        0 -> fmap BlockByron   <$> Byron.decodeByronBoundaryBlock epochSlots
        1 -> fmap BlockByron   <$> Byron.decodeByronRegularBlock  epochSlots
        -- We don't have to drop the first two bytes from the 'ByteString'
        -- passed to the decoder as slicing already takes care of this.
        2 -> fmap BlockShelley <$> decodeDisk ccfgShelley
        3 -> fmap BlockAllegra <$> decodeDisk ccfgAllegra
        4 -> fmap BlockMary    <$> decodeDisk ccfgMary
        5 -> fmap BlockAlonzo  <$> decodeDisk ccfgAlonzo
        t -> cborError $ DecoderErrorUnknownTag "CardanoBlock" (fromIntegral t)
    where
      epochSlots = Byron.getByronEpochSlots ccfgByron

  reconstructHfcPrefixLen _ = PrefixLen 2

  reconstructHfcNestedCtxt _ prefix blockSize =
      case Short.index prefix 1 of
        0 -> SomeSecond $ NestedCtxt (NCZ (Byron.CtxtByronBoundary blockSize))
        1 -> SomeSecond $ NestedCtxt (NCZ (Byron.CtxtByronRegular  blockSize))
        2 -> SomeSecond $ NestedCtxt (NCS (NCZ Shelley.CtxtShelley))
        3 -> SomeSecond $ NestedCtxt (NCS (NCS (NCZ Shelley.CtxtShelley)))
        4 -> SomeSecond $ NestedCtxt (NCS (NCS (NCS (NCZ Shelley.CtxtShelley))))
        5 -> SomeSecond $ NestedCtxt (NCS (NCS (NCS (NCS (NCZ Shelley.CtxtShelley)))))
        _ -> error $ "CardanoBlock: invalid prefix " <> show prefix

  getHfcBinaryBlockInfo = \case
      BlockByron   blockByron   ->
        getBinaryBlockInfo blockByron
      -- For Shelley and the later eras, we need to account for the two extra
      -- bytes of the envelope.
      BlockShelley blockShelley ->
        shiftHeaderOffset 2 $ getBinaryBlockInfo blockShelley
      BlockAllegra blockAllegra ->
        shiftHeaderOffset 2 $ getBinaryBlockInfo blockAllegra
      BlockMary blockMary ->
        shiftHeaderOffset 2 $ getBinaryBlockInfo blockMary
      BlockAlonzo blockAlonzo ->
        shiftHeaderOffset 2 $ getBinaryBlockInfo blockAlonzo
    where
      shiftHeaderOffset :: Word16 -> BinaryBlockInfo -> BinaryBlockInfo
      shiftHeaderOffset shift binfo = binfo {
            headerOffset = headerOffset binfo + shift
          }

  estimateHfcBlockSize = \case
      HeaderByron   headerByron   -> estimateBlockSize headerByron
      -- For Shelley and later eras, we add two extra bytes, see the
      -- 'SerialiseHFC' instance.
      HeaderShelley headerShelley -> estimateBlockSize headerShelley + 2
      HeaderAllegra headerAllegra -> estimateBlockSize headerAllegra + 2
      HeaderMary    headerMary    -> estimateBlockSize headerMary    + 2
      HeaderAlonzo  headerAlonzo  -> estimateBlockSize headerAlonzo  + 2

-- | Prepend the given tag by creating a CBOR 2-tuple with the tag as the
-- first element and the given 'Encoding' as the second.
prependTag :: Word -> Encoding -> Encoding
prependTag tag payload = mconcat [
      CBOR.encodeListLen 2
    , CBOR.encodeWord tag
    , payload
    ]

{-------------------------------------------------------------------------------
  SupportedNetworkProtocolVersion instance
-------------------------------------------------------------------------------}

-- Note: we don't support all combinations, so we don't declare them as
-- COMPLETE

-- | We support only Byron V1 with the hard fork disabled, as no other
-- versions have been released before the hard fork
pattern CardanoNodeToNodeVersion1 :: BlockNodeToNodeVersion (CardanoBlock c)
pattern CardanoNodeToNodeVersion1 =
    HardForkNodeToNodeDisabled ByronNodeToNodeVersion1

-- | The hard fork enabled with the latest Byron version and the Shelley era
-- enabled.
pattern CardanoNodeToNodeVersion2 :: BlockNodeToNodeVersion (CardanoBlock c)
pattern CardanoNodeToNodeVersion2 =
    HardForkNodeToNodeEnabled
      HardForkSpecificNodeToNodeVersion1
      (  EraNodeToNodeEnabled ByronNodeToNodeVersion2
      :* EraNodeToNodeEnabled ShelleyNodeToNodeVersion1
      :* EraNodeToNodeDisabled
      :* EraNodeToNodeDisabled
      :* EraNodeToNodeDisabled
      :* Nil
      )

-- | The hard fork enabled with the latest Byron version, the Shelley and
-- Allegra eras enabled.
pattern CardanoNodeToNodeVersion3 :: BlockNodeToNodeVersion (CardanoBlock c)
pattern CardanoNodeToNodeVersion3 =
    HardForkNodeToNodeEnabled
      HardForkSpecificNodeToNodeVersion1
      (  EraNodeToNodeEnabled ByronNodeToNodeVersion2
      :* EraNodeToNodeEnabled ShelleyNodeToNodeVersion1
      :* EraNodeToNodeEnabled ShelleyNodeToNodeVersion1
      :* EraNodeToNodeDisabled
      :* EraNodeToNodeDisabled
      :* Nil
      )

-- | The hard fork enabled with the latest Byron version, the Shelley, Allegra,
-- and Mary eras enabled.
pattern CardanoNodeToNodeVersion4 :: BlockNodeToNodeVersion (CardanoBlock c)
pattern CardanoNodeToNodeVersion4 =
    HardForkNodeToNodeEnabled
      HardForkSpecificNodeToNodeVersion1
      (  EraNodeToNodeEnabled ByronNodeToNodeVersion2
      :* EraNodeToNodeEnabled ShelleyNodeToNodeVersion1
      :* EraNodeToNodeEnabled ShelleyNodeToNodeVersion1
      :* EraNodeToNodeEnabled ShelleyNodeToNodeVersion1
      :* EraNodeToNodeDisabled
      :* Nil
      )

-- | The hard fork enabled with the latest Byron version, the Shelley, Allegra,
-- Mary and Alonzo eras enabled.
pattern CardanoNodeToNodeVersion5 :: BlockNodeToNodeVersion (CardanoBlock c)
pattern CardanoNodeToNodeVersion5 =
    HardForkNodeToNodeEnabled
      HardForkSpecificNodeToNodeVersion1
      (  EraNodeToNodeEnabled ByronNodeToNodeVersion2
      :* EraNodeToNodeEnabled ShelleyNodeToNodeVersion1
      :* EraNodeToNodeEnabled ShelleyNodeToNodeVersion1
      :* EraNodeToNodeEnabled ShelleyNodeToNodeVersion1
      :* EraNodeToNodeEnabled ShelleyNodeToNodeVersion1
      :* Nil
      )

-- | We support the sole Byron version with the hard fork disabled.
pattern CardanoNodeToClientVersion1 :: BlockNodeToClientVersion (CardanoBlock c)
pattern CardanoNodeToClientVersion1 =
    HardForkNodeToClientDisabled ByronNodeToClientVersion1

-- | The hard fork enabled and the Shelley era enabled.
pattern CardanoNodeToClientVersion2 :: BlockNodeToClientVersion (CardanoBlock c)
pattern CardanoNodeToClientVersion2 =
    HardForkNodeToClientEnabled
      HardForkSpecificNodeToClientVersion1
      (  EraNodeToClientEnabled ByronNodeToClientVersion1
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion1
      :* EraNodeToClientDisabled
      :* EraNodeToClientDisabled
      :* EraNodeToClientDisabled
      :* Nil
      )

-- | The hard fork enabled and the Shelley era enabled, but using
-- 'ShelleyNodeToClientVersion2' and 'HardForkSpecificNodeToClientVersion2'.
pattern CardanoNodeToClientVersion3 :: BlockNodeToClientVersion (CardanoBlock c)
pattern CardanoNodeToClientVersion3 =
    HardForkNodeToClientEnabled
      HardForkSpecificNodeToClientVersion2
      (  EraNodeToClientEnabled ByronNodeToClientVersion1
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion2
      :* EraNodeToClientDisabled
      :* EraNodeToClientDisabled
      :* EraNodeToClientDisabled
      :* Nil
      )

-- | The hard fork enabled, and the Shelley and Allegra eras enabled.
--
-- We don't bother with 'ShelleyNodeToClientVersion1' and
-- 'HardForkSpecificNodeToClientVersion1'.
pattern CardanoNodeToClientVersion4 :: BlockNodeToClientVersion (CardanoBlock c)
pattern CardanoNodeToClientVersion4 =
    HardForkNodeToClientEnabled
      HardForkSpecificNodeToClientVersion2
      (  EraNodeToClientEnabled ByronNodeToClientVersion1
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion2
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion2
      :* EraNodeToClientDisabled
      :* EraNodeToClientDisabled
      :* Nil
      )

-- | The hard fork enabled, and the Shelley, Allegra, and Mary eras enabled.
--
-- We don't bother with 'ShelleyNodeToClientVersion1'.
pattern CardanoNodeToClientVersion5 :: BlockNodeToClientVersion (CardanoBlock c)
pattern CardanoNodeToClientVersion5 =
    HardForkNodeToClientEnabled
      HardForkSpecificNodeToClientVersion2
      (  EraNodeToClientEnabled ByronNodeToClientVersion1
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion2
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion2
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion2
      :* EraNodeToClientDisabled
      :* Nil
      )

-- | The hard fork enabled, and the Shelley, Allegra, and Mary eras enabled, but
-- using 'ShelleyNodeToClientVersion3' for the Shelley-based eras , which
-- enables new queries.
pattern CardanoNodeToClientVersion6 :: BlockNodeToClientVersion (CardanoBlock c)
pattern CardanoNodeToClientVersion6 =
    HardForkNodeToClientEnabled
      HardForkSpecificNodeToClientVersion2
      (  EraNodeToClientEnabled ByronNodeToClientVersion1
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion3
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion3
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion3
      :* EraNodeToClientDisabled
      :* Nil
      )

-- | The hard fork enabled, and the Shelley, Allegra, Mary and Alonzo eras enabled
pattern CardanoNodeToClientVersion7 :: BlockNodeToClientVersion (CardanoBlock c)
pattern CardanoNodeToClientVersion7 =
    HardForkNodeToClientEnabled
      HardForkSpecificNodeToClientVersion2
      (  EraNodeToClientEnabled ByronNodeToClientVersion1
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion3
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion3
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion3
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion3
      :* Nil
      )

instance CardanoHardForkConstraints c
      => SupportedNetworkProtocolVersion (CardanoBlock c) where
  supportedNodeToNodeVersions _ = Map.fromList $
      [ (NodeToNodeV_1, CardanoNodeToNodeVersion1)
      , (NodeToNodeV_2, CardanoNodeToNodeVersion2)
      , (NodeToNodeV_3, CardanoNodeToNodeVersion2)
      , (NodeToNodeV_4, CardanoNodeToNodeVersion3)
      , (NodeToNodeV_5, CardanoNodeToNodeVersion4)
      , (NodeToNodeV_6, CardanoNodeToNodeVersion5)
      ]

  supportedNodeToClientVersions _ = Map.fromList $
      [ (NodeToClientV_1, CardanoNodeToClientVersion1)
      , (NodeToClientV_2, CardanoNodeToClientVersion1)
      , (NodeToClientV_3, CardanoNodeToClientVersion2)
      , (NodeToClientV_4, CardanoNodeToClientVersion3)
      , (NodeToClientV_5, CardanoNodeToClientVersion4)
      , (NodeToClientV_6, CardanoNodeToClientVersion5)
      , (NodeToClientV_7, CardanoNodeToClientVersion6)
      , (NodeToClientV_8, CardanoNodeToClientVersion6)
      , (NodeToClientV_9, CardanoNodeToClientVersion7)
      ]

{-------------------------------------------------------------------------------
  ProtocolInfo
-------------------------------------------------------------------------------}

-- | Parameters needed to transition between two eras.
--
-- The two eras are phantom type parameters of this type to avoid mixing up
-- multiple 'ProtocolParamsTransition's
data ProtocolParamsTransition eraFrom eraTo = ProtocolParamsTransition {
      transitionTrigger    :: TriggerHardFork
    }

-- | Create a 'ProtocolInfo' for 'CardanoBlock'
--
-- NOTE: the initial staking and funds in the 'ShelleyGenesis' are ignored,
-- /unless/ configured to skip the Byron era and hard fork to Shelley or a later
-- era from the start using @TriggerHardForkAtEpoch 0@ for testing purposes.
--
-- PRECONDITION: only a single set of Shelley credentials is allowed when used
-- for mainnet (check against @'SL.gNetworkId' 'shelleyBasedGenesis'@).
protocolInfoCardano ::
     forall c m. (IOLike m, CardanoHardForkConstraints c)
  => ProtocolParamsByron
  -> ProtocolParamsShelleyBased (ShelleyEra c)
  -> ProtocolParamsShelley
  -> ProtocolParamsAllegra
  -> ProtocolParamsMary
  -> ProtocolParamsAlonzo
  -> ProtocolParamsTransition
       ByronBlock
       (ShelleyBlock (ShelleyEra c))
  -> ProtocolParamsTransition
       (ShelleyBlock (ShelleyEra c))
       (ShelleyBlock (AllegraEra c))
  -> ProtocolParamsTransition
       (ShelleyBlock (AllegraEra c))
       (ShelleyBlock (MaryEra c))
  -> ProtocolParamsTransition
       (ShelleyBlock (MaryEra c))
       (ShelleyBlock (AlonzoEra c))
  -> ProtocolInfo m (CardanoBlock c)
protocolInfoCardano protocolParamsByron@ProtocolParamsByron {
                        byronGenesis           = genesisByron
                      , byronLeaderCredentials = mCredsByron
                      }
                    ProtocolParamsShelleyBased {
                        shelleyBasedGenesis           = genesisShelley
                      , shelleyBasedInitialNonce      = initialNonceShelley
                      , shelleyBasedLeaderCredentials = credssShelleyBased
                      }
                    ProtocolParamsShelley {
                        shelleyProtVer = protVerShelley
                      }
                    ProtocolParamsAllegra {
                        allegraProtVer = protVerAllegra
                      }
                    ProtocolParamsMary {
                        maryProtVer = protVerMary
                      }
                    ProtocolParamsAlonzo {
                        alonzoProtVer = protVerAlonzo
                      }
                    ProtocolParamsTransition {
                        transitionTrigger = triggerHardForkByronShelley
                      }
                    ProtocolParamsTransition {
                        transitionTrigger = triggerHardForkShelleyAllegra
                      }
                    ProtocolParamsTransition {
                        transitionTrigger = triggerHardForkAllegraMary
                      }
                    ProtocolParamsTransition {
                        transitionTrigger = triggerHardForkMaryAlonzo
                      }
  | SL.Mainnet <- SL.sgNetworkId genesisShelley
  , length credssShelleyBased > 1
  = error "Multiple Shelley-based credentials not allowed for mainnet"
  | otherwise
  = assertWithMsg (validateGenesis genesisShelley) $
    ProtocolInfo {
        pInfoConfig       = cfg
      , pInfoInitLedger   = initExtLedgerStateCardano
      , pInfoBlockForging = blockForging
      }
  where
    -- The major protocol version of the last era is the maximum major protocol
    -- version we support.
    maxMajorProtVer :: MaxMajorProtVer
    maxMajorProtVer = MaxMajorProtVer (pvMajor protVerMary)

    -- Byron
    ProtocolInfo {
        pInfoConfig = topLevelConfigByron@TopLevelConfig {
            topLevelConfigProtocol = consensusConfigByron
          , topLevelConfigLedger   = ledgerConfigByron
          , topLevelConfigBlock    = blockConfigByron
          }
      , pInfoInitLedger = initExtLedgerStateByron
      } = protocolInfoByron @m protocolParamsByron

    partialConsensusConfigByron :: PartialConsensusConfig (BlockProtocol ByronBlock)
    partialConsensusConfigByron = consensusConfigByron

    partialLedgerConfigByron :: PartialLedgerConfig ByronBlock
    partialLedgerConfigByron = ByronPartialLedgerConfig {
          byronLedgerConfig    = ledgerConfigByron
        , byronTriggerHardFork = triggerHardForkByronShelley
        }

    kByron :: SecurityParam
    kByron = Byron.genesisSecurityParam genesisByron

    -- Shelley

    tpraosParams :: TPraosParams
    tpraosParams@TPraosParams { tpraosSlotsPerKESPeriod } =
        Shelley.mkTPraosParams
          maxMajorProtVer
          initialNonceShelley
          genesisShelley

    blockConfigShelley :: BlockConfig (ShelleyBlock (ShelleyEra c))
    blockConfigShelley =
        Shelley.mkShelleyBlockConfig
          protVerShelley
          genesisShelley
          (tpraosBlockIssuerVKey <$> credssShelleyBased)

    partialConsensusConfigShelley ::
         PartialConsensusConfig (BlockProtocol (ShelleyBlock (ShelleyEra c)))
    partialConsensusConfigShelley = tpraosParams

    partialLedgerConfigShelley :: PartialLedgerConfig (ShelleyBlock (ShelleyEra c))
    partialLedgerConfigShelley =
        mkPartialLedgerConfigShelley
          genesisShelley
          maxMajorProtVer
          triggerHardForkShelleyAllegra

    kShelley :: SecurityParam
    kShelley = SecurityParam $ sgSecurityParam genesisShelley

    -- Allegra

    genesisAllegra :: ShelleyGenesis (AllegraEra c)
    genesisAllegra = SL.translateEra' () genesisShelley

    blockConfigAllegra :: BlockConfig (ShelleyBlock (AllegraEra c))
    blockConfigAllegra =
        Shelley.mkShelleyBlockConfig
          protVerAllegra
          genesisAllegra
          (tpraosBlockIssuerVKey <$> credssShelleyBased)

    partialConsensusConfigAllegra ::
         PartialConsensusConfig (BlockProtocol (ShelleyBlock (AllegraEra c)))
    partialConsensusConfigAllegra = tpraosParams

    partialLedgerConfigAllegra :: PartialLedgerConfig (ShelleyBlock (AllegraEra c))
    partialLedgerConfigAllegra =
        mkPartialLedgerConfigShelley
          genesisAllegra
          maxMajorProtVer
          triggerHardForkAllegraMary

    -- Mary

    genesisMary :: ShelleyGenesis (MaryEra c)
    genesisMary = SL.translateEra' () genesisAllegra

    blockConfigMary :: BlockConfig (ShelleyBlock (MaryEra c))
    blockConfigMary =
        Shelley.mkShelleyBlockConfig
          protVerMary
          genesisMary
          (tpraosBlockIssuerVKey <$> credssShelleyBased)

    partialConsensusConfigMary ::
         PartialConsensusConfig (BlockProtocol (ShelleyBlock (MaryEra c)))
    partialConsensusConfigMary = tpraosParams

    partialLedgerConfigMary :: PartialLedgerConfig (ShelleyBlock (MaryEra c))
    partialLedgerConfigMary =
        mkPartialLedgerConfigShelley
          genesisMary
          maxMajorProtVer
          triggerHardForkMaryAlonzo

    -- Alonzo

    genesisAlonzo :: ShelleyGenesis (AlonzoEra c)
    genesisAlonzo = SL.translateEra' () genesisAllegra

    blockConfigAlonzo :: BlockConfig (ShelleyBlock (AlonzoEra c))
    blockConfigAlonzo =
        Shelley.mkShelleyBlockConfig
          protVerAlonzo
          genesisAlonzo
          (tpraosBlockIssuerVKey <$> credssShelleyBased)

    partialConsensusConfigAlonzo ::
         PartialConsensusConfig (BlockProtocol (ShelleyBlock (AlonzoEra c)))
    partialConsensusConfigAlonzo = tpraosParams

    partialLedgerConfigAlonzo :: PartialLedgerConfig (ShelleyBlock (AlonzoEra c))
    partialLedgerConfigAlonzo =
        mkPartialLedgerConfigShelley
          genesisAlonzo
          maxMajorProtVer
          TriggerHardForkNever

    -- Cardano

    k :: SecurityParam
    k = assert (kByron == kShelley) kByron

    shape :: History.Shape (CardanoEras c)
    shape = History.Shape $ Exactly $
           K (Byron.byronEraParams     genesisByron)
        :* K (Shelley.shelleyEraParams genesisShelley)
        :* K (Shelley.shelleyEraParams genesisAllegra)
        :* K (Shelley.shelleyEraParams genesisMary)
        :* K (Shelley.shelleyEraParams genesisAlonzo)
        :* Nil

    cfg :: TopLevelConfig (CardanoBlock c)
    cfg = TopLevelConfig {
        topLevelConfigProtocol = HardForkConsensusConfig {
            hardForkConsensusConfigK      = k
          , hardForkConsensusConfigShape  = shape
          , hardForkConsensusConfigPerEra = PerEraConsensusConfig
              (  WrapPartialConsensusConfig partialConsensusConfigByron
              :* WrapPartialConsensusConfig partialConsensusConfigShelley
              :* WrapPartialConsensusConfig partialConsensusConfigAllegra
              :* WrapPartialConsensusConfig partialConsensusConfigMary
              :* WrapPartialConsensusConfig partialConsensusConfigAlonzo
              :* Nil
              )
          }
      , topLevelConfigLedger = HardForkLedgerConfig {
            hardForkLedgerConfigShape  = shape
          , hardForkLedgerConfigPerEra = PerEraLedgerConfig
              (  WrapPartialLedgerConfig partialLedgerConfigByron
              :* WrapPartialLedgerConfig partialLedgerConfigShelley
              :* WrapPartialLedgerConfig partialLedgerConfigAllegra
              :* WrapPartialLedgerConfig partialLedgerConfigMary
              :* WrapPartialLedgerConfig partialLedgerConfigAlonzo
              :* Nil
              )
          }
      , topLevelConfigBlock =
          CardanoBlockConfig
            blockConfigByron
            blockConfigShelley
            blockConfigAllegra
            blockConfigMary
            blockConfigAlonzo
      , topLevelConfigCodec =
          CardanoCodecConfig
            (configCodec topLevelConfigByron)
            Shelley.ShelleyCodecConfig
            Shelley.ShelleyCodecConfig
            Shelley.ShelleyCodecConfig
            Shelley.ShelleyCodecConfig
      , topLevelConfigStorage =
          CardanoStorageConfig
            (configStorage topLevelConfigByron)
            (Shelley.ShelleyStorageConfig tpraosSlotsPerKESPeriod k)
            (Shelley.ShelleyStorageConfig tpraosSlotsPerKESPeriod k)
            (Shelley.ShelleyStorageConfig tpraosSlotsPerKESPeriod k)
            (Shelley.ShelleyStorageConfig tpraosSlotsPerKESPeriod k)
      }

    -- When the initial ledger state is not in the Byron era, register the
    -- initial staking and initial funds (if provided in the genesis config) in
    -- the ledger state.
    initExtLedgerStateCardano :: ExtLedgerState (CardanoBlock c)
    initExtLedgerStateCardano = ExtLedgerState {
          headerState = initHeaderState
        , ledgerState = overShelleyBasedLedgerState register initLedgerState
        }
      where
        initHeaderState :: HeaderState (CardanoBlock c)
        initLedgerState :: LedgerState (CardanoBlock c)
        ExtLedgerState initLedgerState initHeaderState =
          injectInitialExtLedgerState cfg initExtLedgerStateByron

        register ::
             (EraCrypto era ~ c, ShelleyBasedEra era)
          => LedgerState (ShelleyBlock era)
          -> LedgerState (ShelleyBlock era)
        register st = st {
              Shelley.shelleyLedgerState =
                -- We must first register the initial funds, because the stake
                -- information depends on it.
                  registerGenesisStaking
                    (SL.sgStaking genesisShelley)
                . registerInitialFunds
                    (SL.sgInitialFunds genesisShelley)
                $ Shelley.shelleyLedgerState st
            }

    -- | For each element in the list, a block forging thread will be started.
    --
    -- When no credentials are passed, there will be no threads.
    --
    -- Typically, there will only be a single set of credentials for Shelley.
    --
    -- In case there are multiple credentials for Shelley, which is only done
    -- for testing/benchmarking purposes, we'll have a separate thread for each
    -- of them.
    --
    -- If Byron credentials are passed, we merge them with the Shelley
    -- credentials if possible, so that we only have a single thread running in
    -- the case we have Byron credentials and a single set of Shelley
    -- credentials. If there are multiple Shelley credentials, we merge the
    -- Byron credentials with the first Shelley one but still have separate
    -- threads for the remaining Shelley ones.
    blockForging :: m [BlockForging m (CardanoBlock c)]
    blockForging = do
        shelleyBased <- blockForgingShelleyBased
        let blockForgings :: [OptNP 'False (BlockForging m) (CardanoEras c)]
            blockForgings = case (mBlockForgingByron, shelleyBased) of
              (Nothing,    shelleys)         -> shelleys
              (Just byron, [])               -> [byron]
              (Just byron, shelley:shelleys) ->
                  OptNP.zipWith merge byron shelley : shelleys
                where
                  -- When merging Byron with Shelley-based eras, we should never
                  -- merge two from the same era.
                  merge (These1 _ _) = error "forgings of the same era"
                  merge (This1 x)    = x
                  merge (That1 y)    = y

        return $ hardForkBlockForging "Cardano" <$> blockForgings

    mBlockForgingByron :: Maybe (OptNP 'False (BlockForging m) (CardanoEras c))
    mBlockForgingByron = do
        creds <- mCredsByron
        return $ byronBlockForging creds `OptNP.at` IZ

    blockForgingShelleyBased :: m [OptNP 'False (BlockForging m) (CardanoEras c)]
    blockForgingShelleyBased = do
        shelleyBased <-
          traverse
            (shelleySharedBlockForging (Proxy @(ShelleyBasedEras c)) tpraosParams)
            credssShelleyBased
        return $ reassoc <$> shelleyBased
      where
        reassoc ::
             NP (BlockForging m :.: ShelleyBlock) (ShelleyBasedEras c)
          -> OptNP 'False (BlockForging m) (CardanoEras c)
        reassoc = OptSkip . injectShelleyOptNP unComp . OptNP.fromNonEmptyNP

protocolClientInfoCardano
  :: forall c.
     -- Byron
     EpochSlots
  -> ProtocolClientInfo (CardanoBlock c)
protocolClientInfoCardano epochSlots = ProtocolClientInfo {
      pClientInfoCodecConfig =
        CardanoCodecConfig
          (pClientInfoCodecConfig (protocolClientInfoByron epochSlots))
          (pClientInfoCodecConfig protocolClientInfoShelley)
          (pClientInfoCodecConfig protocolClientInfoShelley)
          (pClientInfoCodecConfig protocolClientInfoShelley)
          (pClientInfoCodecConfig protocolClientInfoShelley)
    }

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

mkPartialLedgerConfigShelley ::
     ShelleyGenesis era
  -> MaxMajorProtVer
  -> TriggerHardFork
  -> PartialLedgerConfig (ShelleyBlock era)
mkPartialLedgerConfigShelley genesisShelley maxMajorProtVer shelleyTriggerHardFork =
    ShelleyPartialLedgerConfig {
          shelleyLedgerConfig =
            Shelley.mkShelleyLedgerConfig
              genesisShelley
              -- 'completeLedgerConfig' will replace the 'History.dummyEpochInfo'
              -- in the partial ledger config with the correct one.
              History.dummyEpochInfo
              maxMajorProtVer
        , shelleyTriggerHardFork = shelleyTriggerHardFork
        }
