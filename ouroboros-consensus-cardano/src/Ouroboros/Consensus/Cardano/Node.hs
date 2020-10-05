{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Cardano.Node (
    protocolInfoCardano
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
  ) where

import qualified Codec.CBOR.Decoding as CBOR
import           Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as CBOR
import           Control.Exception (assert)
import qualified Data.ByteString.Short as Short
import           Data.Functor.Contravariant (contramap)
import qualified Data.Map.Strict as Map
import           Data.SOP.Strict (K (..), NP (..), NS (..))
import           Data.Word (Word16)

import           Cardano.Binary (DecoderError (..), enforceSize)
import qualified Cardano.Chain.Genesis as Byron.Genesis
import           Cardano.Chain.Slotting (EpochSlots)
import qualified Cardano.Chain.Update as Byron.Update
import           Cardano.Prelude (cborError)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Storage.ImmutableDB (simpleChunkInfo)
import           Ouroboros.Consensus.Storage.Serialisation
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util.Assert
import           Ouroboros.Consensus.Util.Counting
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.Serialisation

import           Ouroboros.Consensus.Byron.Ledger (ByronBlock)
import qualified Ouroboros.Consensus.Byron.Ledger as Byron
import qualified Ouroboros.Consensus.Byron.Ledger.Conversions as Byron
import           Ouroboros.Consensus.Byron.Ledger.NetworkProtocolVersion
import           Ouroboros.Consensus.Byron.Node

import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)
import qualified Ouroboros.Consensus.Shelley.Ledger as Shelley
import           Ouroboros.Consensus.Shelley.Ledger.NetworkProtocolVersion
import           Ouroboros.Consensus.Shelley.Node
import           Ouroboros.Consensus.Shelley.Protocol (MaxMajorProtVer (..),
                     TPraosCrypto, TPraosParams (..))
import qualified Ouroboros.Consensus.Shelley.Protocol as Shelley

import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Cardano.CanHardFork

{-------------------------------------------------------------------------------
  SerialiseHFC
-------------------------------------------------------------------------------}

instance SerialiseConstraintsHFC ByronBlock
instance TPraosCrypto era => SerialiseConstraintsHFC (ShelleyBlock era)

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
  encodeDiskHfcBlock (CardanoCodecConfig ccfgByron ccfgShelley ccfgAllegra ccfgMary) = \case
      -- We are backwards compatible with Byron and thus use the exact same
      -- encoding.
      BlockByron   blockByron   ->                encodeDisk ccfgByron blockByron
      -- For Shelley and later eras, we need to prepend the hard fork envelope.
      BlockShelley blockShelley -> prependTag 2 $ encodeDisk ccfgShelley blockShelley
      BlockAllegra blockAllegra -> prependTag 3 $ encodeDisk ccfgAllegra blockAllegra
      BlockMary    blockMary    -> prependTag 4 $ encodeDisk ccfgMary    blockMary
  decodeDiskHfcBlock (CardanoCodecConfig ccfgByron ccfgShelley ccfgAllegra ccfgMary) = do
      enforceSize "CardanoBlock" 2
      CBOR.decodeWord >>= \case
        0 -> fmap BlockByron   <$> Byron.decodeByronBoundaryBlock epochSlots
        1 -> fmap BlockByron   <$> Byron.decodeByronRegularBlock  epochSlots
        -- We don't have to drop the first two bytes from the 'ByteString'
        -- passed to the decoder as slicing already takes care of this.
        2 -> fmap BlockShelley <$> decodeDisk ccfgShelley
        3 -> fmap BlockAllegra <$> decodeDisk ccfgAllegra
        4 -> fmap BlockMary    <$> decodeDisk ccfgMary
        t -> cborError $ DecoderErrorUnknownTag "CardanoBlock" (fromIntegral t)
    where
      epochSlots = Byron.getByronEpochSlots ccfgByron

  reconstructHfcPrefixLen _ = PrefixLen 2

  reconstructHfcNestedCtxt _ prefix blockSize =
      case Short.index prefix 1 of
        0 -> SomeBlock $ NestedCtxt (NCZ (Byron.CtxtByronBoundary blockSize))
        1 -> SomeBlock $ NestedCtxt (NCZ (Byron.CtxtByronRegular  blockSize))
        2 -> SomeBlock $ NestedCtxt (NCS (NCZ Shelley.CtxtShelley))
        3 -> SomeBlock $ NestedCtxt (NCS (NCS (NCZ Shelley.CtxtShelley)))
        4 -> SomeBlock $ NestedCtxt (NCS (NCS (NCS (NCZ Shelley.CtxtShelley))))
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
    where
      shiftHeaderOffset :: Word16 -> BinaryBlockInfo -> BinaryBlockInfo
      shiftHeaderOffset shift binfo = binfo {
            headerOffset = headerOffset binfo + shift
          }

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
    HardForkNodeToNodeEnabled (
         EraNodeToNodeEnabled ByronNodeToNodeVersion2
      :* EraNodeToNodeEnabled ShelleyNodeToNodeVersion1
      :* EraNodeToNodeDisabled
      :* EraNodeToNodeDisabled
      :* Nil
      )

-- | The hard fork enabled with the latest Byron version, the Shelley and
-- Allegra eras enabled.
pattern CardanoNodeToNodeVersion3 :: BlockNodeToNodeVersion (CardanoBlock c)
pattern CardanoNodeToNodeVersion3 =
    HardForkNodeToNodeEnabled (
         EraNodeToNodeEnabled ByronNodeToNodeVersion2
      :* EraNodeToNodeEnabled ShelleyNodeToNodeVersion1
      :* EraNodeToNodeEnabled ShelleyNodeToNodeVersion1
      :* EraNodeToNodeDisabled
      :* Nil
      )

-- | The hard fork enabled with the latest Byron version, the Shelley, Allegra,
-- and Mary eras enabled.
pattern CardanoNodeToNodeVersion4 :: BlockNodeToNodeVersion (CardanoBlock c)
pattern CardanoNodeToNodeVersion4 =
    HardForkNodeToNodeEnabled (
         EraNodeToNodeEnabled ByronNodeToNodeVersion2
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
    HardForkNodeToClientEnabled (
         EraNodeToClientEnabled ByronNodeToClientVersion1
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion1
      :* EraNodeToClientDisabled
      :* EraNodeToClientDisabled
      :* Nil
      )

-- | The hard fork enabled, and the Shelley and Allegra eras enabled.
pattern CardanoNodeToClientVersion3 :: BlockNodeToClientVersion (CardanoBlock c)
pattern CardanoNodeToClientVersion3 =
    HardForkNodeToClientEnabled (
         EraNodeToClientEnabled ByronNodeToClientVersion1
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion1
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion1
      :* EraNodeToClientDisabled
      :* Nil
      )

-- | The hard fork enabled, and the Shelley, Allegra, and Mary eras enabled.
pattern CardanoNodeToClientVersion4 :: BlockNodeToClientVersion (CardanoBlock c)
pattern CardanoNodeToClientVersion4 =
    HardForkNodeToClientEnabled (
         EraNodeToClientEnabled ByronNodeToClientVersion1
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion1
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion1
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion1
      :* Nil
      )

instance CardanoHardForkConstraints c
      => SupportedNetworkProtocolVersion (CardanoBlock c) where
  supportedNodeToNodeVersions _ = Map.fromList $
      [ (NodeToNodeV_1, CardanoNodeToNodeVersion1)
      , (NodeToNodeV_2, CardanoNodeToNodeVersion2)
      , (NodeToNodeV_3, CardanoNodeToNodeVersion2)
      -- TODO #2670 enable these
      -- , (NodeToNodeV_4, CardanoNodeToNodeVersion3)
      -- , (NodeToNodeV_5, CardanoNodeToNodeVersion4)
      ]

  supportedNodeToClientVersions _ = Map.fromList $
      [ (NodeToClientV_1, CardanoNodeToClientVersion1)
      , (NodeToClientV_2, CardanoNodeToClientVersion1)
      , (NodeToClientV_3, CardanoNodeToClientVersion2)
      -- TODO #2670 enable these
      -- , (NodeToClientV_4, CardanoNodeToClientVersion3)
      -- , (NodeToClientV_5, CardanoNodeToClientVersion4)
      ]

{-------------------------------------------------------------------------------
  RunNode instance
-------------------------------------------------------------------------------}

instance CardanoHardForkConstraints c => RunNode (CardanoBlock c) where
  -- TODO pull out of RunNode
  nodeBlockFetchSize = \case
      HeaderByron   headerByron   -> nodeBlockFetchSize headerByron
      -- For Shelley and later eras, we add two extra bytes, see the
      -- 'SerialiseHFC' instance.
      HeaderShelley headerShelley -> nodeBlockFetchSize headerShelley + 2
      HeaderAllegra headerAllegra -> nodeBlockFetchSize headerAllegra + 2
      HeaderMary    headerMary    -> nodeBlockFetchSize headerMary    + 2

  -- Use a ChunkInfo with Byron's epoch size for the whole chain. This means
  -- Shelley chunks will be 10x smaller, as the slot density is 10x smaller.
  --
  -- TODO update when a non-uniform chunk size is supported
  nodeImmutableDbChunkInfo =
        simpleChunkInfo
      . History.eraEpochSize
      . exactlyHead
      . History.getShape
      . hardForkLedgerConfigShape
      . configLedger

  -- Call Byron's intialisation, as the chain starts with Byron
  nodeInitChainDB cfg initChainDB =
      nodeInitChainDB
        (projByronTopLevelConfig cfg)
        (contramap BlockByron initChainDB)

  nodeCheckIntegrity cfg = \case
      BlockByron blockByron ->
        Byron.verifyBlockIntegrity blockCfgByron blockByron
      BlockShelley blockShelley ->
        Shelley.verifyBlockIntegrity slotsPerKESPeriodShelley blockShelley
      BlockAllegra blockAllegra ->
        Shelley.verifyBlockIntegrity slotsPerKESPeriodAllegra blockAllegra
      BlockMary blockMary ->
        Shelley.verifyBlockIntegrity slotsPerKESPeriodMary    blockMary
    where
      TopLevelConfig {
          topLevelConfigProtocol =
            CardanoConsensusConfig
              _
              TPraosParams { tpraosSlotsPerKESPeriod = slotsPerKESPeriodShelley }
              TPraosParams { tpraosSlotsPerKESPeriod = slotsPerKESPeriodAllegra }
              TPraosParams { tpraosSlotsPerKESPeriod = slotsPerKESPeriodMary    }
        , topLevelConfigBlock = CardanoBlockConfig blockCfgByron _ _ _
        } = cfg

{-------------------------------------------------------------------------------
  ProtocolInfo
-------------------------------------------------------------------------------}

protocolInfoCardano ::
     forall c m. (IOLike m, CardanoHardForkConstraints c)
     -- Common
  => Byron.Update.ProtocolVersion  -- ^ Protocol version used for all eras
  -> MaxMajorProtVer

     -- Byron
  -> Byron.Genesis.Config
  -> Maybe PBftSignatureThreshold
  -> Byron.Update.SoftwareVersion
  -> [ByronLeaderCredentials]
  -> Maybe EpochNo   -- ^ Lower bound on first Shelley epoch
  -> TriggerHardFork -- ^ Transition from Byron to Shelley

     -- Shelley
  -> ShelleyGenesis (ShelleyEra c)
  -> Nonce
     -- ^ The initial nonce for the Shelley era, typically derived from the
     -- hash of Shelley Genesis config JSON file.
  -> [TPraosLeaderCredentials (ShelleyEra c)]
  -> Maybe EpochNo   -- ^ Lower bound on first Allegra epoch
  -> TriggerHardFork -- ^ Transition from Shelley to Allegra

     -- Allegra
  -> [TPraosLeaderCredentials (AllegraEra c)]
  -> Maybe EpochNo   -- ^ Lower bound on first Allegra epoch
  -> TriggerHardFork -- ^ Transition from Allegra to Mary

     -- Mary
  -> [TPraosLeaderCredentials (MaryEra c)]

  -> ProtocolInfo m (CardanoBlock c)
protocolInfoCardano protVerByron maxMajorProtVer
                    genesisByron mSigThresh softVerByron credssByron mbLowerBoundShelley triggerHardForkByronShelley
                    genesisShelley initialNonce credssShelley mbLowerBoundAllegra triggerHardForkShelleyAllegra
                    credssAllegra mbLowerBoundMary triggerHardForkAllegraMary
                    credssMary =
    assertWithMsg (validateGenesis genesisShelley) $
    ProtocolInfo {
        pInfoConfig = cfg
      , pInfoInitLedger = ExtLedgerState {
            ledgerState =
              HardForkLedgerState $
                initHardForkState initLedgerStateByron
          , headerState =
              genesisHeaderState $
                initHardForkState $
                  WrapChainDepState $
                    headerStateChainDep initHeaderStateByron
          }
      , pInfoBlockForging = mconcat [
            [ return $ hardForkBlockForging $ Z $ byronBlockForging credsByron
            | credsByron <- credssByron
            ]
          , [ hardForkBlockForging . S . Z
                <$> shelleyBlockForging tpraosParams credsShelley
            | credsShelley <- credssShelley
            ]
          , [ hardForkBlockForging . S . S . Z
                <$> shelleyBlockForging tpraosParams credsAllegra
            | credsAllegra <- credssAllegra
            ]
          , [ hardForkBlockForging . S . S . S . Z
                <$> shelleyBlockForging tpraosParams credsMary
            | credsMary <- credssMary
            ]
          ]
      }
  where
    -- Common
    protVer :: ProtVer
    protVer = translateProtocolVersion protVerByron

    -- Byron
    ProtocolInfo {
        pInfoConfig = topLevelConfigByron@TopLevelConfig {
            topLevelConfigProtocol = consensusConfigByron
          , topLevelConfigLedger   = ledgerConfigByron
          , topLevelConfigBlock    = blockConfigByron
          }
      , pInfoInitLedger = ExtLedgerState {
            ledgerState = initLedgerStateByron
          , headerState = initHeaderStateByron
          }
      } = protocolInfoByron @m genesisByron mSigThresh protVerByron softVerByron credssByron

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
    tpraosParams =
        Shelley.mkTPraosParams
          maxMajorProtVer
          initialNonce
          genesisShelley

    blockConfigShelley :: BlockConfig (ShelleyBlock (ShelleyEra c))
    blockConfigShelley =
        Shelley.mkShelleyBlockConfig
          protVer
          genesisShelley
          (tpraosBlockIssuerVKey <$> credssShelley)

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

    blockConfigAllegra :: BlockConfig (ShelleyBlock (AllegraEra c))
    blockConfigAllegra = blockConfigShelley

    partialConsensusConfigAllegra ::
         PartialConsensusConfig (BlockProtocol (ShelleyBlock (AllegraEra c)))
    partialConsensusConfigAllegra = tpraosParams

    partialLedgerConfigAllegra :: PartialLedgerConfig (ShelleyBlock (AllegraEra c))
    partialLedgerConfigAllegra =
        mkPartialLedgerConfigShelley
          genesisShelley
          maxMajorProtVer
          triggerHardForkAllegraMary

    -- Mary

    blockConfigMary :: BlockConfig (ShelleyBlock (MaryEra c))
    blockConfigMary = blockConfigShelley

    partialConsensusConfigMary ::
         PartialConsensusConfig (BlockProtocol (ShelleyBlock (MaryEra c)))
    partialConsensusConfigMary = tpraosParams

    partialLedgerConfigMary :: PartialLedgerConfig (ShelleyBlock (MaryEra c))
    partialLedgerConfigMary =
        mkPartialLedgerConfigShelley
          genesisShelley
          maxMajorProtVer
          TriggerHardForkNever

    -- Cardano

    k :: SecurityParam
    k = assert (kByron == kShelley) kByron

    shape :: History.Shape (CardanoEras c)
    shape = History.Shape $ Exactly $
           K (Byron.byronEraParams     (safeBefore mbLowerBoundShelley) genesisByron)
        :* K (Shelley.shelleyEraParams (safeBefore mbLowerBoundAllegra) genesisShelley)
        :* K (Shelley.shelleyEraParams (safeBefore mbLowerBoundMary)    genesisShelley)
        :* K (Shelley.shelleyEraParams (safeBefore Nothing)             genesisShelley)
        :* Nil
      where
        safeBefore :: Maybe EpochNo -> History.SafeBeforeEpoch
        safeBefore = maybe History.NoLowerBound History.LowerBound

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
              :* Nil
              )
          }
      , topLevelConfigBlock =
          CardanoBlockConfig
            blockConfigByron
            blockConfigShelley
            blockConfigAllegra
            blockConfigMary
      , topLevelConfigCodec =
          CardanoCodecConfig
            (configCodec topLevelConfigByron)
            Shelley.ShelleyCodecConfig
            Shelley.ShelleyCodecConfig
            Shelley.ShelleyCodecConfig
      }

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

-- | We are lucky that for Byron we can construct all the full configs from
-- the partial ones, which means we can reconstruct the 'TopLevelConfig' for
-- Byron. This is not possible for Shelley, as we would have to call
-- 'completeLedgerConfig' and 'completeConsensusConfig' first.
projByronTopLevelConfig
  :: TopLevelConfig (CardanoBlock c)
  -> TopLevelConfig ByronBlock
projByronTopLevelConfig cfg = byronCfg
  where
    TopLevelConfig {
        topLevelConfigProtocol = CardanoConsensusConfig byronConsensusCfg _ _ _
      , topLevelConfigLedger   = CardanoLedgerConfig    byronLedgerCfg    _ _ _
      , topLevelConfigBlock    = CardanoBlockConfig     byronBlockCfg     _ _ _
      , topLevelConfigCodec    = CardanoCodecConfig     byronCodecCfg     _ _ _
      } = cfg

    byronCfg :: TopLevelConfig ByronBlock
    byronCfg = TopLevelConfig {
        topLevelConfigProtocol = byronConsensusCfg
      , topLevelConfigLedger   = byronLedgerConfig byronLedgerCfg
      , topLevelConfigBlock    = byronBlockCfg
      , topLevelConfigCodec    = byronCodecCfg
      }

-- | Translate the protocol version from the Byron era to a protocol version
-- from a Shelley-based era.
--
-- A Shelley protocol version has two components: /major/ and /minor/. The Byron
-- protocol version has one extra component, /alt/, which we drop in the
-- translation to the Shelley one.
translateProtocolVersion :: Byron.Update.ProtocolVersion -> ProtVer
translateProtocolVersion (Byron.Update.ProtocolVersion major minor _alt) =
    ProtVer (fromIntegral major) (fromIntegral minor)
