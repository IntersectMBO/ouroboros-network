{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -Wno-orphans
                -Wno-incomplete-patterns
                -Wno-incomplete-uni-patterns
                -Wno-incomplete-record-updates
                -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Ouroboros.Consensus.Cardano.Node (
    CardanoHardForkConstraints
  , MaxMajorProtVer (..)
  , ProtocolParamsAllegra (..)
  , ProtocolParamsMary (..)
  , ProtocolTransitionParamsShelleyBased (..)
  , TriggerHardFork (..)
  , protocolClientInfoCardano
  , protocolInfoCardano
    -- * SupportedNetworkProtocolVersion
  , pattern CardanoNodeToClientVersion1
  , pattern CardanoNodeToClientVersion10
  , pattern CardanoNodeToClientVersion11
  , pattern CardanoNodeToClientVersion2
  , pattern CardanoNodeToClientVersion3
  , pattern CardanoNodeToClientVersion4
  , pattern CardanoNodeToClientVersion5
  , pattern CardanoNodeToClientVersion6
  , pattern CardanoNodeToClientVersion7
  , pattern CardanoNodeToClientVersion8
  , pattern CardanoNodeToClientVersion9
  , pattern CardanoNodeToNodeVersion1
  , pattern CardanoNodeToNodeVersion2
  , pattern CardanoNodeToNodeVersion3
  , pattern CardanoNodeToNodeVersion4
  , pattern CardanoNodeToNodeVersion5
  , pattern CardanoNodeToNodeVersion6
  , pattern CardanoNodeToNodeVersion7
  ) where

import           Cardano.Binary (DecoderError (..), enforceSize)
import           Cardano.Chain.Slotting (EpochSlots)
import qualified Cardano.Ledger.BaseTypes as SL
import qualified Cardano.Ledger.Era as Core
import qualified Cardano.Ledger.Shelley.API as SL
import           Cardano.Prelude (cborError)
import qualified Cardano.Protocol.TPraos.OCert as Absolute (KESPeriod (..),
                     ocertKESPeriod)
import           Cardano.Slotting.Time (SystemStart (SystemStart))
import qualified Codec.CBOR.Decoding as CBOR
import           Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as CBOR
import           Control.Exception (assert)
import qualified Data.ByteString.Short as Short
import           Data.Functor.These (These1 (..))
import qualified Data.ListMap as ListMap
import qualified Data.Map.Strict as Map
import           Data.SOP.Counting (Exactly (Exactly), exactlyWeakenNonEmpty,
                     nonEmptyLast)
import           Data.SOP.Index (Index (..))
import           Data.SOP.OptNP (NonEmptyOptNP, OptNP (OptSkip))
import qualified Data.SOP.OptNP as OptNP
import           Data.SOP.Strict (K (K), NP (Nil, (:*)))
import           Data.Word (Word16, Word64)
import           Ouroboros.Consensus.Block (BlockConfig, BlockForging,
                     BlockProtocol, NestedCtxt (NestedCtxt), SlotNo (SlotNo),
                     SomeSecond (SomeSecond))
import           Ouroboros.Consensus.Byron.Ledger (ByronBlock)
import qualified Ouroboros.Consensus.Byron.Ledger as Byron
import qualified Ouroboros.Consensus.Byron.Ledger.Conversions as Byron
import           Ouroboros.Consensus.Byron.Ledger.NetworkProtocolVersion
                     (ByronNodeToClientVersion (ByronNodeToClientVersion1),
                     ByronNodeToNodeVersion (ByronNodeToNodeVersion1, ByronNodeToNodeVersion2))
import           Ouroboros.Consensus.Byron.Node
                     (ProtocolParamsByron (ProtocolParamsByron, byronGenesis, byronLeaderCredentials, byronMaxTxCapacityOverrides),
                     byronBlockForging, protocolClientInfoByron,
                     protocolInfoByron)
import           Ouroboros.Consensus.Cardano.Block (AllegraEra, AlonzoEra,
                     BabbageEra, BlockConfig (CardanoBlockConfig), CardanoBlock,
                     CardanoEras, CardanoShelleyEras,
                     CodecConfig (CardanoCodecConfig), ConwayEra, EraCrypto,
                     HardForkBlock (BlockAllegra, BlockAlonzo, BlockBabbage, BlockByron, BlockConway, BlockMary, BlockShelley),
                     Header (HeaderAllegra, HeaderAlonzo, HeaderBabbage, HeaderByron, HeaderConway, HeaderMary, HeaderShelley),
                     LedgerState, MaryEra, ShelleyBasedEra, ShelleyEra,
                     StandardCrypto, StorageConfig (CardanoStorageConfig))
import           Ouroboros.Consensus.Cardano.CanHardFork
                     (ByronPartialLedgerConfig (ByronPartialLedgerConfig, byronLedgerConfig, byronTriggerHardFork),
                     CardanoHardForkConstraints,
                     ShelleyPartialLedgerConfig (ShelleyPartialLedgerConfig, shelleyLedgerConfig, shelleyTriggerHardFork),
                     TriggerHardFork (..))
import           Ouroboros.Consensus.Config (SecurityParam (SecurityParam),
                     TopLevelConfig (..), configCodec, configStorage)
import           Ouroboros.Consensus.HardFork.Combinator
                     (ConsensusConfig (HardForkConsensusConfig, hardForkConsensusConfigK, hardForkConsensusConfigPerEra, hardForkConsensusConfigShape),
                     HardForkLedgerConfig (HardForkLedgerConfig, hardForkLedgerConfigPerEra, hardForkLedgerConfigShape),
                     HasPartialConsensusConfig (PartialConsensusConfig),
                     HasPartialLedgerConfig (PartialLedgerConfig),
                     LedgerState (..), NestedCtxt_ (NCS, NCZ),
                     PerEraConsensusConfig (PerEraConsensusConfig),
                     PerEraLedgerConfig (PerEraLedgerConfig),
                     WrapPartialConsensusConfig (WrapPartialConsensusConfig),
                     WrapPartialLedgerConfig (WrapPartialLedgerConfig),
                     hardForkBlockForging)
import           Ouroboros.Consensus.HardFork.Combinator.Embed.Nary
                     (injectInitialExtLedgerState)
import           Ouroboros.Consensus.HardFork.Combinator.Serialisation
                     (EraNodeToClientVersion (EraNodeToClientDisabled, EraNodeToClientEnabled),
                     EraNodeToNodeVersion (EraNodeToNodeDisabled, EraNodeToNodeEnabled),
                     HardForkNodeToClientVersion (HardForkNodeToClientDisabled, HardForkNodeToClientEnabled),
                     HardForkNodeToNodeVersion (HardForkNodeToNodeDisabled, HardForkNodeToNodeEnabled),
                     HardForkSpecificNodeToClientVersion (HardForkSpecificNodeToClientVersion1, HardForkSpecificNodeToClientVersion2),
                     HardForkSpecificNodeToNodeVersion (HardForkSpecificNodeToNodeVersion1),
                     SerialiseConstraintsHFC, SerialiseHFC (..))
import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.HeaderValidation (HeaderState)
import           Ouroboros.Consensus.Ledger.Extended (ExtLedgerState (..))
import qualified Ouroboros.Consensus.Mempool as Mempool
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
                     (HasNetworkProtocolVersion (BlockNodeToClientVersion, BlockNodeToNodeVersion),
                     NodeToClientVersion (NodeToClientV_10, NodeToClientV_11, NodeToClientV_12, NodeToClientV_13, NodeToClientV_14, NodeToClientV_15, NodeToClientV_9),
                     NodeToNodeVersion (NodeToNodeV_10, NodeToNodeV_11, NodeToNodeV_7, NodeToNodeV_8, NodeToNodeV_9),
                     SupportedNetworkProtocolVersion (..))
import           Ouroboros.Consensus.Node.ProtocolInfo (ProtocolClientInfo (..),
                     ProtocolInfo (..))
import           Ouroboros.Consensus.Node.Run
                     (SerialiseNodeToNodeConstraints (estimateBlockSize))
import qualified Ouroboros.Consensus.Protocol.Ledger.HotKey as HotKey
import           Ouroboros.Consensus.Protocol.Praos (Praos, PraosParams (..))
import           Ouroboros.Consensus.Protocol.Praos.Common
                     (praosCanBeLeaderOpCert)
import           Ouroboros.Consensus.Protocol.TPraos (TPraos, TPraosParams (..))
import qualified Ouroboros.Consensus.Protocol.TPraos as Shelley
import           Ouroboros.Consensus.Shelley.Ledger (LedgerState (..),
                     ShelleyBlock, ShelleyCompatible)
import qualified Ouroboros.Consensus.Shelley.Ledger as Shelley
import           Ouroboros.Consensus.Shelley.Ledger.NetworkProtocolVersion
                     (ShelleyNodeToClientVersion (ShelleyNodeToClientVersion1, ShelleyNodeToClientVersion2, ShelleyNodeToClientVersion3, ShelleyNodeToClientVersion4, ShelleyNodeToClientVersion5, ShelleyNodeToClientVersion6),
                     ShelleyNodeToNodeVersion (ShelleyNodeToNodeVersion1))
import           Ouroboros.Consensus.Shelley.Node (MaxMajorProtVer (..),
                     ProtVer (pvMajor), ProtocolParamsAllegra (..),
                     ProtocolParamsAlonzo (..), ProtocolParamsMary (..),
                     ProtocolParamsShelley (..),
                     ProtocolParamsShelleyBased (..), ShelleyGenesis (..),
                     ShelleyGenesisStaking (..), ShelleyLeaderCredentials (..),
                     protocolClientInfoShelley, registerGenesisStaking,
                     registerInitialFunds, validateGenesis)
import           Ouroboros.Consensus.Shelley.Node.Common (ShelleyEraWithCrypto,
                     shelleyBlockIssuerVKey, translateShelleyLeaderCredentials)
import Ouroboros.Consensus.Shelley.HFEras()

import           Cardano.Ledger.Address (BootstrapAddress (..), RewardAcnt (..))
import qualified Cardano.Ledger.BaseTypes as SL
import           Cardano.Ledger.CompactAddress (Addr (..))
import           Cardano.Ledger.Core (PParams (..))
import           Cardano.Ledger.Credential (StakeReference (..))
import           Cardano.Ledger.Crypto (Crypto (..))
import           Cardano.Ledger.Keys (GenDelegPair (..))
import           Cardano.Ledger.PoolParams (PoolParams (..))
import           Cardano.Ledger.Shelley.API (Credential (..))
import           Cardano.Slotting.Time (SystemStart (SystemStart))
import           Data.Coerce (coerce)
import qualified Data.Set as Set
import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Cardano.Block ()
import           Ouroboros.Consensus.Cardano.CanHardFork
import           Ouroboros.Consensus.Cardano.CanHardFork ()
import           Ouroboros.Consensus.Protocol.BatchCompatibleCrypto
                     (BatchCompatibleCrypto)
import           Ouroboros.Consensus.Protocol.Ledger.HotKey (HotKey (..))
import           Ouroboros.Consensus.Protocol.Praos (Praos, PraosCrypto,
                     PraosParams (..))
import           Ouroboros.Consensus.Protocol.Praos.Common
                     (praosCanBeLeaderOpCert, translateCanBeLeader)
import           Ouroboros.Consensus.Protocol.Praos.Crypto
                     (CanConvertVRF (convertVKey))
import           Ouroboros.Consensus.Shelley.Node.Praos
                     (ProtocolParamsBabbage (..), ProtocolParamsConway (..))
import qualified Ouroboros.Consensus.Shelley.Node.Praos as Praos
import qualified Ouroboros.Consensus.Shelley.Node.TPraos as TPraos
import           Ouroboros.Consensus.Storage.Serialisation
                     (BinaryBlockInfo (headerOffset), DecodeDisk (decodeDisk),
                     EncodeDisk (encodeDisk),
                     HasBinaryBlockInfo (getBinaryBlockInfo),
                     PrefixLen (PrefixLen))
import           Ouroboros.Consensus.Util.Assert (assertWithMsg)
import           Ouroboros.Consensus.Util.IOLike (IOLike)
import           Ouroboros.Network.Magic (NetworkMagic (..))

{-------------------------------------------------------------------------------
  SerialiseHFC
-------------------------------------------------------------------------------}

instance SerialiseConstraintsHFC ByronBlock

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
instance CardanoHardForkConstraints c1 c2 => SerialiseHFC (CardanoEras c1 c2) where
  encodeDiskHfcBlock (CardanoCodecConfig ccfgByron ccfgShelley ccfgAllegra ccfgMary ccfgAlonzo ccfgBabbage ccfgConway) = \case
      -- We are backwards compatible with Byron and thus use the exact same
      -- encoding.
      BlockByron   blockByron   ->                encodeDisk ccfgByron blockByron
      -- For Shelley and later eras, we need to prepend the hard fork envelope.
      BlockShelley blockShelley -> prependTag 2 $ encodeDisk ccfgShelley blockShelley
      BlockAllegra blockAllegra -> prependTag 3 $ encodeDisk ccfgAllegra blockAllegra
      BlockMary    blockMary    -> prependTag 4 $ encodeDisk ccfgMary    blockMary
      BlockAlonzo  blockAlonzo  -> prependTag 5 $ encodeDisk ccfgAlonzo  blockAlonzo
      BlockBabbage blockBabbage -> prependTag 6 $ encodeDisk ccfgBabbage blockBabbage
      BlockConway  blockConway  -> prependTag 7 $ encodeDisk ccfgConway  blockConway
  decodeDiskHfcBlock (CardanoCodecConfig ccfgByron ccfgShelley ccfgAllegra ccfgMary ccfgAlonzo ccfgBabbage ccfgConway) = do
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
        6 -> fmap BlockBabbage <$> decodeDisk ccfgBabbage
        7 -> fmap BlockConway  <$> decodeDisk ccfgConway
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
        6 -> SomeSecond $ NestedCtxt (NCS (NCS (NCS (NCS (NCS (NCZ Shelley.CtxtShelley))))))
        7 -> SomeSecond $ NestedCtxt (NCS (NCS (NCS (NCS (NCS (NCS (NCZ Shelley.CtxtShelley)))))))
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
      BlockBabbage blockBabbage ->
        shiftHeaderOffset 2 $ getBinaryBlockInfo blockBabbage
      BlockConway blockConway ->
        shiftHeaderOffset 2 $ getBinaryBlockInfo blockConway
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
      HeaderBabbage headerBabbage -> estimateBlockSize headerBabbage + 2
      HeaderConway  headerConway  -> estimateBlockSize headerConway  + 2

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
pattern CardanoNodeToNodeVersion1 :: BlockNodeToNodeVersion (CardanoBlock c1 c2)
pattern CardanoNodeToNodeVersion1 =
    HardForkNodeToNodeDisabled ByronNodeToNodeVersion1

-- | The hard fork enabled with the latest Byron version and the Shelley era
-- enabled.
pattern CardanoNodeToNodeVersion2 :: BlockNodeToNodeVersion (CardanoBlock c1 c2)
pattern CardanoNodeToNodeVersion2 =
    HardForkNodeToNodeEnabled
      HardForkSpecificNodeToNodeVersion1
      (  EraNodeToNodeEnabled ByronNodeToNodeVersion2
      :* EraNodeToNodeEnabled ShelleyNodeToNodeVersion1
      :* EraNodeToNodeDisabled
      :* EraNodeToNodeDisabled
      :* EraNodeToNodeDisabled
      :* EraNodeToNodeDisabled
      :* EraNodeToNodeDisabled
      :* Nil
      )

-- | The hard fork enabled with the latest Byron version, the Shelley and
-- Allegra eras enabled.
pattern CardanoNodeToNodeVersion3 :: BlockNodeToNodeVersion (CardanoBlock c1 c2)
pattern CardanoNodeToNodeVersion3 =
    HardForkNodeToNodeEnabled
      HardForkSpecificNodeToNodeVersion1
      (  EraNodeToNodeEnabled ByronNodeToNodeVersion2
      :* EraNodeToNodeEnabled ShelleyNodeToNodeVersion1
      :* EraNodeToNodeEnabled ShelleyNodeToNodeVersion1
      :* EraNodeToNodeDisabled
      :* EraNodeToNodeDisabled
      :* EraNodeToNodeDisabled
      :* EraNodeToNodeDisabled
      :* Nil
      )

-- | The hard fork enabled with the latest Byron version, the Shelley, Allegra,
-- and Mary eras enabled.
pattern CardanoNodeToNodeVersion4 :: BlockNodeToNodeVersion (CardanoBlock c1 c2)
pattern CardanoNodeToNodeVersion4 =
    HardForkNodeToNodeEnabled
      HardForkSpecificNodeToNodeVersion1
      (  EraNodeToNodeEnabled ByronNodeToNodeVersion2
      :* EraNodeToNodeEnabled ShelleyNodeToNodeVersion1
      :* EraNodeToNodeEnabled ShelleyNodeToNodeVersion1
      :* EraNodeToNodeEnabled ShelleyNodeToNodeVersion1
      :* EraNodeToNodeDisabled
      :* EraNodeToNodeDisabled
      :* EraNodeToNodeDisabled
      :* Nil
      )

-- | The hard fork enabled with the latest Byron version, the Shelley, Allegra,
-- Mary and Alonzo eras enabled.
pattern CardanoNodeToNodeVersion5 :: BlockNodeToNodeVersion (CardanoBlock c1 c2)
pattern CardanoNodeToNodeVersion5 =
    HardForkNodeToNodeEnabled
      HardForkSpecificNodeToNodeVersion1
      (  EraNodeToNodeEnabled ByronNodeToNodeVersion2
      :* EraNodeToNodeEnabled ShelleyNodeToNodeVersion1
      :* EraNodeToNodeEnabled ShelleyNodeToNodeVersion1
      :* EraNodeToNodeEnabled ShelleyNodeToNodeVersion1
      :* EraNodeToNodeEnabled ShelleyNodeToNodeVersion1
      :* EraNodeToNodeDisabled
      :* EraNodeToNodeDisabled
      :* Nil
      )

-- | The hard fork enabled with the latest Byron version, the Shelley, Allegra,
-- Mary, Alonzo and Babbage eras enabled.
pattern CardanoNodeToNodeVersion6 :: BlockNodeToNodeVersion (CardanoBlock c1 c2)
pattern CardanoNodeToNodeVersion6 =
    HardForkNodeToNodeEnabled
      HardForkSpecificNodeToNodeVersion1
      (  EraNodeToNodeEnabled ByronNodeToNodeVersion2
      :* EraNodeToNodeEnabled ShelleyNodeToNodeVersion1
      :* EraNodeToNodeEnabled ShelleyNodeToNodeVersion1
      :* EraNodeToNodeEnabled ShelleyNodeToNodeVersion1
      :* EraNodeToNodeEnabled ShelleyNodeToNodeVersion1
      :* EraNodeToNodeEnabled ShelleyNodeToNodeVersion1
      :* EraNodeToNodeDisabled
      :* Nil
      )

-- | The hard fork enabled with the latest Byron version, the Shelley, Allegra,
-- Mary, Alonzo, Babbage and Conway eras enabled.
pattern CardanoNodeToNodeVersion7 :: BlockNodeToNodeVersion (CardanoBlock c1 c2)
pattern CardanoNodeToNodeVersion7 =
    HardForkNodeToNodeEnabled
      HardForkSpecificNodeToNodeVersion1
      (  EraNodeToNodeEnabled ByronNodeToNodeVersion2
      :* EraNodeToNodeEnabled ShelleyNodeToNodeVersion1
      :* EraNodeToNodeEnabled ShelleyNodeToNodeVersion1
      :* EraNodeToNodeEnabled ShelleyNodeToNodeVersion1
      :* EraNodeToNodeEnabled ShelleyNodeToNodeVersion1
      :* EraNodeToNodeEnabled ShelleyNodeToNodeVersion1
      :* EraNodeToNodeEnabled ShelleyNodeToNodeVersion1
      :* Nil
      )

-- | We support the sole Byron version with the hard fork disabled.
pattern CardanoNodeToClientVersion1 :: BlockNodeToClientVersion (CardanoBlock c1 c2)
pattern CardanoNodeToClientVersion1 =
    HardForkNodeToClientDisabled ByronNodeToClientVersion1

-- | The hard fork enabled and the Shelley era enabled.
pattern CardanoNodeToClientVersion2 :: BlockNodeToClientVersion (CardanoBlock c1 c2)
pattern CardanoNodeToClientVersion2 =
    HardForkNodeToClientEnabled
      HardForkSpecificNodeToClientVersion1
      (  EraNodeToClientEnabled ByronNodeToClientVersion1
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion1
      :* EraNodeToClientDisabled
      :* EraNodeToClientDisabled
      :* EraNodeToClientDisabled
      :* EraNodeToClientDisabled
      :* EraNodeToClientDisabled
      :* Nil
      )

-- | The hard fork enabled and the Shelley era enabled, but using
-- 'ShelleyNodeToClientVersion2' and 'HardForkSpecificNodeToClientVersion2'.
pattern CardanoNodeToClientVersion3 :: BlockNodeToClientVersion (CardanoBlock c1 c2)
pattern CardanoNodeToClientVersion3 =
    HardForkNodeToClientEnabled
      HardForkSpecificNodeToClientVersion2
      (  EraNodeToClientEnabled ByronNodeToClientVersion1
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion2
      :* EraNodeToClientDisabled
      :* EraNodeToClientDisabled
      :* EraNodeToClientDisabled
      :* EraNodeToClientDisabled
      :* EraNodeToClientDisabled
      :* Nil
      )

-- | The hard fork enabled, and the Shelley and Allegra eras enabled.
--
-- We don't bother with 'ShelleyNodeToClientVersion1' and
-- 'HardForkSpecificNodeToClientVersion1'.
pattern CardanoNodeToClientVersion4 :: BlockNodeToClientVersion (CardanoBlock c1 c2)
pattern CardanoNodeToClientVersion4 =
    HardForkNodeToClientEnabled
      HardForkSpecificNodeToClientVersion2
      (  EraNodeToClientEnabled ByronNodeToClientVersion1
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion2
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion2
      :* EraNodeToClientDisabled
      :* EraNodeToClientDisabled
      :* EraNodeToClientDisabled
      :* EraNodeToClientDisabled
      :* Nil
      )

-- | The hard fork enabled, and the Shelley, Allegra, and Mary eras enabled.
--
-- We don't bother with 'ShelleyNodeToClientVersion1'.
pattern CardanoNodeToClientVersion5 :: BlockNodeToClientVersion (CardanoBlock c1 c2)
pattern CardanoNodeToClientVersion5 =
    HardForkNodeToClientEnabled
      HardForkSpecificNodeToClientVersion2
      (  EraNodeToClientEnabled ByronNodeToClientVersion1
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion2
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion2
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion2
      :* EraNodeToClientDisabled
      :* EraNodeToClientDisabled
      :* EraNodeToClientDisabled
      :* Nil
      )

-- | The hard fork enabled, and the Shelley, Allegra, and Mary eras enabled, but
-- using 'ShelleyNodeToClientVersion3' for the Shelley-based eras , which
-- enables new queries.
pattern CardanoNodeToClientVersion6 :: BlockNodeToClientVersion (CardanoBlock c1 c2)
pattern CardanoNodeToClientVersion6 =
    HardForkNodeToClientEnabled
      HardForkSpecificNodeToClientVersion2
      (  EraNodeToClientEnabled ByronNodeToClientVersion1
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion3
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion3
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion3
      :* EraNodeToClientDisabled
      :* EraNodeToClientDisabled
      :* EraNodeToClientDisabled
      :* Nil
      )

-- | The hard fork enabled, and the Shelley, Allegra, Mary and Alonzo eras enabled
pattern CardanoNodeToClientVersion7 :: BlockNodeToClientVersion (CardanoBlock c1 c2)
pattern CardanoNodeToClientVersion7 =
    HardForkNodeToClientEnabled
      HardForkSpecificNodeToClientVersion2
      (  EraNodeToClientEnabled ByronNodeToClientVersion1
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion4
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion4
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion4
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion4
      :* EraNodeToClientDisabled
      :* EraNodeToClientDisabled
      :* Nil
      )

-- | The hard fork enabled, and the Shelley, Allegra, Mary and Alonzo eras enabled
-- Using 'ShelleyNodeToClientVersion5' for the Shelley-based eras , which
-- enables new queries.
pattern CardanoNodeToClientVersion8 :: BlockNodeToClientVersion (CardanoBlock c1 c2)
pattern CardanoNodeToClientVersion8 =
    HardForkNodeToClientEnabled
      HardForkSpecificNodeToClientVersion2
      (  EraNodeToClientEnabled ByronNodeToClientVersion1
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion5
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion5
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion5
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion5
      :* EraNodeToClientDisabled
      :* EraNodeToClientDisabled
      :* Nil
      )

-- | The hard fork enabled, and the Shelley, Allegra, Mary, Alonzo and Babbage
-- eras enabled Using 'ShelleyNodeToClientVersion5' for the Shelley-based eras,
-- which enables new queries.
pattern CardanoNodeToClientVersion9 :: BlockNodeToClientVersion (CardanoBlock c1 c2)
pattern CardanoNodeToClientVersion9 =
    HardForkNodeToClientEnabled
      HardForkSpecificNodeToClientVersion2
      (  EraNodeToClientEnabled ByronNodeToClientVersion1
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion5
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion5
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion5
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion5
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion5
      :* EraNodeToClientDisabled
      :* Nil
      )

-- | The hard fork enabled, and the Shelley, Allegra, Mary, Alonzo and Babbage
-- eras enabled Using 'ShelleyNodeToClientVersion6' for the Shelley-based eras,
-- which enables new queries.
pattern CardanoNodeToClientVersion10 :: BlockNodeToClientVersion (CardanoBlock c1 c2)
pattern CardanoNodeToClientVersion10 =
    HardForkNodeToClientEnabled
      HardForkSpecificNodeToClientVersion2
      (  EraNodeToClientEnabled ByronNodeToClientVersion1
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion6
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion6
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion6
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion6
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion6
      :* EraNodeToClientDisabled
      :* Nil
      )

-- | The hard fork enabled, and the Shelley, Allegra, Mary, Alonzo, Babbage and
-- Conway eras enabled Using 'ShelleyNodeToClientVersion6' for the
-- Shelley-based eras, which enables new queries.
pattern CardanoNodeToClientVersion11 :: BlockNodeToClientVersion (CardanoBlock c1 c2)
pattern CardanoNodeToClientVersion11 =
    HardForkNodeToClientEnabled
      HardForkSpecificNodeToClientVersion2
      (  EraNodeToClientEnabled ByronNodeToClientVersion1
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion6
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion6
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion6
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion6
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion6
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion6
      :* Nil
      )

instance CardanoHardForkConstraints c1 c2
      => SupportedNetworkProtocolVersion (CardanoBlock c1 c2) where
  supportedNodeToNodeVersions _ = Map.fromList
      [ (NodeToNodeV_7, CardanoNodeToNodeVersion5)
      , (NodeToNodeV_8, CardanoNodeToNodeVersion5)
      , (NodeToNodeV_9, CardanoNodeToNodeVersion6)
      , (NodeToNodeV_10, CardanoNodeToNodeVersion6)
      , (NodeToNodeV_11, CardanoNodeToNodeVersion7)
      ]

  supportedNodeToClientVersions _ = Map.fromList
      [ (NodeToClientV_9 , CardanoNodeToClientVersion7)
      , (NodeToClientV_10, CardanoNodeToClientVersion7)
      , (NodeToClientV_11, CardanoNodeToClientVersion8)
      , (NodeToClientV_12, CardanoNodeToClientVersion8)
      , (NodeToClientV_13, CardanoNodeToClientVersion9)
      , (NodeToClientV_14, CardanoNodeToClientVersion10)
      , (NodeToClientV_15, CardanoNodeToClientVersion11)
      ]

  latestReleasedNodeVersion _prx = (Just NodeToNodeV_10, Just NodeToClientV_14)

{-------------------------------------------------------------------------------
  ProtocolInfo
-------------------------------------------------------------------------------}

-- | Parameters needed to transition to a Shelley era.
data ProtocolTransitionParamsShelleyBased era = ProtocolTransitionParamsShelleyBased {
      transitionTranslationContext :: Core.TranslationContext era
    , transitionTrigger            :: TriggerHardFork
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
     forall c1 c2 m. (IOLike m, KES c1 ~ KES c2, CardanoHardForkConstraints c1 c2)
  => ProtocolParamsByron
  -> ProtocolParamsShelleyBased (ShelleyEra c1)
  -> ProtocolParamsShelley c1
  -> ProtocolParamsAllegra c1
  -> ProtocolParamsMary    c1
  -> ProtocolParamsAlonzo  c1
  -> ProtocolParamsBabbage c1
  -> ProtocolParamsConway  c2
  -> ProtocolTransitionParamsShelleyBased (ShelleyEra c1)
  -> ProtocolTransitionParamsShelleyBased (AllegraEra c1)
  -> ProtocolTransitionParamsShelleyBased (MaryEra c1)
  -> ProtocolTransitionParamsShelleyBased (AlonzoEra c1)
  -> ProtocolTransitionParamsShelleyBased (BabbageEra c1)
  -> ProtocolTransitionParamsShelleyBased (ConwayEra c2)
  -> ProtocolInfo m (CardanoBlock c1 c2)
protocolInfoCardano protocolParamsByron@ProtocolParamsByron {
                        byronGenesis                = genesisByron
                      , byronLeaderCredentials      = mCredsByron
                      , byronMaxTxCapacityOverrides = maxTxCapacityOverridesByron
                      }
                    ProtocolParamsShelleyBased {
                        shelleyBasedGenesis           = genesisShelley
                      , shelleyBasedInitialNonce      = initialNonceShelley
                      , shelleyBasedLeaderCredentials = credssShelleyBased
                      }
                    ProtocolParamsShelley {
                        shelleyProtVer                = protVerShelley
                      , shelleyMaxTxCapacityOverrides = maxTxCapacityOverridesShelley
                      }
                    ProtocolParamsAllegra {
                        allegraProtVer                = protVerAllegra
                      , allegraMaxTxCapacityOverrides = maxTxCapacityOverridesAllegra
                      }
                    ProtocolParamsMary {
                        maryProtVer                = protVerMary
                      , maryMaxTxCapacityOverrides = maxTxCapacityOverridesMary
                      }
                    ProtocolParamsAlonzo {
                        alonzoProtVer                = protVerAlonzo
                      , alonzoMaxTxCapacityOverrides = maxTxCapacityOverridesAlonzo
                      }
                    ProtocolParamsBabbage {
                        babbageProtVer                = protVerBabbage
                      , babbageMaxTxCapacityOverrides = maxTxCapacityOverridesBabbage
                      }
                    ProtocolParamsConway {
                        conwayProtVer                = protVerConway
                      , conwayMaxTxCapacityOverrides = maxTxCapacityOverridesConway
                      }
                    ProtocolTransitionParamsShelleyBased {
                        transitionTranslationContext = transCtxtShelley
                      , transitionTrigger            = triggerHardForkShelley
                      }
                    ProtocolTransitionParamsShelleyBased {
                        transitionTranslationContext = ()
                      , transitionTrigger            = triggerHardForkAllegra
                      }
                    ProtocolTransitionParamsShelleyBased {
                        transitionTranslationContext = ()
                      , transitionTrigger            = triggerHardForkMary
                      }
                    ProtocolTransitionParamsShelleyBased {
                        transitionTranslationContext = transCtxtAlonzo
                      , transitionTrigger            = triggerHardForkAlonzo
                      }
                    ProtocolTransitionParamsShelleyBased {
                        transitionTranslationContext = transCtxtBabbage
                      , transitionTrigger            = triggerHardForkBabbage
                      }
                    ProtocolTransitionParamsShelleyBased {
                        transitionTranslationContext = transCtxtConway
                      , transitionTrigger            = triggerHardForkConway
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
    maxMajorProtVer =
          MaxMajorProtVer
        $ pvMajor
        $ nonEmptyLast
        $ exactlyWeakenNonEmpty protVers
      where
        protVers :: Exactly (CardanoShelleyEras StandardCrypto BatchCompatibleCrypto) ProtVer
        protVers = Exactly $
          -- ensure that these have the same order as 'CardanoShelleyEras'!
          K protVerShelley :*
          K protVerAllegra :*
          K protVerMary :*
          K protVerAlonzo :*
          K protVerBabbage :*
          K protVerConway :*
          Nil

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
        , byronTriggerHardFork = triggerHardForkShelley
        }

    kByron :: SecurityParam
    kByron = Byron.genesisSecurityParam genesisByron

    -- Shelley

    tpraosParams :: TPraosParams
    tpraosParams =
        Shelley.mkTPraosParams
          maxMajorProtVer
          initialNonceShelley
          genesisShelley

    TPraosParams { tpraosSlotsPerKESPeriod, tpraosMaxKESEvo } = tpraosParams

    systemStart = SystemStart $ SL.sgSystemStart genesisShelley

    networkMagic = NetworkMagic $ SL.sgNetworkMagic genesisShelley

    praosParams :: PraosParams
    praosParams = PraosParams
      { praosSlotsPerKESPeriod = SL.sgSlotsPerKESPeriod genesisShelley,
        praosLeaderF = SL.mkActiveSlotCoeff $ SL.sgActiveSlotsCoeff genesisShelley,
        praosSecurityParam = SecurityParam $ SL.sgSecurityParam genesisShelley,
        praosMaxKESEvo = SL.sgMaxKESEvolutions genesisShelley,
        praosQuorum = SL.sgUpdateQuorum genesisShelley,
        praosMaxMajorPV = maxMajorProtVer,
        praosMaxLovelaceSupply = SL.sgMaxLovelaceSupply genesisShelley,
        praosNetworkId = SL.sgNetworkId genesisShelley,
        praosSystemStart = systemStart
      }

    PraosParams { praosSlotsPerKESPeriod, praosMaxKESEvo } = praosParams

    blockConfigShelley :: BlockConfig (ShelleyBlock (TPraos c1) (ShelleyEra c1))
    blockConfigShelley =
        Shelley.mkShelleyBlockConfig
          protVerShelley
          systemStart
          networkMagic
          (shelleyBlockIssuerVKey <$> credssShelleyBased)

    partialConsensusConfigShelley ::
         PartialConsensusConfig (BlockProtocol (ShelleyBlock (TPraos c1) (ShelleyEra c1)))
    partialConsensusConfigShelley = tpraosParams

    partialLedgerConfigShelley :: PartialLedgerConfig (ShelleyBlock (TPraos c1) (ShelleyEra c1))
    partialLedgerConfigShelley =
        mkPartialLedgerConfigShelley
          genesisShelley
          transCtxtShelley
          maxMajorProtVer
          triggerHardForkAllegra

    kShelley :: SecurityParam
    kShelley = SecurityParam $ sgSecurityParam genesisShelley

    -- Allegra

    blockConfigAllegra :: BlockConfig (ShelleyBlock (TPraos c1) (AllegraEra c1))
    blockConfigAllegra =
        Shelley.mkShelleyBlockConfig
          protVerAllegra
          systemStart
          networkMagic
          (shelleyBlockIssuerVKey <$> credssShelleyBased)

    partialConsensusConfigAllegra ::
         PartialConsensusConfig (BlockProtocol (ShelleyBlock (TPraos c1) (AllegraEra c1)))
    partialConsensusConfigAllegra = tpraosParams

    partialLedgerConfigAllegra :: PartialLedgerConfig (ShelleyBlock (TPraos c1) (AllegraEra c1))
    partialLedgerConfigAllegra =
        mkPartialLedgerConfigShelley
          genesisShelley
          ()  -- trivial translation context
          maxMajorProtVer
          triggerHardForkMary

    -- Mary

    blockConfigMary :: BlockConfig (ShelleyBlock (TPraos c1) (MaryEra c1))
    blockConfigMary =
        Shelley.mkShelleyBlockConfig
          protVerMary
          systemStart
          networkMagic
          (shelleyBlockIssuerVKey <$> credssShelleyBased)

    partialConsensusConfigMary ::
         PartialConsensusConfig (BlockProtocol (ShelleyBlock (TPraos c1) (MaryEra c1)))
    partialConsensusConfigMary = tpraosParams

    partialLedgerConfigMary :: PartialLedgerConfig (ShelleyBlock (TPraos c1) (MaryEra c1))
    partialLedgerConfigMary =
        mkPartialLedgerConfigShelley
          genesisShelley
          ()  -- trivial translation context
          maxMajorProtVer
          triggerHardForkAlonzo

    -- Alonzo

    blockConfigAlonzo :: BlockConfig (ShelleyBlock (TPraos c1) (AlonzoEra c1))
    blockConfigAlonzo =
        Shelley.mkShelleyBlockConfig
          protVerAlonzo
          systemStart
          networkMagic
          (shelleyBlockIssuerVKey <$> credssShelleyBased)

    partialConsensusConfigAlonzo ::
         PartialConsensusConfig (BlockProtocol (ShelleyBlock (TPraos c1) (AlonzoEra c1)))
    partialConsensusConfigAlonzo = tpraosParams

    partialLedgerConfigAlonzo :: PartialLedgerConfig (ShelleyBlock (TPraos c1) (AlonzoEra c1))
    partialLedgerConfigAlonzo =
        mkPartialLedgerConfigShelley
          genesisShelley
          transCtxtAlonzo
          maxMajorProtVer
          triggerHardForkBabbage

    -- Babbage

    blockConfigBabbage :: BlockConfig (ShelleyBlock (Praos c1) (BabbageEra c1))
    blockConfigBabbage =
        Shelley.mkShelleyBlockConfig
          protVerBabbage
          systemStart
          networkMagic
          (shelleyBlockIssuerVKey <$> credssShelleyBased)

    partialConsensusConfigBabbage ::
         PartialConsensusConfig (BlockProtocol (ShelleyBlock (Praos c1) (BabbageEra c1)))
    partialConsensusConfigBabbage = praosParams

    partialLedgerConfigBabbage :: PartialLedgerConfig (ShelleyBlock (Praos c1) (BabbageEra c1))
    partialLedgerConfigBabbage =
        mkPartialLedgerConfigShelley
          genesisShelley
          transCtxtBabbage
          maxMajorProtVer
          triggerHardForkConway

    -- Conway

    genesisShelleyConway :: ShelleyGenesis c2
    genesisShelleyConway = translateShelleyGenesis genesisShelley

    blockConfigConway :: BlockConfig (ShelleyBlock (Praos c2) (ConwayEra c2))
    blockConfigConway =
        Shelley.mkShelleyBlockConfig
          protVerConway
          systemStart
          networkMagic
          (shelleyBlockIssuerVKey . translateShelleyLeaderCredentials <$> credssShelleyBased)

    partialConsensusConfigConway ::
         PartialConsensusConfig (BlockProtocol (ShelleyBlock (Praos c2) (ConwayEra c2)))
    partialConsensusConfigConway = praosParams

    partialLedgerConfigConway :: PartialLedgerConfig (ShelleyBlock (Praos c2) (ConwayEra c2))
    partialLedgerConfigConway =
        mkPartialLedgerConfigShelley
          genesisShelleyConway
          transCtxtConway
          maxMajorProtVer
          TriggerHardForkNever

    -- Cardano

    k :: SecurityParam
    k = assert (kByron == kShelley) kByron

    shape :: History.Shape (CardanoEras c1 c2)
    shape = History.Shape $ Exactly $
           K (Byron.byronEraParams     genesisByron)
        :* K (Shelley.shelleyEraParams genesisShelley)
        :* K (Shelley.shelleyEraParams genesisShelley)
        :* K (Shelley.shelleyEraParams genesisShelley)
        :* K (Shelley.shelleyEraParams genesisShelley)
        :* K (Shelley.shelleyEraParams genesisShelley)
        :* K (Shelley.shelleyEraParams genesisShelley)
        :* Nil

    cfg :: TopLevelConfig (CardanoBlock c1 c2)
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
              :* WrapPartialConsensusConfig partialConsensusConfigBabbage
              :* WrapPartialConsensusConfig partialConsensusConfigConway
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
              :* WrapPartialLedgerConfig partialLedgerConfigBabbage
              :* WrapPartialLedgerConfig partialLedgerConfigConway
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
            blockConfigBabbage
            blockConfigConway
      , topLevelConfigCodec =
          CardanoCodecConfig
            (configCodec topLevelConfigByron)
            Shelley.ShelleyCodecConfig
            Shelley.ShelleyCodecConfig
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
            (Shelley.ShelleyStorageConfig tpraosSlotsPerKESPeriod k)
            (Shelley.ShelleyStorageConfig tpraosSlotsPerKESPeriod k)
      }

    -- When the initial ledger state is not in the Byron era, register the
    -- initial staking and initial funds (if provided in the genesis config) in
    -- the ledger state.
    initExtLedgerStateCardano :: ExtLedgerState (CardanoBlock c1 c2)
    initExtLedgerStateCardano = ExtLedgerState {
          headerState = initHeaderState
        , ledgerState = register (SL.sgStaking genesisShelley) (SL.sgInitialFunds genesisShelley) initLedgerState
        }
      where
        initHeaderState :: HeaderState (CardanoBlock c1 c2)
        initLedgerState :: LedgerState (CardanoBlock c1 c2)
        ExtLedgerState initLedgerState initHeaderState =
          injectInitialExtLedgerState cfg initExtLedgerStateByron

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
    blockForging :: m [BlockForging m (CardanoBlock c1 c2)]
    blockForging = do
        shelleyBased <- traverse blockForgingShelleyBased credssShelleyBased
        let blockForgings :: [NonEmptyOptNP (BlockForging m) (CardanoEras c1 c2)]
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

    mBlockForgingByron :: Maybe (NonEmptyOptNP (BlockForging m) (CardanoEras c1 c2))
    mBlockForgingByron = do
        creds <- mCredsByron
        return $ byronBlockForging maxTxCapacityOverridesByron creds `OptNP.at` IZ

    blockForgingShelleyBased ::
         ShelleyLeaderCredentials c1
      -> m (NonEmptyOptNP (BlockForging m) (CardanoEras c1 c2))
    blockForgingShelleyBased credentials = do
        let ShelleyLeaderCredentials
              { shelleyLeaderCredentialsInitSignKey = initSignKey
              , shelleyLeaderCredentialsCanBeLeader = canBeLeader
              } = credentials

        hotKey <- do
          let maxKESEvo :: Word64
              maxKESEvo = assert (tpraosMaxKESEvo == praosMaxKESEvo) praosMaxKESEvo

              startPeriod :: Absolute.KESPeriod
              startPeriod = Absolute.ocertKESPeriod $ praosCanBeLeaderOpCert canBeLeader

          HotKey.mkHotKey @m @c1 initSignKey startPeriod maxKESEvo

        let slotToPeriod :: SlotNo -> Absolute.KESPeriod
            slotToPeriod (SlotNo slot) = assert (tpraosSlotsPerKESPeriod == praosSlotsPerKESPeriod) $
              Absolute.KESPeriod $ fromIntegral $ slot `div` praosSlotsPerKESPeriod

        let tpraos :: forall era.
                 ShelleyEraWithCrypto c1 (TPraos c1) era
              => Mempool.TxOverrides (ShelleyBlock (TPraos c1) era)
              -> BlockForging m      (ShelleyBlock (TPraos c1) era)
            tpraos maxTxCapacityOverrides =
              TPraos.shelleySharedBlockForging hotKey slotToPeriod credentials maxTxCapacityOverrides

        let praos :: forall c era.
                 ShelleyEraWithCrypto c (Praos c) era
              => ShelleyLeaderCredentials c
              -> HotKey.HotKey c m
              -> Mempool.TxOverrides (ShelleyBlock (Praos c) era)
              -> BlockForging m      (ShelleyBlock (Praos c) era)
            praos creds hk maxTxCapacityOverrides =
              Praos.praosSharedBlockForging hk slotToPeriod creds maxTxCapacityOverrides

        pure
          $ OptSkip    -- Byron
          $ OptNP.fromNonEmptyNP $
            tpraos maxTxCapacityOverridesShelley :*
            tpraos maxTxCapacityOverridesAllegra :*
            tpraos maxTxCapacityOverridesMary    :*
            tpraos maxTxCapacityOverridesAlonzo  :*
            praos credentials hotKey maxTxCapacityOverridesBabbage :*
            praos (translateCredentials credentials) (translateHotKey hotKey) maxTxCapacityOverridesConway  :*
            Nil

register ::
  forall c1 c2 .
  (PraosCrypto c1, PraosCrypto c2, ADDRHASH c1 ~ ADDRHASH c2, HASH c1 ~ HASH c2)
  => ShelleyGenesisStaking c1
  -> ListMap.ListMap (Addr c1) SL.Coin
  -> LedgerState (CardanoBlock c1 c2)
  -> LedgerState (CardanoBlock c1 c2)
register
  staking
  initialFunds
  (HardForkLedgerState st) = HardForkLedgerState $ hap fs st
  where
    fs :: NP (LedgerState -.-> LedgerState)
             (CardanoEras c1 c2)
    fs = fn id
        :* injectInC1
        :* injectInC1
        :* injectInC1
        :* injectInC1
        :* injectInC1
        :* injectInC2
        :* Nil

    injectInC1 ::
         (EraCrypto era ~ c1, ShelleyCompatible proto era)
      => (LedgerState -.-> LedgerState) (ShelleyBlock proto era)
    injectInC1 = fn (updateState staking initialFunds)

    injectInC2 ::
         (EraCrypto era ~ c2, ShelleyCompatible proto era)
      => (LedgerState -.-> LedgerState) (ShelleyBlock proto era)
    injectInC2 = fn (updateState (translateStaking staking) (translateInitialFunds initialFunds))

    updateState ::
      ShelleyBasedEra era
      => ShelleyGenesisStaking (EraCrypto era)
      -> ListMap.ListMap (Addr (EraCrypto era)) SL.Coin
      -> LedgerState (ShelleyBlock proto era)
      -> LedgerState (ShelleyBlock proto era)
    updateState staking initialFunds (Shelley.ShelleyLedgerState tip st trs) =
      Shelley.ShelleyLedgerState { shelleyLedgerTip = tip
                                 , shelleyLedgerTransition = trs
                                 , shelleyLedgerState =
                                     registerGenesisStaking staking
                                   . registerInitialFunds (ListMap.toMap initialFunds)
                                   $ st
                                 }


translateHotKey :: (KES c1 ~ KES c2) => HotKey.HotKey c1 m -> HotKey.HotKey c2 m
translateHotKey HotKey.HotKey {evolve, getInfo, isPoisoned, sign_} =
  HotKey.HotKey {evolve, getInfo, isPoisoned, sign_}

translateCredentials :: (DSIGN c1 ~ DSIGN c2, KES c1 ~ KES c2, CanConvertVRF (VRF c1) (VRF c2)) => ShelleyLeaderCredentials c1 -> ShelleyLeaderCredentials c2
translateCredentials
  ShelleyLeaderCredentials {shelleyLeaderCredentialsInitSignKey,
                            shelleyLeaderCredentialsCanBeLeader, shelleyLeaderCredentialsLabel}
  = ShelleyLeaderCredentials { shelleyLeaderCredentialsInitSignKey
                             , shelleyLeaderCredentialsCanBeLeader = translateCanBeLeader shelleyLeaderCredentialsCanBeLeader
                             , shelleyLeaderCredentialsLabel}


translateShelleyGenesis ::
  forall c1 c2 .
  (ADDRHASH c1 ~ ADDRHASH c2, HASH c1 ~ HASH c2)
  => ShelleyGenesis c1 -> ShelleyGenesis c2
translateShelleyGenesis
  ShelleyGenesis {sgSystemStart, sgNetworkMagic, sgNetworkId,
                  sgActiveSlotsCoeff, sgSecurityParam, sgEpochLength,
                  sgSlotsPerKESPeriod, sgMaxKESEvolutions, sgSlotLength,
                  sgUpdateQuorum, sgMaxLovelaceSupply, sgProtocolParams, sgGenDelegs,
                  sgInitialFunds, sgStaking}
  = ShelleyGenesis {sgSystemStart, sgNetworkMagic, sgNetworkId,
                  sgActiveSlotsCoeff, sgSecurityParam, sgEpochLength,
                  sgSlotsPerKESPeriod, sgMaxKESEvolutions, sgSlotLength,
                  sgUpdateQuorum, sgMaxLovelaceSupply, sgProtocolParams = translatePParams sgProtocolParams, sgGenDelegs = translateGenDelegs sgGenDelegs,
                  sgInitialFunds = translateInitialFunds sgInitialFunds, sgStaking = translateStaking sgStaking}

translateStaking ::
  forall c1 c2 .
  (ADDRHASH c1 ~ ADDRHASH c2, HASH c1 ~ HASH c2)
  => ShelleyGenesisStaking c1 -> ShelleyGenesisStaking c2
translateStaking ShelleyGenesisStaking {sgsPools, sgsStake} = ShelleyGenesisStaking { sgsPools = translateStakingPools sgsPools, sgsStake = translateStake sgsStake }
  where
    translateStake :: ListMap.ListMap (SL.KeyHash 'SL.Staking c1) (SL.KeyHash 'SL.StakePool c1) -> ListMap.ListMap (SL.KeyHash 'SL.Staking c2) (SL.KeyHash 'SL.StakePool c2)
    translateStake = ListMap.foldrWithKey translateStakingPair mempty

    translateStakingPair ::
      (SL.KeyHash 'SL.Staking c1, SL.KeyHash 'SL.StakePool c1)
      -> ListMap.ListMap (SL.KeyHash 'SL.Staking c2) (SL.KeyHash 'SL.StakePool c2)
      -> ListMap.ListMap (SL.KeyHash 'SL.Staking c2) (SL.KeyHash 'SL.StakePool c2)
    translateStakingPair (stakeKey, stakePool) (ListMap.ListMap stakes) = ListMap.ListMap $ (coerce stakeKey, coerce stakePool) : stakes

translateStakingPools ::
  forall c1 c2 .
  (ADDRHASH c1 ~ ADDRHASH c2, HASH c1 ~ HASH c2)
  => ListMap.ListMap (SL.KeyHash 'SL.StakePool c1) (SL.PoolParams c1)
  -> ListMap.ListMap (SL.KeyHash 'SL.StakePool c2) (SL.PoolParams c2)
translateStakingPools = ListMap.foldrWithKey f mempty
 where
   f :: (SL.KeyHash 'SL.StakePool c1, SL.PoolParams c1)
     -> ListMap.ListMap (SL.KeyHash 'SL.StakePool c2) (SL.PoolParams c2)
     -> ListMap.ListMap (SL.KeyHash 'SL.StakePool c2) (SL.PoolParams c2)
   f (kh, params) (ListMap.ListMap ps) = ListMap.ListMap $ (coerce kh, translatePoolParams params) : ps

   translatePoolParams :: SL.PoolParams c1 -> SL.PoolParams c2
   translatePoolParams
     PoolParams {ppId, ppVrf, ppPledge, ppCost, ppMargin, ppRewardAcnt,
                 ppOwners, ppRelays, ppMetadata}
     = PoolParams {ppId = coerce ppId, ppVrf = coerce ppVrf, ppPledge, ppCost, ppMargin, ppRewardAcnt = translateRewardAcnt ppRewardAcnt,
                 ppOwners = Set.map coerce ppOwners, ppRelays, ppMetadata}

   translateRewardAcnt :: SL.RewardAcnt c1 -> SL.RewardAcnt c2
   translateRewardAcnt (RewardAcnt net cre) = RewardAcnt net (translateCredential cre)

translateCredential ::
  forall c1 c2 r .
  (ADDRHASH c1 ~ ADDRHASH c2)
  => SL.Credential r c1 -> SL.Credential r c2
translateCredential (ScriptHashObj sh) = ScriptHashObj $ coerce sh
translateCredential (KeyHashObj kh)    = KeyHashObj $ coerce kh

translateInitialFunds ::
  forall c1 c2 .
  (ADDRHASH c1 ~ ADDRHASH c2)
  => ListMap.ListMap (SL.Addr c1) SL.Coin -> ListMap.ListMap (SL.Addr c2) SL.Coin
translateInitialFunds = ListMap.mapKeys translateAddr
  where
    translateAddr :: SL.Addr c1 -> SL.Addr c2
    translateAddr (Addr net cre sr) = Addr net (translateCredential cre) (translateStakeReference sr)
    translateAddr (AddrBootstrap ba) = AddrBootstrap (translateBootstrap ba)

    translateStakeReference :: SL.StakeReference c1 -> SL.StakeReference c2
    translateStakeReference (StakeRefBase cre) = StakeRefBase (translateCredential cre)
    translateStakeReference (StakeRefPtr ptr) = StakeRefPtr ptr
    translateStakeReference StakeRefNull = StakeRefNull

    translateBootstrap :: BootstrapAddress c1 -> BootstrapAddress c2
    translateBootstrap (BootstrapAddress ad) = BootstrapAddress ad

translateGenDelegs ::
  forall c1 c2 .
  (HASH c1 ~ HASH c2, ADDRHASH c1 ~ ADDRHASH c2 )
  => Map.Map (SL.KeyHash 'SL.Genesis c1) (SL.GenDelegPair c1)
  -> Map.Map (SL.KeyHash 'SL.Genesis c2) (SL.GenDelegPair c2)
translateGenDelegs = Map.map g . Map.mapKeysMonotonic f
 where
  g :: SL.GenDelegPair c1 -> SL.GenDelegPair c2
  g GenDelegPair {genDelegKeyHash, genDelegVrfHash} =
    GenDelegPair {genDelegKeyHash = coerce genDelegKeyHash, genDelegVrfHash = coerce genDelegVrfHash}

  f :: SL.KeyHash 'SL.Genesis c1 -> SL.KeyHash 'SL.Genesis c2
  f = coerce

translatePParams :: PParams (ShelleyEra c1) -> PParams (ShelleyEra c2)
translatePParams (PParams pph) = PParams (coerce pph)

protocolClientInfoCardano
  :: forall c1 c2.
     -- Byron
     EpochSlots
  -> ProtocolClientInfo (CardanoBlock c1 c2)
protocolClientInfoCardano epochSlots = ProtocolClientInfo {
      pClientInfoCodecConfig =
        CardanoCodecConfig
          (pClientInfoCodecConfig (protocolClientInfoByron epochSlots))
          (pClientInfoCodecConfig protocolClientInfoShelley)
          (pClientInfoCodecConfig protocolClientInfoShelley)
          (pClientInfoCodecConfig protocolClientInfoShelley)
          (pClientInfoCodecConfig protocolClientInfoShelley)
          (pClientInfoCodecConfig protocolClientInfoShelley)
          (pClientInfoCodecConfig protocolClientInfoShelley)
    }

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

mkPartialLedgerConfigShelley ::
     ShelleyGenesis (EraCrypto era)
  -> Core.TranslationContext era
  -> MaxMajorProtVer
  -> TriggerHardFork
  -> PartialLedgerConfig (ShelleyBlock proto era)
mkPartialLedgerConfigShelley
  genesisShelley
  transCtxt
  maxMajorProtVer
  shelleyTriggerHardFork =
    ShelleyPartialLedgerConfig {
          shelleyLedgerConfig =
            Shelley.mkShelleyLedgerConfig
              genesisShelley
              transCtxt
              -- 'completeLedgerConfig' will replace the 'History.dummyEpochInfo'
              -- in the partial ledger config with the correct one.
              History.dummyEpochInfo
              maxMajorProtVer
        , shelleyTriggerHardFork = shelleyTriggerHardFork
        }
