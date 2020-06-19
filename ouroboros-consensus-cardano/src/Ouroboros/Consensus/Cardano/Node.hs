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
  , HardCodedTransition (..)
    -- * TranslateNetworkProtocolVersion
  , pattern CardanoNodeToNodeVersion1
  , pattern CardanoNodeToNodeVersion2
  , pattern CardanoNodeToClientVersion1
  , pattern CardanoNodeToClientVersion2
  , pattern CardanoNodeToClientVersion3
  ) where

import qualified Codec.CBOR.Decoding as CBOR
import           Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as CBOR
import           Control.Exception (assert)
import qualified Data.ByteString.Short as Short
import           Data.Functor.Contravariant (contramap)
import qualified Data.List.NonEmpty as NE
import           Data.SOP.Strict (NP (..), hd, unK)
import           Data.Word (Word16)

import           Cardano.Binary (DecoderError (..), enforceSize)
import qualified Cardano.Chain.Genesis as Genesis
import           Cardano.Chain.Slotting (EpochSlots)
import qualified Cardano.Chain.Update as Update
import           Cardano.Prelude (Natural, cborError)
import           Cardano.Slotting.Slot (EpochNo)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Config.SecurityParam
import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Storage.ChainDB.Serialisation
import           Ouroboros.Consensus.Storage.Common (BinaryBlockInfo (..))
import           Ouroboros.Consensus.Storage.ImmutableDB (simpleChunkInfo)
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util.Counting (exactlyTwo)
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.SOP (OptNP (..))

import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.Forge
                     (undistribMaintainForgeState)
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
import           Ouroboros.Consensus.Shelley.Protocol (TPraosCrypto,
                     TPraosParams (..))
import qualified Ouroboros.Consensus.Shelley.Protocol as Shelley

import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Cardano.CanHardFork

{-------------------------------------------------------------------------------
  SerialiseHFC
-------------------------------------------------------------------------------}

instance SerialiseConstraintsHFC ByronBlock
instance TPraosCrypto sc => SerialiseConstraintsHFC (ShelleyBlock sc)

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
-- 3. Goguen block
-- 4. etc.
--
-- For more details, see:
-- <https://github.com/input-output-hk/ouroboros-network/pull/1175#issuecomment-558147194>
instance TPraosCrypto sc => SerialiseHFC (CardanoEras sc) where
  encodeDiskHfcBlock (CardanoCodecConfig ccfgByron ccfgShelley) = \case
      -- We are backwards compatible with Byron and thus use the exact same
      -- encoding.
      BlockByron   blockByron   ->                encodeDisk ccfgByron blockByron
      -- For Shelley, we need to prepend the hard fork envelope.
      BlockShelley blockShelley -> prependTag 2 $ encodeDisk ccfgShelley blockShelley
  decodeDiskHfcBlock (CardanoCodecConfig ccfgByron ccfgShelley) = do
      enforceSize "CardanoBlock" 2
      CBOR.decodeWord >>= \case
        0 -> fmap BlockByron   <$> Byron.decodeByronBoundaryBlock epochSlots
        1 -> fmap BlockByron   <$> Byron.decodeByronRegularBlock  epochSlots
        -- We don't have to drop the first two bytes from the 'ByteString'
        -- passed to the decoder as slicing already takes care of this.
        2 -> fmap BlockShelley <$> decodeDisk ccfgShelley
        t -> cborError $ DecoderErrorUnknownTag "CardanoBlock" (fromIntegral t)
    where
      epochSlots = Byron.getByronEpochSlots ccfgByron

  reconstructHfcPrefixLen _ = PrefixLen 2

  reconstructHfcNestedCtxt _ prefix blockSize =
      case Short.index prefix 1 of
        0 -> SomeBlock $ NestedCtxt (NCZ (Byron.CtxtByronBoundary blockSize))
        1 -> SomeBlock $ NestedCtxt (NCZ (Byron.CtxtByronRegular  blockSize))
        2 -> SomeBlock $ NestedCtxt (NCS (NCZ Shelley.CtxtShelley))
        _ -> error $ "CardanoBlock: invalid prefix " <> show prefix

-- | Prepend the given tag by creating a CBOR 2-tuple with the tag as the
-- first element and the given 'Encoding' as the second.
prependTag :: Word -> Encoding -> Encoding
prependTag tag payload = mconcat [
      CBOR.encodeListLen 2
    , CBOR.encodeWord tag
    , payload
    ]

{-------------------------------------------------------------------------------
  TranslateNetworkProtocolVersion instance
-------------------------------------------------------------------------------}

-- Note: we don't support all combinations, so we don't declare them as
-- COMPLETE

-- | We support only Byron V1 with the hard fork disabled, as no other
-- versions have been released before the hard fork
pattern CardanoNodeToNodeVersion1 :: BlockNodeToNodeVersion (CardanoBlock sc)
pattern CardanoNodeToNodeVersion1 =
    HardForkNodeToNodeDisabled (WrapNodeToNodeVersion ByronNodeToNodeVersion1)

pattern CardanoNodeToNodeVersion2 :: BlockNodeToNodeVersion (CardanoBlock sc)
pattern CardanoNodeToNodeVersion2 =
    HardForkNodeToNodeEnabled (
         WrapNodeToNodeVersion ByronNodeToNodeVersion2
      :* WrapNodeToNodeVersion ShelleyNodeToNodeVersion1
      :* Nil
      )

-- | We support Byron V1 with hard fork disabled, as it was released before
-- the hard fork
pattern CardanoNodeToClientVersion1 :: BlockNodeToClientVersion (CardanoBlock sc)
pattern CardanoNodeToClientVersion1 =
    HardForkNodeToClientDisabled (WrapNodeToClientVersion ByronNodeToClientVersion1)

-- | We support Byron V2 with hard fork disabled, as it was released before
-- the hard fork
pattern CardanoNodeToClientVersion2 :: BlockNodeToClientVersion (CardanoBlock sc)
pattern CardanoNodeToClientVersion2 =
    HardForkNodeToClientDisabled (WrapNodeToClientVersion ByronNodeToClientVersion2)

pattern CardanoNodeToClientVersion3 :: BlockNodeToClientVersion (CardanoBlock sc)
pattern CardanoNodeToClientVersion3 =
    HardForkNodeToClientEnabled (
         WrapNodeToClientVersion ByronNodeToClientVersion2
      :* WrapNodeToClientVersion ShelleyNodeToClientVersion1
      :* Nil
      )

instance TPraosCrypto sc => TranslateNetworkProtocolVersion (CardanoBlock sc) where
  supportedNodeToNodeVersions _ = NE.fromList $
      [ CardanoNodeToNodeVersion1
      , CardanoNodeToNodeVersion2
      ]

  supportedNodeToClientVersions _ = NE.fromList $
      [ CardanoNodeToClientVersion1
      , CardanoNodeToClientVersion2
      , CardanoNodeToClientVersion3
      ]

  mostRecentSupportedNodeToNode   _ = CardanoNodeToNodeVersion2
  mostRecentSupportedNodeToClient _ = CardanoNodeToClientVersion3

  nodeToNodeProtocolVersion _ = \case
      CardanoNodeToNodeVersion1 -> NodeToNodeV_1
      CardanoNodeToNodeVersion2 -> NodeToNodeV_2
      v                         -> error $ "unsupported version: " <> show v

  nodeToClientProtocolVersion _ = \case
      CardanoNodeToClientVersion1 -> NodeToClientV_1
      CardanoNodeToClientVersion2 -> NodeToClientV_2
      CardanoNodeToClientVersion3 -> NodeToClientV_3
      v                           -> error $ "unsupported version: " <> show v

{-------------------------------------------------------------------------------
  RunNode instance
-------------------------------------------------------------------------------}

instance TPraosCrypto sc => RunNode (CardanoBlock sc) where
  -- TODO pull out of RunNode
  nodeBlockFetchSize = \case
      HeaderByron   headerByron   -> nodeBlockFetchSize headerByron
      -- For Shelley, we add two extra bytes, see the 'SerialiseHFC' instance.
      HeaderShelley headerShelley -> nodeBlockFetchSize headerShelley + 2

  -- Use a ChunkInfo with Byron's epoch size for the whole chain. This means
  -- Shelley chunks will be 10x smaller, as the slot density is 10x smaller.
  --
  -- TODO update when a non-uniform chunk size is supported
  nodeImmDbChunkInfo = simpleChunkInfo
                     . History.eraEpochSize
                     . unK . hd
                     . History.getShape
                     . hardForkLedgerConfigShape
                     . configLedger

  nodeGetBinaryBlockInfo = \case
      BlockByron   blockByron   ->
        nodeGetBinaryBlockInfo blockByron
      BlockShelley blockShelley ->
        -- We need to account for the two extra bytes of the envelope, see the
        -- 'SerialiseHFC' instance.
        shiftHeaderOffset 2 $ nodeGetBinaryBlockInfo blockShelley
    where
      shiftHeaderOffset :: Word16 -> BinaryBlockInfo -> BinaryBlockInfo
      shiftHeaderOffset shift binfo = binfo {
            headerOffset = headerOffset binfo + shift
          }

  -- Call Byron's intialisation, as the chain starts with Byron
  nodeInitChainDB cfg initChainDB =
      nodeInitChainDB
        (projByronTopLevelConfig cfg)
        (contramap BlockByron initChainDB)

  nodeCheckIntegrity cfg = \case
      BlockByron byronBlock ->
        Byron.verifyBlockIntegrity byronBlockCfg byronBlock
      BlockShelley shelleyBlock ->
        Shelley.verifyBlockIntegrity tpraosSlotsPerKESPeriod shelleyBlock
    where
      TopLevelConfig {
          configConsensus = CardanoConsensusConfig _ shelleyPartialConsensusCfg
        , configBlock = CardanoBlockConfig byronBlockCfg _
        } = cfg

      TPraosParams { tpraosSlotsPerKESPeriod } = shelleyPartialConsensusCfg

{-------------------------------------------------------------------------------
  ProtocolInfo
-------------------------------------------------------------------------------}

protocolInfoCardano
  :: forall sc m. (IOLike m, TPraosCrypto sc)
     -- Byron
  => Genesis.Config
  -> Maybe PBftSignatureThreshold
  -> Update.ProtocolVersion
  -> Update.SoftwareVersion
  -> Maybe PBftLeaderCredentials
     -- Shelley
  -> ShelleyGenesis sc
  -> ProtVer
  -> Natural
  -> Maybe (TPraosLeaderCredentials sc)
     -- Hard fork
  -> Maybe EpochNo  -- ^ lower bound on first Shelley epoch
  -> HardCodedTransition
  -> ProtocolInfo m (CardanoBlock sc)
protocolInfoCardano genesisByron mSigThresh pVer sVer mbCredsByron
                    genesisShelley protVer maxMajorPV mbCredsShelley
                    mbLowerBound hardCodedTransition =
    ProtocolInfo {
        pInfoConfig      = cfg
      , pInfoInitLedger  = ExtLedgerState {
            ledgerState = HardForkLedgerState $
                            initHardForkState initLedgerStateByron
          , headerState = genesisHeaderState $
                            initHardForkState
                              (WrapChainDepState initChainDepStateByron)
          }
      , pInfoLeaderCreds = creds
      }
  where
    -- Byron
    ProtocolInfo {
        pInfoConfig      = TopLevelConfig {
            configConsensus = consensusConfigByron
          , configLedger    = ledgerConfigByron
          , configBlock     = blockConfigByron
          }
      , pInfoInitLedger  = ExtLedgerState {
            ledgerState = initLedgerStateByron
          , headerState = HeaderState {
                headerStateConsensus = initChainDepStateByron
              }
          }
      } = protocolInfoByron @m genesisByron mSigThresh pVer sVer mbCredsByron

    partialConsensusConfigByron :: PartialConsensusConfig (BlockProtocol ByronBlock)
    partialConsensusConfigByron = consensusConfigByron

    partialLedgerConfigByron :: PartialLedgerConfig ByronBlock
    partialLedgerConfigByron = ByronPartialLedgerConfig {
          byronLedgerConfig = ledgerConfigByron
        , transitionEpoch   = hardCodedTransition
        }

    kByron :: SecurityParam
    kByron = Byron.genesisSecurityParam genesisByron

    -- Shelley

    tpraosParams :: TPraosParams
    tpraosParams = Shelley.mkTPraosParams maxMajorPV genesisShelley

    blockConfigShelley :: BlockConfig (ShelleyBlock sc)
    blockConfigShelley = Shelley.mkShelleyBlockConfig protVer genesisShelley

    partialConsensusConfigShelley :: PartialConsensusConfig (BlockProtocol (ShelleyBlock sc))
    partialConsensusConfigShelley = tpraosParams

    partialLedgerConfigShelley :: PartialLedgerConfig (ShelleyBlock sc)
    partialLedgerConfigShelley = ShelleyPartialLedgerConfig {
        shelleyPartialLedgerGenesis = genesisShelley
      , shelleyPartialMaxMajorPV    = maxMajorPV
      }

    kShelley :: SecurityParam
    kShelley = SecurityParam $ sgSecurityParam genesisShelley

    -- Cardano

    k :: SecurityParam
    k = assert (kByron == kShelley) kByron

    shape :: History.Shape (CardanoEras sc)
    shape = History.Shape $
      exactlyTwo
        (Byron.byronEraParams safeBeforeByron genesisByron)
        (Shelley.shelleyEraParams genesisShelley)
      where
        safeBeforeByron :: History.SafeBeforeEpoch
        safeBeforeByron =
            maybe History.NoLowerBound History.LowerBound mbLowerBound

    cfg :: TopLevelConfig (CardanoBlock sc)
    cfg = TopLevelConfig {
        configConsensus = HardForkConsensusConfig {
            hardForkConsensusConfigK      = k
          , hardForkConsensusConfigShape  = shape
          , hardForkConsensusConfigPerEra = PerEraConsensusConfig
              (  WrapPartialConsensusConfig partialConsensusConfigByron
              :* WrapPartialConsensusConfig partialConsensusConfigShelley
              :* Nil
              )
          }
      , configIndep     = PerEraChainIndepStateConfig
              (  WrapChainIndepStateConfig ()
              :* WrapChainIndepStateConfig tpraosParams
              :* Nil
              )
      , configLedger    = HardForkLedgerConfig {
            hardForkLedgerConfigK      = k
          , hardForkLedgerConfigShape  = shape
          , hardForkLedgerConfigPerEra = PerEraLedgerConfig
              (  WrapPartialLedgerConfig partialLedgerConfigByron
              :* WrapPartialLedgerConfig partialLedgerConfigShelley
              :* Nil
              )
          }
      , configBlock     = CardanoBlockConfig blockConfigByron blockConfigShelley
      }

    creds :: Maybe
               ( HardForkCanBeLeader  (CardanoEras sc)
               , MaintainForgeState m (CardanoBlock sc)
               )
    creds = case (mbCredsByron, mbCredsShelley) of
        (Nothing, Nothing) -> Nothing

        (Just credsByron, Just credsShelley) -> Just (
              OptCons (WrapCanBeLeader (mkPBftIsLeader credsByron))
            $ OptCons (WrapCanBeLeader (tpraosLeaderCredentialsIsCoreNode credsShelley))
            $ OptNil
          , undistribMaintainForgeState $
                 defaultMaintainForgeState
              :* (shelleyMaintainForgeState partialConsensusConfigShelley credsShelley)
              :* Nil
          )

        (Nothing, Just credsShelley) -> Just (
              OptSkip
            $ OptCons (WrapCanBeLeader (tpraosLeaderCredentialsIsCoreNode credsShelley))
            $ OptNil
          , undistribMaintainForgeState $
                 defaultMaintainForgeState
              :* (shelleyMaintainForgeState partialConsensusConfigShelley credsShelley)
              :* Nil
          )

        -- TODO 'ForgeState' is an 'NP' while 'CanBeLeader' is an 'OptNP', so
        -- even when we have no Shelley credentials, we'd still need to
        -- maintain a Shelley 'ForgeState', for which we'd need the key stored
        -- in the credentials.
        (Just _, Nothing) ->
          error "can't be a Byron core node without being a Shelley core node"

protocolClientInfoCardano
  :: forall sc.
     -- Byron
     EpochSlots
  -> SecurityParam
  -> ProtocolClientInfo (CardanoBlock sc)
protocolClientInfoCardano epochSlots secParam = ProtocolClientInfo {
      pClientInfoCodecConfig =
        CardanoCodecConfig
          (pClientInfoCodecConfig (protocolClientInfoByron epochSlots secParam))
          (pClientInfoCodecConfig protocolClientInfoShelley)
    }

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

-- | We are lucky that for Byron we can construct all the full configs from
-- the partial ones, which means we can reconstruct the 'TopLevelConfig' for
-- Byron. This is not possible for Shelley, as we would have to call
-- 'completeLedgerConfig' and 'completeConsensusConfig' first.
projByronTopLevelConfig
  :: TopLevelConfig (CardanoBlock sc)
  -> TopLevelConfig ByronBlock
projByronTopLevelConfig cfg = byronCfg
  where
    TopLevelConfig {
        configBlock     = CardanoBlockConfig     byronBlockCfg     _
      , configConsensus = CardanoConsensusConfig byronConsensusCfg _
      , configLedger    = CardanoLedgerConfig    byronLedgerCfg    _
      } = cfg

    byronCfg :: TopLevelConfig ByronBlock
    byronCfg = TopLevelConfig {
        configBlock     = byronBlockCfg
      , configConsensus = byronConsensusCfg
      , configIndep     = ()
      , configLedger    = byronLedgerConfig byronLedgerCfg
      }
