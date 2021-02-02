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
module Ouroboros.Consensus.Example.Node (
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

instance ShelleyBasedEra era => SerialiseConstraintsHFC (ExampleBlock era)

instance ExampleHardForkConstraints c => SerialiseHFC (ExampleEras c) where
  encodeDiskHfcBlock (ExampleCodecConfig ccfgShelley ccfgExample) = \case
      -- For Shelley and later eras, we need to prepend the hard fork envelope.
      BlockShelley blockShelley -> prependTag 2 $ encodeDisk ccfgShelley blockShelley
      BlockExample blockExample -> prependTag 99 $ encodeDisk ccfgExample blockExample
  decodeDiskHfcBlock (ExampleCodecConfig ccfgShelley ccfgExample) = do
      enforceSize "ExampleBlock" 2
      CBOR.decodeWord >>= \case
        -- We don't have to drop the first two bytes from the 'ByteString'
        -- passed to the decoder as slicing already takes care of this.
        2 -> fmap BlockShelley <$> decodeDisk ccfgShelley
        99 -> fmap BlockExample <$> decodeDisk ccfgExample
        t -> cborError $ DecoderErrorUnknownTag "ExampleBlock" (fromIntegral t)

  reconstructHfcPrefixLen _ = PrefixLen 2

  reconstructHfcNestedCtxt _ prefix blockSize =
      case Short.index prefix 1 of
        2 -> SomeSecond $ NestedCtxt (NCZ Shelley.CtxtShelley)
        99 -> SomeSecond $ NestedCtxt (NCS (NCZ Shelley.CtxtShelley))
        _ -> error $ "ExampleBlock: invalid prefix " <> show prefix

  getHfcBinaryBlockInfo = \case
      -- For Shelley and the later eras, we need to account for the two extra
      -- bytes of the envelope.
      BlockShelley blockShelley ->
        shiftHeaderOffset 2 $ getBinaryBlockInfo blockShelley
      BlockMary blockExample ->
        shiftHeaderOffset 2 $ getBinaryBlockInfo blockExample
    where
      shiftHeaderOffset :: Word16 -> BinaryBlockInfo -> BinaryBlockInfo
      shiftHeaderOffset shift binfo = binfo {
            headerOffset = headerOffset binfo + shift
          }

  estimateHfcBlockSize = \case
      -- For Shelley and later eras, we add two extra bytes, see the
      -- 'SerialiseHFC' instance.
      HeaderShelley headerShelley -> estimateBlockSize headerShelley + 2
      HeaderExample headerExample -> estimateBlockSize headerExample + 2

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

-- | The hard fork enabled and the Shelley era enabled.
pattern ExampleNodeToClientVersion1 :: BlockNodeToClientVersion (ExampleBlock c)
pattern ExampleNodeToClientVersion1 =
    HardForkNodeToClientEnabled
      HardForkSpecificNodeToClientVersion3
      (  EraNodeToClientEnabled ShelleyNodeToClientVersion3
      :* EraNodeToClientDisabled
      :* Nil
      )

-- | The hard fork enabled with the Shelley era and Example era enabled.
pattern ExampleNodeToClientVersion2 :: BlockNodeToClientVersion (ExampleBlock c)
pattern ExampleNodeToClientVersion2 =
    HardForkNodeToClientEnabled
      HardForkSpecificNodeToClientVersion3
      (  EraNodeToClientEnabled ShelleyNodeToClientVersion3
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion3
      :* Nil
      )

instance ExampleHardForkConstraints c
      => SupportedNetworkProtocolVersion (ExampleBlock c) where
  supportedNodeToNodeVersions _ = Map.fromList $
      [ (NodeToNodeV_1, CardanoNodeToNodeVersion1)
      , (NodeToNodeV_2, CardanoNodeToNodeVersion2)
      , (NodeToNodeV_3, CardanoNodeToNodeVersion2)
      , (NodeToNodeV_4, CardanoNodeToNodeVersion3)
      , (NodeToNodeV_5, CardanoNodeToNodeVersion4)
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
  => ProtocolParamsShelleyBased (ShelleyEra c)
  -> ProtocolParamsShelley
  -> ProtocolParamsExample
  -> ProtocolParamsTransition
       (ShelleyBlock (ShelleyEra c))
       (ShelleyBlock (ExampleEra c))
  -> ProtocolInfo m (ExampleBlock c)
protocolInfoCardano ProtocolParamsShelleyBased {
                        shelleyBasedGenesis           = genesisShelley
                      , shelleyBasedInitialNonce      = initialNonceShelley
                      , shelleyBasedLeaderCredentials = credssShelleyBased
                      }
                    ProtocolParamsShelley {
                        shelleyProtVer = protVerShelley
                      }
                    ProtocolParamsExample {
                        allegraProtVer = protVerExample
                      }
                    ProtocolParamsTransition {
                        transitionTrigger = triggerHardForkShelleyExample
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
      , topLevelConfigStorage =
          CardanoStorageConfig
            (configStorage topLevelConfigByron)
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
