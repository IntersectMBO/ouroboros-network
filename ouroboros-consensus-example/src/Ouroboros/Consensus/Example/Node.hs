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
    ExampleHardForkConstraints
  , MaxMajorProtVer (..)
  , ProtocolParamsExample (..)
  , ProtocolParamsTransition (..)
  , TriggerHardFork (..)
  , protocolClientInfoExample
  , protocolInfoExample
    -- * SupportedNetworkProtocolVersion
  , pattern ExampleNodeToClientVersion1
  , pattern ExampleNodeToClientVersion2
  , pattern ExampleNodeToNodeVersion1
  , pattern ExampleNodeToNodeVersion2
  ) where

import qualified Codec.CBOR.Decoding as CBOR
import           Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as CBOR
import qualified Data.ByteString.Short as Short
import qualified Data.Map.Strict as Map
import           Data.SOP.Strict hiding (shape, shift)
import           Data.Word (Word16)

import           Cardano.Binary (DecoderError (..), enforceSize)
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

import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.Embed.Nary
import           Ouroboros.Consensus.HardFork.Combinator.Serialisation

import qualified Cardano.Ledger.Era as SL
import           Cardano.Ledger.Val (coin, (<->))
import qualified Cardano.Ledger.Val as Val
import           Ouroboros.Consensus.Example.ShelleyBased
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)
import qualified Ouroboros.Consensus.Shelley.Ledger as Shelley
import           Ouroboros.Consensus.Shelley.Ledger.NetworkProtocolVersion
import           Ouroboros.Consensus.Shelley.Node
import           Ouroboros.Consensus.Shelley.Protocol (TPraosParams (..))
import qualified Ouroboros.Consensus.Shelley.Protocol as Shelley
import           Ouroboros.Consensus.Shelley.ShelleyBased
import qualified Shelley.Spec.Ledger.API as SL

import           Ouroboros.Consensus.Example.Block
import           Ouroboros.Consensus.Example.CanHardFork

{-------------------------------------------------------------------------------
  SerialiseHFC
-------------------------------------------------------------------------------}

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
        2  -> fmap BlockShelley <$> decodeDisk ccfgShelley
        99 -> fmap BlockExample <$> decodeDisk ccfgExample
        t  -> cborError $ DecoderErrorUnknownTag "ExampleBlock" (fromIntegral t)

  reconstructHfcPrefixLen _ = PrefixLen 2

  reconstructHfcNestedCtxt _ prefix _ =
      case Short.index prefix 1 of
        2  -> SomeSecond $ NestedCtxt (NCZ Shelley.CtxtShelley)
        99 -> SomeSecond $ NestedCtxt (NCS (NCZ Shelley.CtxtShelley))
        _  -> error $ "ExampleBlock: invalid prefix " <> show prefix

  getHfcBinaryBlockInfo = \case
      -- For Shelley and the later eras, we need to account for the two extra
      -- bytes of the envelope.
      BlockShelley blockShelley ->
        shiftHeaderOffset 2 $ getBinaryBlockInfo blockShelley
      BlockExample blockExample ->
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

-- | The hard fork enabled with the Shelley and Example eras enabled.
pattern ExampleNodeToNodeVersion1 :: BlockNodeToNodeVersion (ExampleBlock c)
pattern ExampleNodeToNodeVersion1 =
    HardForkNodeToNodeEnabled
      HardForkSpecificNodeToNodeVersion1
      (  EraNodeToNodeEnabled ShelleyNodeToNodeVersion1
      :* EraNodeToNodeDisabled
      :* Nil
      )

-- | The hard fork enabled with the Shelley and Example eras enabled.
pattern ExampleNodeToNodeVersion2 :: BlockNodeToNodeVersion (ExampleBlock c)
pattern ExampleNodeToNodeVersion2 =
    HardForkNodeToNodeEnabled
      HardForkSpecificNodeToNodeVersion1
      (  EraNodeToNodeEnabled ShelleyNodeToNodeVersion1
      :* EraNodeToNodeEnabled ShelleyNodeToNodeVersion1
      :* Nil
      )

-- | The hard fork enabled, and the Shelley era enabled using 'ShelleyNodeToClientVersion3' protocol.
pattern ExampleNodeToClientVersion1 :: BlockNodeToClientVersion (ExampleBlock c)
pattern ExampleNodeToClientVersion1 =
    HardForkNodeToClientEnabled
      HardForkSpecificNodeToClientVersion2
      (  EraNodeToClientEnabled ShelleyNodeToClientVersion3
      :* EraNodeToClientDisabled
      :* Nil
      )

-- | The hard fork enabled, and the Shelley and Example eras enabled using 'ShelleyNodeToClientVersion3' protocol.
pattern ExampleNodeToClientVersion2 :: BlockNodeToClientVersion (ExampleBlock c)
pattern ExampleNodeToClientVersion2 =
    HardForkNodeToClientEnabled
      HardForkSpecificNodeToClientVersion2
      (  EraNodeToClientEnabled ShelleyNodeToClientVersion3
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion3
      :* Nil
      )

instance ExampleHardForkConstraints c
      => SupportedNetworkProtocolVersion (ExampleBlock c) where
  supportedNodeToNodeVersions _ = Map.fromList $
      [ (NodeToNodeV_3, ExampleNodeToNodeVersion1)
      , (NodeToNodeV_3, ExampleNodeToNodeVersion2)
      ]

  supportedNodeToClientVersions _ = Map.fromList $
      [ (NodeToClientV_4, ExampleNodeToClientVersion1)
      , (NodeToClientV_4, ExampleNodeToClientVersion2)
      ]

  latestReleasedNodeVersion = latestReleasedNodeVersionDefault

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

-- | Parameters needed to run Shelley
data ProtocolParamsExample = ProtocolParamsExample {
      exampleProtVer :: SL.ProtVer
    }

-- | Create a 'ProtocolInfo' for 'ExampleBlock'
--
-- NOTE: the initial staking and funds in the 'ShelleyGenesis' are ignored,
-- /unless/ configured to skip the Shelley era and hard fork to Example
-- era from the start using @TriggerHardForkAtEpoch 0@ for testing purposes.
--
-- PRECONDITION: only a single set of Shelley credentials is allowed when used
-- for mainnet (check against @'SL.gNetworkId' 'shelleyBasedGenesis'@).
protocolInfoExample ::
     forall c m. (IOLike m, ExampleHardForkConstraints c)
  => ProtocolParamsShelleyBased (ShelleyEra c)
  -> ProtocolParamsShelley
  -> ProtocolParamsExample
  -> ProtocolParamsTransition
       (ShelleyBlock (ShelleyEra c))
       (ShelleyBlock (ExampleEra c))
  -> ProtocolInfo m (ExampleBlock c)
protocolInfoExample ProtocolParamsShelleyBased {
                        shelleyBasedGenesis           = genesisShelley
                      , shelleyBasedInitialNonce      = initialNonceShelley
                      , shelleyBasedLeaderCredentials = credssShelleyBased
                      }
                    ProtocolParamsShelley {
                        shelleyProtVer = protVerShelley
                      }
                    ProtocolParamsExample {
                        exampleProtVer = protVerExample
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
      , pInfoInitLedger   = initExtLedgerStateExample
      , pInfoBlockForging = blockForging
      }
  where
    -- The major protocol version of the last era is the maximum major protocol
    -- version we support.
    maxMajorProtVer :: MaxMajorProtVer
    maxMajorProtVer = MaxMajorProtVer (pvMajor protVerExample)

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
          triggerHardForkShelleyExample

    kShelley :: SecurityParam
    kShelley = SecurityParam $ sgSecurityParam genesisShelley

    -- Allegra

    genesisExample :: ShelleyGenesis (ExampleEra c)
    genesisExample = SL.translateEra' () genesisShelley

    blockConfigExample :: BlockConfig (ShelleyBlock (ExampleEra c))
    blockConfigExample =
        Shelley.mkShelleyBlockConfig
          protVerExample
          genesisExample
          (tpraosBlockIssuerVKey <$> credssShelleyBased)

    partialConsensusConfigExample ::
         PartialConsensusConfig (BlockProtocol (ShelleyBlock (ExampleEra c)))
    partialConsensusConfigExample = tpraosParams

    partialLedgerConfigExample :: PartialLedgerConfig (ShelleyBlock (ExampleEra c))
    partialLedgerConfigExample =
        mkPartialLedgerConfigShelley
          genesisExample
          maxMajorProtVer
          TriggerHardForkNever
    --
    -- Cardano

    k :: SecurityParam
    k = kShelley

    shape :: History.Shape (ExampleEras c)
    shape = History.Shape $ Exactly $
           K (Shelley.shelleyEraParams genesisShelley)
        :* K (Shelley.shelleyEraParams genesisExample)
        :* Nil

    cfg :: TopLevelConfig (ExampleBlock c)
    cfg = TopLevelConfig {
        topLevelConfigProtocol = HardForkConsensusConfig {
            hardForkConsensusConfigK      = k
          , hardForkConsensusConfigShape  = shape
          , hardForkConsensusConfigPerEra = PerEraConsensusConfig
              (  WrapPartialConsensusConfig partialConsensusConfigShelley
              :* WrapPartialConsensusConfig partialConsensusConfigExample
              :* Nil
              )
          }
      , topLevelConfigLedger = HardForkLedgerConfig {
            hardForkLedgerConfigShape  = shape
          , hardForkLedgerConfigPerEra = PerEraLedgerConfig
              (  WrapPartialLedgerConfig partialLedgerConfigShelley
              :* WrapPartialLedgerConfig partialLedgerConfigExample
              :* Nil
              )
          }
      , topLevelConfigBlock =
          ExampleBlockConfig
            blockConfigShelley
            blockConfigExample
      , topLevelConfigCodec =
          ExampleCodecConfig
            Shelley.ShelleyCodecConfig
            Shelley.ShelleyCodecConfig
      , topLevelConfigStorage =
          ExampleStorageConfig
            (Shelley.ShelleyStorageConfig tpraosSlotsPerKESPeriod k)
            (Shelley.ShelleyStorageConfig tpraosSlotsPerKESPeriod k)
      }

    -- Register the initial staking and initial funds (if provided in the genesis config) in
    -- the ledger state.
    initExtLedgerStateExample :: ExtLedgerState (ExampleBlock c)
    initExtLedgerStateExample = ExtLedgerState {
          headerState = initHeaderState
        , ledgerState = overShelleyBasedLedgerState register initLedgerState
        }
      where
        initHeaderState :: HeaderState (ExampleBlock c)
        initLedgerState :: LedgerState (ExampleBlock c)
        ExtLedgerState initLedgerState initHeaderState =
          injectInitialExtLedgerState cfg $ ExtLedgerState {
            ledgerState = Shelley.ShelleyLedgerState {
              Shelley.shelleyLedgerTip        = Origin
            , Shelley.shelleyLedgerState      = SL.chainNes initShelleyState
            , Shelley.shelleyLedgerTransition = Shelley.ShelleyTransitionInfo {Shelley.shelleyAfterVoting = 0}
            }
          , headerState = genesisHeaderState initChainDepState
          }

        initChainDepState :: Shelley.TPraosState c
        initChainDepState = Shelley.TPraosState Origin $
          SL.ChainDepState {
              SL.csProtocol = SL.PrtclState
                (SL.chainOCertIssue     initShelleyState)
                (SL.chainEvolvingNonce  initShelleyState)
                (SL.chainCandidateNonce initShelleyState)
            , SL.csTickn = SL.TicknState
                (SL.chainEpochNonce     initShelleyState)
                (SL.chainPrevEpochNonce initShelleyState)
            , SL.csLabNonce =
                (SL.chainPrevEpochNonce initShelleyState)
            }

        initShelleyState :: SL.ChainState (ShelleyEra c)
        initShelleyState =
            overNewEpochState
              (registerGenesisStaking (SL.sgStaking genesisShelley)) $
              SL.initialShelleyState
                Origin
                0
                (SL.genesisUTxO genesisShelley)
                (coin $ Val.inject (SL.word64ToCoin (SL.sgMaxLovelaceSupply genesisShelley))
                    <-> SL.balance (SL.genesisUTxO genesisShelley))
                (SL.sgGenDelegs genesisShelley)
                (SL.sgProtocolParams genesisShelley)
                initialNonceShelley

        overNewEpochState ::
             (SL.NewEpochState era -> SL.NewEpochState era)
          -> (SL.ChainState    era -> SL.ChainState    era)
        overNewEpochState f cs = cs { SL.chainNes = f (SL.chainNes cs) }

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
    blockForging :: m [BlockForging m (ExampleBlock c)]
    blockForging = do
        shelleyBased :: [ OptNP 'False (BlockForging m) (ExampleEras c) ] <- blockForgingShelleyBased
        return $ hardForkBlockForging "Example" <$> shelleyBased

    blockForgingShelleyBased :: m [OptNP 'False (BlockForging m) (ExampleEras c)]
    blockForgingShelleyBased = do
        shelleyBased <-
          traverse
            (shelleySharedBlockForging (Proxy @(ShelleyBasedExampleEras c)) tpraosParams)
            credssShelleyBased
        return $ reassoc <$> shelleyBased
      where
        reassoc ::
             NP (BlockForging m :.: ShelleyBlock) (ShelleyBasedExampleEras c)
          -> OptNP 'False (BlockForging m) (ExampleEras c)
        reassoc = injectShelleyOptNP unComp . OptNP.fromNonEmptyNP

protocolClientInfoExample
  :: forall c.  ProtocolClientInfo (ExampleBlock c)
protocolClientInfoExample = ProtocolClientInfo {
      pClientInfoCodecConfig =
        ExampleCodecConfig
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
