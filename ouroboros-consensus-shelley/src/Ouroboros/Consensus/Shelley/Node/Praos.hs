{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Ouroboros.Consensus.Shelley.Node.Praos (
    ProtocolParamsBabbage (..)
  , blockForgingPraosBabbage
  , blockForgingPraosShelleyBased
  , praosBlockForging
  , praosSharedBlockForging
  , protocolInfoPraosBabbage
  , protocolInfoPraosShelleyBased
  ) where

import qualified Cardano.Ledger.BaseTypes as SL (mkActiveSlotCoeff)
import qualified Cardano.Ledger.Era as Core
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Protocol.TPraos.OCert as Absolute
import qualified Cardano.Protocol.TPraos.OCert as SL
import           Cardano.Slotting.EpochInfo (EpochInfo, fixedEpochInfo)
import           Cardano.Slotting.Time (SystemStart (SystemStart), mkSlotLength)
import           Control.Monad.Except (Except)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config (SecurityParam (SecurityParam),
                     TopLevelConfig (..), configConsensus)
import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.HeaderValidation
                     (HeaderState (HeaderState))
import           Ouroboros.Consensus.Ledger.Abstract (LedgerConfig)
import           Ouroboros.Consensus.Ledger.Extended (ExtLedgerState (..))
import           Ouroboros.Consensus.Mempool.TxLimits
import qualified Ouroboros.Consensus.Mempool.TxLimits as TxLimits
import           Ouroboros.Consensus.Node (ProtocolInfo (..))
import           Ouroboros.Consensus.Protocol.Abstract (ConsensusConfig)
import qualified Ouroboros.Consensus.Protocol.Ledger.HotKey as HotKey
import           Ouroboros.Consensus.Protocol.Praos (ConsensusConfig (..),
                     Praos, PraosParams (..), PraosState (..),
                     praosCheckCanForge)
import           Ouroboros.Consensus.Protocol.Praos.Common
                     (MaxMajorProtVer (MaxMajorProtVer),
                     PraosCanBeLeader (praosCanBeLeaderOpCert))
import           Ouroboros.Consensus.Shelley.Eras (BabbageEra, EraCrypto,
                     ShelleyBasedEra (shelleyBasedEraName))
import           Ouroboros.Consensus.Shelley.Ledger
                     (CodecConfig (ShelleyCodecConfig), LedgerState (..),
                     ShelleyBlock, ShelleyCompatible, ShelleyTransition (..),
                     StorageConfig (..), forgeShelleyBlock,
                     mkShelleyBlockConfig, mkShelleyLedgerConfig)
import           Ouroboros.Consensus.Shelley.Node
                     (ProtocolParamsShelleyBased (..),
                     ShelleyLeaderCredentials (..), validateGenesis)
import           Ouroboros.Consensus.Shelley.Node.Common (ShelleyEraWithCrypto,
                     shelleyBlockIssuerVKey)
import           Ouroboros.Consensus.Shelley.Protocol.Praos ()
import           Ouroboros.Consensus.Util.Assert (assertWithMsg)
import           Ouroboros.Consensus.Util.IOLike (IOLike)

{-------------------------------------------------------------------------------
  BlockForging
-------------------------------------------------------------------------------}

-- | Create a 'BlockForging' record for a single era.
praosBlockForging ::
  forall m era c.
  ( ShelleyCompatible (Praos c) era,
    c ~ EraCrypto era,
    TxLimits (ShelleyBlock (Praos c) era),
    IOLike m
  ) =>
  PraosParams ->
  TxLimits.Overrides (ShelleyBlock (Praos c) era) ->
  ShelleyLeaderCredentials (EraCrypto era) ->
  m (BlockForging m (ShelleyBlock (Praos c) era))
praosBlockForging praosParams maxTxCapacityOverrides credentials = do
    hotKey <- HotKey.mkHotKey @m @c initSignKey startPeriod praosMaxKESEvo
    pure $ praosSharedBlockForging hotKey slotToPeriod credentials maxTxCapacityOverrides
  where
    PraosParams {praosMaxKESEvo, praosSlotsPerKESPeriod} = praosParams

    ShelleyLeaderCredentials {
        shelleyLeaderCredentialsInitSignKey = initSignKey
      , shelleyLeaderCredentialsCanBeLeader = canBeLeader
      } = credentials

    startPeriod :: Absolute.KESPeriod
    startPeriod = SL.ocertKESPeriod $ praosCanBeLeaderOpCert canBeLeader

    slotToPeriod :: SlotNo -> Absolute.KESPeriod
    slotToPeriod (SlotNo slot) =
      SL.KESPeriod $ fromIntegral $ slot `div` praosSlotsPerKESPeriod

-- | Create a 'BlockForging' record safely using the given 'Hotkey'.
--
-- The name of the era (separated by a @_@) will be appended to each
-- 'forgeLabel'.
praosSharedBlockForging ::
  forall m c era.
  ( ShelleyEraWithCrypto c (Praos c) era,
    IOLike m
  )
  => HotKey.HotKey c m
  -> (SlotNo -> Absolute.KESPeriod)
  -> ShelleyLeaderCredentials c
  -> TxLimits.Overrides (ShelleyBlock (Praos c) era)
  -> BlockForging m     (ShelleyBlock (Praos c) era)
praosSharedBlockForging
  hotKey
  slotToPeriod
  ShelleyLeaderCredentials
    { shelleyLeaderCredentialsCanBeLeader = canBeLeader,
      shelleyLeaderCredentialsLabel = label
    }
  maxTxCapacityOverrides = do
      BlockForging
        { forgeLabel = label <> "_" <> shelleyBasedEraName (Proxy @era),
          canBeLeader = canBeLeader,
          updateForgeState = \_ curSlot _ ->
            forgeStateUpdateInfoFromUpdateInfo
              <$> HotKey.evolve hotKey (slotToPeriod curSlot),
          checkCanForge = \cfg curSlot _tickedChainDepState _isLeader ->
            praosCheckCanForge
              (configConsensus cfg)
              curSlot,
          forgeBlock = \cfg ->
            forgeShelleyBlock
              hotKey
              canBeLeader
              cfg
              maxTxCapacityOverrides
        }

{-------------------------------------------------------------------------------
  ProtocolInfo
-------------------------------------------------------------------------------}

-- | Parameters needed to run Babbage
data ProtocolParamsBabbage c = ProtocolParamsBabbage
  { babbageProtVer :: SL.ProtVer,
    babbageMaxTxCapacityOverrides :: TxLimits.Overrides (ShelleyBlock (Praos c) (BabbageEra c))
  }

protocolInfoPraosBabbage ::
  forall c.
  ShelleyCompatible (Praos c) (BabbageEra c) =>
  ProtocolParamsShelleyBased (BabbageEra c) ->
  ProtocolParamsBabbage c ->
  ProtocolInfo (ShelleyBlock (Praos c) (BabbageEra c))
protocolInfoPraosBabbage
  protocolParamsShelleyBased
  ProtocolParamsBabbage
    { babbageProtVer = protVer } =
    protocolInfoPraosShelleyBased
      protocolParamsShelleyBased
      (error "Babbage currently pretending to be Alonzo")
      protVer

blockForgingPraosBabbage ::
  forall m c.
  ( IOLike m,
    ShelleyCompatible (Praos c) (BabbageEra c)
  ) =>
  ProtocolParamsShelleyBased (BabbageEra c) ->
  ProtocolParamsBabbage c ->
  m [BlockForging m (ShelleyBlock (Praos c) (BabbageEra c))]
blockForgingPraosBabbage
  protocolParamsShelleyBased
  ProtocolParamsBabbage
    { babbageProtVer = protoVer,
      babbageMaxTxCapacityOverrides = maxTxCapacityOverrides
    } =
  blockForgingPraosShelleyBased
    protocolParamsShelleyBased
    protoVer
    maxTxCapacityOverrides

protocolInfoPraosShelleyBased ::
  forall era c.
  ( ShelleyCompatible (Praos c) era,
    c ~ EraCrypto era
  ) =>
  ProtocolParamsShelleyBased era ->
  Core.TranslationContext era ->
  SL.ProtVer ->
  ProtocolInfo (ShelleyBlock (Praos c) era)
protocolInfoPraosShelleyBased
  ProtocolParamsShelleyBased
    { shelleyBasedGenesis = genesis,
      shelleyBasedInitialNonce = initialNonce,
      shelleyBasedLeaderCredentials = credentialss
    }
  transCtxt
  protVer =
    assertWithMsg (validateGenesis genesis) $
      ProtocolInfo
        { pInfoConfig = topLevelConfig,
          pInfoInitLedger = initExtLedgerState
        }
    where
      additionalGenesisConfig :: SL.AdditionalGenesisConfig era
      additionalGenesisConfig = transCtxt

      maxMajorProtVer :: MaxMajorProtVer
      maxMajorProtVer = MaxMajorProtVer $ SL.pvMajor protVer

      topLevelConfig :: TopLevelConfig (ShelleyBlock (Praos c) era)
      topLevelConfig =
        TopLevelConfig
          { topLevelConfigProtocol = consensusConfig,
            topLevelConfigLedger = ledgerConfig,
            topLevelConfigBlock = blockConfig,
            topLevelConfigCodec = ShelleyCodecConfig,
            topLevelConfigStorage = storageConfig
          }

      consensusConfig :: ConsensusConfig (BlockProtocol (ShelleyBlock (Praos c) era))
      consensusConfig =
        PraosConfig
          { praosParams,
            praosEpochInfo = epochInfo
          }

      ledgerConfig :: LedgerConfig (ShelleyBlock (Praos c) era)
      ledgerConfig = mkShelleyLedgerConfig genesis transCtxt epochInfo maxMajorProtVer

      epochInfo :: EpochInfo (Except History.PastHorizonException)
      epochInfo =
        fixedEpochInfo
          (SL.sgEpochLength genesis)
          (mkSlotLength $ SL.sgSlotLength genesis)

      praosParams :: PraosParams
      praosParams =
        PraosParams
          { praosSlotsPerKESPeriod = SL.sgSlotsPerKESPeriod genesis,
            praosLeaderF = SL.mkActiveSlotCoeff $ SL.sgActiveSlotsCoeff genesis,
            praosSecurityParam = SecurityParam $ SL.sgSecurityParam genesis,
            praosMaxKESEvo = SL.sgMaxKESEvolutions genesis,
            praosQuorum = SL.sgUpdateQuorum genesis,
            praosMaxMajorPV = maxMajorProtVer,
            praosMaxLovelaceSupply = SL.sgMaxLovelaceSupply genesis,
            praosNetworkId = SL.sgNetworkId genesis,
            praosSystemStart = SystemStart $ SL.sgSystemStart genesis
          }

      blockConfig :: BlockConfig (ShelleyBlock (Praos c) era)
      blockConfig =
        mkShelleyBlockConfig
          protVer
          genesis
          (shelleyBlockIssuerVKey <$> credentialss)

      storageConfig :: StorageConfig (ShelleyBlock (Praos c) era)
      storageConfig =
        ShelleyStorageConfig
          { shelleyStorageConfigSlotsPerKESPeriod = praosSlotsPerKESPeriod praosParams,
            shelleyStorageConfigSecurityParam = praosSecurityParam praosParams
          }

      initLedgerState :: LedgerState (ShelleyBlock (Praos c) era)
      initLedgerState =
        ShelleyLedgerState
          { shelleyLedgerTip = Origin,
            shelleyLedgerState = SL.initialState genesis additionalGenesisConfig,
            shelleyLedgerTransition = ShelleyTransitionInfo {shelleyAfterVoting = 0}
          }

      initChainDepState :: PraosState c
      initChainDepState =
        PraosState
          { praosStateLastSlot = Origin,
            praosStateOCertCounters = mempty,
            praosStateEvolvingNonce = initialNonce,
            praosStateCandidateNonce = initialNonce,
            praosStateEpochNonce = initialNonce,
            praosStateLabNonce = initialNonce,
            praosStateLastEpochBlockNonce = initialNonce
          }

      initExtLedgerState :: ExtLedgerState (ShelleyBlock (Praos c) era)
      initExtLedgerState =
        ExtLedgerState
          { ledgerState = initLedgerState,
            headerState = HeaderState Origin initChainDepState
          }

blockForgingPraosShelleyBased ::
  forall m era c.
  ( IOLike m,
    ShelleyCompatible (Praos c) era,
    TxLimits (ShelleyBlock (Praos c) era),
    c ~ EraCrypto era
  ) =>
  ProtocolParamsShelleyBased era ->
  SL.ProtVer ->
  TxLimits.Overrides (ShelleyBlock (Praos c) era) ->
  m [BlockForging m (ShelleyBlock (Praos c) era)]
blockForgingPraosShelleyBased
  ProtocolParamsShelleyBased
    { shelleyBasedGenesis = genesis,
      shelleyBasedLeaderCredentials = credentialss
    }
  protVer
  maxTxCapacityOverrides =
    traverse
      (praosBlockForging praosParams maxTxCapacityOverrides)
      credentialss
  where
    maxMajorProtVer :: MaxMajorProtVer
    maxMajorProtVer = MaxMajorProtVer $ SL.pvMajor protVer

    praosParams :: PraosParams
    praosParams =
      PraosParams
        { praosSlotsPerKESPeriod = SL.sgSlotsPerKESPeriod genesis,
          praosLeaderF = SL.mkActiveSlotCoeff $ SL.sgActiveSlotsCoeff genesis,
          praosSecurityParam = SecurityParam $ SL.sgSecurityParam genesis,
          praosMaxKESEvo = SL.sgMaxKESEvolutions genesis,
          praosQuorum = SL.sgUpdateQuorum genesis,
          praosMaxMajorPV = maxMajorProtVer,
          praosMaxLovelaceSupply = SL.sgMaxLovelaceSupply genesis,
          praosNetworkId = SL.sgNetworkId genesis,
          praosSystemStart = SystemStart $ SL.sgSystemStart genesis
        }
