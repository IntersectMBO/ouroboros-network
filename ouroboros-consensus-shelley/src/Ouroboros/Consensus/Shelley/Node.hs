{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Shelley.Node (
    protocolInfoShelley
  , ProtocolParamsShelley (..)
  , ProtocolParamsAllegra (..)
  , ProtocolParamsMary (..)
  , protocolClientInfoShelley
  , SL.ShelleyGenesis (..)
  , SL.ShelleyGenesisStaking (..)
  , TPraosLeaderCredentials (..)
  , shelleyBlockForging
  , tpraosBlockIssuerVKey
  , SL.ProtVer (..)
  , SL.Nonce (..)
  , MaxMajorProtVer (..)
  , SL.emptyGenesisStaking
  , validateGenesis
  ) where

import           Data.Bifunctor (first)
import           Data.Foldable (toList)
import           Data.Functor.Identity (Identity)
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import qualified Data.Text as Text

import qualified Cardano.Crypto.VRF as VRF
import           Cardano.Slotting.EpochInfo

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Config.SupportsNode
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Node.InitStorage
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Storage.ImmutableDB (simpleChunkInfo)
import           Ouroboros.Consensus.Util.Assert
import           Ouroboros.Consensus.Util.IOLike

import           Cardano.Ledger.Val ((<->))
import qualified Shelley.Spec.Ledger.API as SL
import qualified Shelley.Spec.Ledger.OCert as Absolute (KESPeriod (..))

import           Ouroboros.Consensus.Shelley.Eras
import           Ouroboros.Consensus.Shelley.Ledger
import           Ouroboros.Consensus.Shelley.Ledger.Inspect ()
import           Ouroboros.Consensus.Shelley.Ledger.NetworkProtocolVersion ()
import           Ouroboros.Consensus.Shelley.Node.Serialisation ()
import           Ouroboros.Consensus.Shelley.Protocol
import qualified Ouroboros.Consensus.Shelley.Protocol.HotKey as HotKey

{-------------------------------------------------------------------------------
  Credentials
-------------------------------------------------------------------------------}

data TPraosLeaderCredentials era = TPraosLeaderCredentials {
      -- | The unevolved signing KES key (at evolution 0).
      --
      -- Note that this is not inside 'TPraosCanBeLeader' since it gets evolved
      -- automatically, whereas 'TPraosCanBeLeader' does not change.
      tpraosLeaderCredentialsInitSignKey :: SL.SignKeyKES era
    , tpraosLeaderCredentialsCanBeLeader :: TPraosCanBeLeader era
      -- | Identifier for this set of credentials.
      --
      -- Useful when the node is running with multiple sets of credentials.
    , tpraosLeaderCredentialsLabel       :: Text
    }

tpraosBlockIssuerVKey ::
     TPraosLeaderCredentials era -> SL.VKey 'SL.BlockIssuer era
tpraosBlockIssuerVKey =
    tpraosCanBeLeaderColdVerKey . tpraosLeaderCredentialsCanBeLeader

{-------------------------------------------------------------------------------
  BlockForging
-------------------------------------------------------------------------------}

type instance CannotForge (ShelleyBlock era) = TPraosCannotForge era

type instance ForgeStateInfo (ShelleyBlock era) = HotKey.KESInfo

type instance ForgeStateUpdateError (ShelleyBlock era) = HotKey.KESEvolutionError

shelleyBlockForging
  :: forall m era. (TPraosCrypto era, IOLike m)
  => TPraosParams
  -> TPraosLeaderCredentials era
  -> m (BlockForging m (ShelleyBlock era))
shelleyBlockForging TPraosParams {..}
                    TPraosLeaderCredentials {
                        tpraosLeaderCredentialsInitSignKey = initSignKey
                      , tpraosLeaderCredentialsCanBeLeader = canBeLeader
                      , tpraosLeaderCredentialsLabel       = label
                      } = do
    hotKey <- HotKey.mkHotKey initSignKey startPeriod tpraosMaxKESEvo
    return BlockForging {
        forgeLabel       = label
      , canBeLeader      = canBeLeader
      , updateForgeState = \curSlot ->
                               ForgeStateUpdateInfo <$>
                                 HotKey.evolve hotKey (slotToPeriod curSlot)
      , checkCanForge    = \cfg curSlot _tickedChainDepState ->
                               tpraosCheckCanForge
                                 (configConsensus cfg)
                                 forgingVRFHash
                                 curSlot
      , forgeBlock       = forgeShelleyBlock hotKey canBeLeader
      }
  where
    forgingVRFHash :: SL.Hash era (SL.VerKeyVRF era)
    forgingVRFHash =
          SL.hashVerKeyVRF
        . VRF.deriveVerKeyVRF
        . tpraosCanBeLeaderSignKeyVRF
        $ canBeLeader

    startPeriod :: Absolute.KESPeriod
    startPeriod = SL.ocertKESPeriod $ tpraosCanBeLeaderOpCert canBeLeader

    slotToPeriod :: SlotNo -> Absolute.KESPeriod
    slotToPeriod (SlotNo slot) =
        SL.KESPeriod $ fromIntegral $ slot `div` tpraosSlotsPerKESPeriod

{-------------------------------------------------------------------------------
  ProtocolInfo
-------------------------------------------------------------------------------}

-- | Check the validity of the genesis config. To be used in conjunction with
-- 'assertWithMsg'.
validateGenesis :: TPraosCrypto era => SL.ShelleyGenesis era -> Either String ()
validateGenesis = first errsToString . SL.validateGenesis
  where
    errsToString :: [SL.ValidationErr] -> String
    errsToString errs =
        Text.unpack $ Text.unlines
          ("Invalid genesis config:" : map SL.describeValidationErr errs)

-- | Parameters needed to run Shelley
data ProtocolParamsShelley c f = ProtocolParamsShelley {
      shelleyGenesis           :: SL.ShelleyGenesis (ShelleyEra c)
      -- | The initial nonce, typically derived from the hash of Genesis
      -- config JSON file.
      --
      -- WARNING: chains using different values of this parameter will be
      -- mutually incompatible.
    , shelleyInitialNonce      :: SL.Nonce
    , shelleyProtVer           :: SL.ProtVer
    , shelleyLeaderCredentials :: f (TPraosLeaderCredentials (ShelleyEra c))
    }

-- | Parameters needed to run Allegra
data ProtocolParamsAllegra c f = ProtocolParamsAllegra {
      allegraProtVer           :: SL.ProtVer
    , allegraLeaderCredentials :: f (TPraosLeaderCredentials (AllegraEra c))
    }

-- | Parameters needed to run Mary
data ProtocolParamsMary c f = ProtocolParamsMary {
      maryProtVer           :: SL.ProtVer
    , maryLeaderCredentials :: f (TPraosLeaderCredentials (MaryEra c))
    }

protocolInfoShelley ::
     forall m c f. (IOLike m, TPraosCrypto (ShelleyEra c), Foldable f)
  => ProtocolParamsShelley c f
  -> ProtocolInfo m (ShelleyBlock (ShelleyEra c))
protocolInfoShelley ProtocolParamsShelley {
                        shelleyGenesis           = genesis
                      , shelleyInitialNonce      = initialNonce
                      , shelleyProtVer           = protVer
                      , shelleyLeaderCredentials = credentialss
                      } =
    assertWithMsg (validateGenesis genesis) $
    ProtocolInfo {
        pInfoConfig       = topLevelConfig
      , pInfoInitLedger   = initExtLedgerState
      , pInfoBlockForging = shelleyBlockForging tpraosParams <$> toList credentialss
      }
  where
    maxMajorProtVer :: MaxMajorProtVer
    maxMajorProtVer = MaxMajorProtVer $ SL.pvMajor protVer

    topLevelConfig :: TopLevelConfig (ShelleyBlock (ShelleyEra c))
    topLevelConfig = TopLevelConfig {
        topLevelConfigProtocol = consensusConfig
      , topLevelConfigLedger   = ledgerConfig
      , topLevelConfigBlock    = blockConfig
      , topLevelConfigCodec    = ShelleyCodecConfig
      , topLevelConfigStorage  = storageConfig
      }

    consensusConfig :: ConsensusConfig (BlockProtocol (ShelleyBlock (ShelleyEra c)))
    consensusConfig = TPraosConfig {
        tpraosParams
      , tpraosEpochInfo = epochInfo
      }

    ledgerConfig :: LedgerConfig (ShelleyBlock (ShelleyEra c))
    ledgerConfig = mkShelleyLedgerConfig genesis epochInfo maxMajorProtVer

    epochInfo :: EpochInfo Identity
    epochInfo = fixedSizeEpochInfo $ SL.sgEpochLength genesis

    tpraosParams :: TPraosParams
    tpraosParams = mkTPraosParams maxMajorProtVer initialNonce genesis

    blockConfig :: BlockConfig (ShelleyBlock (ShelleyEra c))
    blockConfig =
        mkShelleyBlockConfig
          protVer
          genesis
          (tpraosBlockIssuerVKey <$> toList credentialss)

    storageConfig :: StorageConfig (ShelleyBlock (ShelleyEra c))
    storageConfig = ShelleyStorageConfig {
          shelleyStorageConfigSlotsPerKESPeriod = tpraosSlotsPerKESPeriod tpraosParams
        , shelleyStorageConfigSecurityParam     = tpraosSecurityParam     tpraosParams
        }

    initLedgerState :: LedgerState (ShelleyBlock (ShelleyEra c))
    initLedgerState = ShelleyLedgerState {
        shelleyLedgerTip        = Origin
      , shelleyLedgerState      = SL.chainNes initShelleyState
      , shelleyLedgerTransition = ShelleyTransitionInfo {shelleyAfterVoting = 0}
      }

    initChainDepState :: TPraosState (ShelleyEra c)
    initChainDepState = TPraosState Origin $
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

    initialEpochNo :: EpochNo
    initialEpochNo = 0

    initialUtxo :: SL.UTxO (ShelleyEra c)
    initialUtxo = SL.genesisUtxO genesis

    initShelleyState :: SL.ChainState (ShelleyEra c)
    initShelleyState = registerGenesisStaking $ SL.initialShelleyState
      Origin
      initialEpochNo
      initialUtxo
      (SL.word64ToCoin (SL.sgMaxLovelaceSupply genesis) <-> SL.balance initialUtxo)
      (SL.sgGenDelegs genesis)
      (SL.sgProtocolParams genesis)
      initialNonce

    initExtLedgerState :: ExtLedgerState (ShelleyBlock (ShelleyEra c))
    initExtLedgerState = ExtLedgerState {
        ledgerState = initLedgerState
      , headerState = genesisHeaderState initChainDepState
      }

    -- Register the initial staking.
    --
    -- This function embodies a little more logic than ideal. We might want to
    -- move it into `cardano-ledger-specs.`
    --
    -- HERE BE DRAGONS! This function is intended to help in testing. It should
    -- not be called with anything other than 'emptyGenesisStaking' in
    -- production.
    registerGenesisStaking :: SL.ChainState (ShelleyEra c) -> SL.ChainState (ShelleyEra c)
    registerGenesisStaking cs@(SL.ChainState {chainNes = oldChainNes} ) = cs
        { SL.chainNes = newChainNes }
      where
        SL.ShelleyGenesisStaking { sgsPools, sgsStake } = SL.sgStaking genesis
        oldEpochState = SL.nesEs $ oldChainNes
        oldLedgerState = SL.esLState oldEpochState
        oldDPState = SL._delegationState oldLedgerState

        -- Note that this is only applicable in the initial configuration where
        -- there is no existing stake distribution, since it would completely
        -- overwrite any such thing.
        newPoolDistr = SL.calculatePoolDistr initSnapShot

        newChainNes = oldChainNes
          { SL.nesEs = newEpochState
          , SL.nesPd = newPoolDistr
          }
        newEpochState = oldEpochState
          { SL.esLState = newLedgerState
          , SL.esSnapshots = (SL.esSnapshots oldEpochState)
            { SL._pstakeMark = initSnapShot }
          }
        newLedgerState = oldLedgerState
          { SL._delegationState = newDPState }
        newDPState = oldDPState
          { SL._dstate = newDState
          , SL._pstate = newPState
          }
        -- New delegation state. Since we're using base addresses, we only care
        -- about updating the '_delegations' field.
        --
        -- See STS DELEG for details
        newDState :: SL.DState (ShelleyEra c)
        newDState = (SL._dstate oldDPState) {
          SL._rewards = Map.map (const $ SL.Coin 0)
                      . Map.mapKeys SL.KeyHashObj
                      $ sgsStake
        , SL._delegations = Map.mapKeys SL.KeyHashObj sgsStake
        }

        -- We consider pools as having been registered in slot 0
        -- See STS POOL for details
        newPState :: SL.PState (ShelleyEra c)
        newPState = (SL._pstate oldDPState) {
          SL._pParams = sgsPools
        }

        -- The new stake distribution is made on the basis of a snapshot taken
        -- during the previous epoch. We create a "fake" snapshot in order to
        -- establish an initial stake distribution.
        initSnapShot = SL.SnapShot
          { SL._stake = SL.Stake . Map.fromList $
              [ (stakeCred, stake)
              | (addr, stake) <- Map.toList (SL.sgInitialFunds genesis)
              , Just stakeCred <- [addrStakeCred addr]
              ]
          , SL._delegations = Map.mapKeys SL.KeyHashObj sgsStake
          , SL._poolParams = sgsPools
          }
          where
            addrStakeCred (SL.AddrBootstrap _) = Nothing
            addrStakeCred (SL.Addr _ _ sr) = case sr of
              SL.StakeRefBase sc -> Just sc
              SL.StakeRefPtr _ ->
                error "Pointer stake addresses not allowed in initial snapshot"
              SL.StakeRefNull -> Nothing

protocolClientInfoShelley :: ProtocolClientInfo (ShelleyBlock era)
protocolClientInfoShelley =
    ProtocolClientInfo {
      -- No particular codec configuration is needed for Shelley
      pClientInfoCodecConfig = ShelleyCodecConfig
    }

{-------------------------------------------------------------------------------
  ConfigSupportsNode instance
-------------------------------------------------------------------------------}

instance ConfigSupportsNode (ShelleyBlock era) where
  getSystemStart  = shelleySystemStart
  getNetworkMagic = shelleyNetworkMagic

{-------------------------------------------------------------------------------
  NodeInitStorage instance
-------------------------------------------------------------------------------}

instance TPraosCrypto era => NodeInitStorage (ShelleyBlock era) where
  -- We fix the chunk size to @10k@ so that we have the same chunk size as
  -- Byron. Consequently, a Shelley net will have the same chunk size as the
  -- Byron-to-Shelley net with the same @k@.
  nodeImmutableDbChunkInfo =
        simpleChunkInfo
      . EpochSize
      . (* 10)
      . maxRollbacks
      . shelleyStorageConfigSecurityParam

  nodeCheckIntegrity cfg =
      verifyBlockIntegrity (shelleyStorageConfigSlotsPerKESPeriod cfg)

{-------------------------------------------------------------------------------
  RunNode instance
-------------------------------------------------------------------------------}

instance TPraosCrypto era => RunNode (ShelleyBlock era)
