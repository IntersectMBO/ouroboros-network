{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE UndecidableSuperClasses  #-}

{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE UndecidableInstances     #-}
module Ouroboros.Consensus.Shelley.Node
  ( MaxMajorProtVer (..)
  , ProtocolParamsAllegra (..)
  , ProtocolParamsMary (..)
  , ProtocolParamsShelley (..)
  , ProtocolParamsShelleyBased (..)
  , SL.Nonce (..)
  , SL.ProtVer (..)
  , SL.ShelleyGenesis (..)
  , SL.ShelleyGenesisStaking (..)
  , SL.emptyGenesisStaking
  , TPraosLeaderCredentials (..)
  , protocolClientInfoShelley
  , protocolInfoShelley
  , protocolInfoShelleyBased
  , registerGenesisStaking
  , registerInitialFunds
  , shelleyBlockForging
  , shelleySharedBlockForging
  , tpraosBlockIssuerVKey
  , validateGenesis
  ) where

import           Control.Monad.Except (Except)
import           Data.Bifunctor (first)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.SOP.Strict
import           Data.Text (Text)
import qualified Data.Text as Text
import           GHC.Stack (HasCallStack)

import qualified Cardano.Crypto.VRF as VRF
import           Cardano.Slotting.EpochInfo
import           Cardano.Slotting.Time (mkSlotLength)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Config.SupportsNode
import qualified Ouroboros.Consensus.HardFork.History as History
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

import qualified Cardano.Ledger.Shelley.Constraints as SL (makeTxOut)
import           Cardano.Ledger.Val (coin, inject, (<->))
import qualified Shelley.Spec.Ledger.API as SL
import qualified Shelley.Spec.Ledger.LedgerState as SL (stakeDistr)
import qualified Shelley.Spec.Ledger.OCert as Absolute (KESPeriod (..))

import           Ouroboros.Consensus.Shelley.Eras
import           Ouroboros.Consensus.Shelley.Ledger
import           Ouroboros.Consensus.Shelley.Ledger.Inspect ()
import           Ouroboros.Consensus.Shelley.Ledger.NetworkProtocolVersion ()
import           Ouroboros.Consensus.Shelley.Node.Serialisation ()
import           Ouroboros.Consensus.Shelley.Protocol
import           Ouroboros.Consensus.Shelley.Protocol.HotKey (HotKey)
import qualified Ouroboros.Consensus.Shelley.Protocol.HotKey as HotKey

{-------------------------------------------------------------------------------
  Credentials
-------------------------------------------------------------------------------}

data TPraosLeaderCredentials c = TPraosLeaderCredentials {
      -- | The unevolved signing KES key (at evolution 0).
      --
      -- Note that this is not inside 'TPraosCanBeLeader' since it gets evolved
      -- automatically, whereas 'TPraosCanBeLeader' does not change.
      tpraosLeaderCredentialsInitSignKey :: SL.SignKeyKES c
    , tpraosLeaderCredentialsCanBeLeader :: TPraosCanBeLeader c
      -- | Identifier for this set of credentials.
      --
      -- Useful when the node is running with multiple sets of credentials.
    , tpraosLeaderCredentialsLabel       :: Text
    }

tpraosBlockIssuerVKey ::
     TPraosLeaderCredentials c -> SL.VKey 'SL.BlockIssuer c
tpraosBlockIssuerVKey =
    tpraosCanBeLeaderColdVerKey . tpraosLeaderCredentialsCanBeLeader

{-------------------------------------------------------------------------------
  BlockForging
-------------------------------------------------------------------------------}

type instance CannotForge (ShelleyBlock era) = TPraosCannotForge (EraCrypto era)

type instance ForgeStateInfo (ShelleyBlock era) = HotKey.KESInfo

type instance ForgeStateUpdateError (ShelleyBlock era) = HotKey.KESEvolutionError

-- | Create a 'BlockForging' record for a single era.
--
-- In case the same credentials should be shared across multiple Shelley-based
-- eras, use 'shelleySharedBlockForging'.
shelleyBlockForging ::
     forall m era. (ShelleyBasedEra era, IOLike m)
  => TPraosParams
  -> TPraosLeaderCredentials (EraCrypto era)
  -> m (BlockForging m (ShelleyBlock era))
shelleyBlockForging tpraosParams credentials =
    aux <$> shelleySharedBlockForging (Proxy @'[era]) tpraosParams credentials
  where
    aux ::
         NP (BlockForging m :.: ShelleyBlock) '[era]
      -> BlockForging m (ShelleyBlock era)
    aux = unComp . hd

-- | Needed in 'shelleySharedBlockForging' because we can't partially apply
-- equality constraints.
class    (ShelleyBasedEra era, EraCrypto era ~ c) => ShelleyEraWithCrypto c era
instance (ShelleyBasedEra era, EraCrypto era ~ c) => ShelleyEraWithCrypto c era

-- | Create a 'BlockForging' record for each of the given Shelley-based eras,
-- safely sharing the same set of credentials for all of them.
--
-- The name of the era (separated by a @_@) will be appended to each
-- 'forgeLabel'.
shelleySharedBlockForging ::
     forall m c eras.
     ( PraosCrypto c
     , All (ShelleyEraWithCrypto c) eras
     , IOLike m
     )
  => Proxy eras
  -> TPraosParams
  -> TPraosLeaderCredentials c
  -> m (NP (BlockForging m :.: ShelleyBlock) eras)
shelleySharedBlockForging
                    _
                    TPraosParams {..}
                    TPraosLeaderCredentials {
                        tpraosLeaderCredentialsInitSignKey = initSignKey
                      , tpraosLeaderCredentialsCanBeLeader = canBeLeader
                      , tpraosLeaderCredentialsLabel       = label
                      } = do
    hotKey <- HotKey.mkHotKey @m @c initSignKey startPeriod tpraosMaxKESEvo
    return $ hcpure (Proxy @(ShelleyEraWithCrypto c)) (Comp (aux hotKey))
  where
    aux ::
         forall era. ShelleyEraWithCrypto c era
      => HotKey c m -> BlockForging m (ShelleyBlock era)
    aux hotKey = BlockForging {
          forgeLabel       = label <> "_" <> shelleyBasedEraName (Proxy @era)
        , canBeLeader      = canBeLeader
        , updateForgeState = \_ curSlot _ ->
                                 forgeStateUpdateInfoFromUpdateInfo <$>
                                   HotKey.evolve hotKey (slotToPeriod curSlot)
        , checkCanForge    = \cfg curSlot _tickedChainDepState ->
                                 tpraosCheckCanForge
                                   (configConsensus cfg)
                                   forgingVRFHash
                                   curSlot
        , forgeBlock       = forgeShelleyBlock hotKey canBeLeader
        }

    forgingVRFHash :: SL.Hash c (SL.VerKeyVRF c)
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
validateGenesis ::
     ShelleyBasedEra era
  => SL.ShelleyGenesis era -> Either String ()
validateGenesis = first errsToString . SL.validateGenesis
  where
    errsToString :: [SL.ValidationErr] -> String
    errsToString errs =
        Text.unpack $ Text.unlines
          ("Invalid genesis config:" : map SL.describeValidationErr errs)

-- | Parameters common to all Shelley-based ledgers.
--
-- When running a chain with multiple Shelley-based eras, in addition to the
-- per-era protocol parameters, one value of 'ProtocolParamsShelleyBased' will
-- be needed, which is shared among all Shelley-based eras.
--
-- The @era@ parameter determines from which era the genesis config will be
-- used.
data ProtocolParamsShelleyBased era = ProtocolParamsShelleyBased {
      shelleyBasedGenesis           :: SL.ShelleyGenesis era
      -- | The initial nonce, typically derived from the hash of Genesis
      -- config JSON file.
      --
      -- WARNING: chains using different values of this parameter will be
      -- mutually incompatible.
    , shelleyBasedInitialNonce      :: SL.Nonce
    , shelleyBasedLeaderCredentials :: [TPraosLeaderCredentials (EraCrypto era)]
    }

-- | Parameters needed to run Shelley
data ProtocolParamsShelley = ProtocolParamsShelley {
      shelleyProtVer :: SL.ProtVer
    }

-- | Parameters needed to run Allegra
data ProtocolParamsAllegra = ProtocolParamsAllegra {
      allegraProtVer :: SL.ProtVer
    }

-- | Parameters needed to run Mary
data ProtocolParamsMary = ProtocolParamsMary {
      maryProtVer :: SL.ProtVer
    }

protocolInfoShelley ::
     forall m c. (IOLike m, ShelleyBasedEra (ShelleyEra c))
  => ProtocolParamsShelleyBased (ShelleyEra c)
  -> ProtocolParamsShelley
  -> ProtocolInfo m (ShelleyBlock (ShelleyEra c))
protocolInfoShelley protocolParamsShelleyBased
                    ProtocolParamsShelley {
                        shelleyProtVer = protVer
                      } =
    protocolInfoShelleyBased protocolParamsShelleyBased protVer

protocolInfoShelleyBased ::
     forall m era. (IOLike m, ShelleyBasedEra era)
  => ProtocolParamsShelleyBased era
  -> SL.ProtVer
  -> ProtocolInfo m (ShelleyBlock era)
protocolInfoShelleyBased ProtocolParamsShelleyBased {
                             shelleyBasedGenesis           = genesis
                           , shelleyBasedInitialNonce      = initialNonce
                           , shelleyBasedLeaderCredentials = credentialss
                           }
                         protVer =
    assertWithMsg (validateGenesis genesis) $
    ProtocolInfo {
        pInfoConfig       = topLevelConfig
      , pInfoInitLedger   = initExtLedgerState
      , pInfoBlockForging =
          traverse (shelleyBlockForging tpraosParams) credentialss
      }
  where
    maxMajorProtVer :: MaxMajorProtVer
    maxMajorProtVer = MaxMajorProtVer $ SL.pvMajor protVer

    topLevelConfig :: TopLevelConfig (ShelleyBlock era)
    topLevelConfig = TopLevelConfig {
        topLevelConfigProtocol = consensusConfig
      , topLevelConfigLedger   = ledgerConfig
      , topLevelConfigBlock    = blockConfig
      , topLevelConfigCodec    = ShelleyCodecConfig
      , topLevelConfigStorage  = storageConfig
      }

    consensusConfig :: ConsensusConfig (BlockProtocol (ShelleyBlock era))
    consensusConfig = TPraosConfig {
        tpraosParams
      , tpraosEpochInfo = epochInfo
      }

    ledgerConfig :: LedgerConfig (ShelleyBlock era)
    ledgerConfig = mkShelleyLedgerConfig genesis epochInfo maxMajorProtVer

    epochInfo :: EpochInfo (Except History.PastHorizonException)
    epochInfo =
        fixedEpochInfo
          (SL.sgEpochLength genesis)
          (mkSlotLength $ SL.sgSlotLength genesis)

    tpraosParams :: TPraosParams
    tpraosParams = mkTPraosParams maxMajorProtVer initialNonce genesis

    blockConfig :: BlockConfig (ShelleyBlock era)
    blockConfig =
        mkShelleyBlockConfig
          protVer
          genesis
          (tpraosBlockIssuerVKey <$> credentialss)

    storageConfig :: StorageConfig (ShelleyBlock era)
    storageConfig = ShelleyStorageConfig {
          shelleyStorageConfigSlotsPerKESPeriod = tpraosSlotsPerKESPeriod tpraosParams
        , shelleyStorageConfigSecurityParam     = tpraosSecurityParam     tpraosParams
        }

    initLedgerState :: LedgerState (ShelleyBlock era)
    initLedgerState = ShelleyLedgerState {
        shelleyLedgerTip        = Origin
      , shelleyLedgerState      =
          registerGenesisStaking (SL.sgStaking genesis) $
            SL.initialState genesis ()
      , shelleyLedgerTransition = ShelleyTransitionInfo {shelleyAfterVoting = 0}
      }

    initChainDepState :: TPraosState (EraCrypto era)
    initChainDepState = TPraosState Origin $
      SL.initialChainDepState initialNonce (SL.sgGenDelegs genesis)

    initExtLedgerState :: ExtLedgerState (ShelleyBlock era)
    initExtLedgerState = ExtLedgerState {
        ledgerState = initLedgerState
      , headerState = genesisHeaderState initChainDepState
      }

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

instance ShelleyBasedEra era => NodeInitStorage (ShelleyBlock era) where
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

instance ShelleyBasedEra era => BlockSupportsMetrics (ShelleyBlock era) where
  isSelfIssued cfg hdr =
       case csvSelfIssued $ selectView cfg hdr of
         SelfIssued    -> IsSelfIssued
         NotSelfIssued -> IsNotSelfIssued

-- TODO It's annoying that I have to put a SerialiseNodeToClient constraint
-- here.
--
-- Can I make this non-orphaned or put this instance next to PartialLedgerConfig?
instance ShelleyBasedEra era => RunNode (ShelleyBlock era)

{-------------------------------------------------------------------------------
  Register genesis staking
-------------------------------------------------------------------------------}

-- | Register the initial staking information in the 'SL.NewEpochState'.
--
-- HERE BE DRAGONS! This function is intended to help in testing.
--
-- In production, the genesis should /not/ contain any initial staking.
--
-- Any existing staking information is overridden, but the UTxO is left
-- untouched.
--
-- TODO adapt and reuse @registerGenesisStaking@ from @cardano-ledger-specs@.
registerGenesisStaking ::
     forall era. ShelleyBasedEra era
  => SL.ShelleyGenesisStaking (EraCrypto era)
  -> SL.NewEpochState era
  -> SL.NewEpochState era
registerGenesisStaking staking nes = nes {
      SL.nesEs = epochState {
          SL.esLState = ledgerState {
          SL._delegationState = dpState {
              SL._dstate = dState'
            , SL._pstate = pState'
            }
        }
        , SL.esSnapshots = (SL.esSnapshots epochState) {
              SL._pstakeMark = initSnapShot
            }
        }

    -- Note that this is only applicable in the initial configuration where
    -- there is no existing stake distribution, since it would completely
    -- overwrite any such thing.
    , SL.nesPd = SL.calculatePoolDistr initSnapShot
    }
  where
    SL.ShelleyGenesisStaking { sgsPools, sgsStake } = staking
    SL.NewEpochState { nesEs = epochState } = nes
    ledgerState = SL.esLState epochState
    dpState = SL._delegationState ledgerState

    -- New delegation state. Since we're using base addresses, we only care
    -- about updating the '_delegations' field.
    --
    -- See STS DELEG for details
    dState' :: SL.DState (EraCrypto era)
    dState' = (SL._dstate dpState) {
          SL._rewards = Map.map (const $ SL.Coin 0)
                      . Map.mapKeys SL.KeyHashObj
                      $ sgsStake
        , SL._delegations = Map.mapKeys SL.KeyHashObj sgsStake
        }

    -- We consider pools as having been registered in slot 0
    -- See STS POOL for details
    pState' :: SL.PState (EraCrypto era)
    pState' = (SL._pstate dpState) {
          SL._pParams = sgsPools
        }

    -- The new stake distribution is made on the basis of a snapshot taken
    -- during the previous epoch. We create a "fake" snapshot in order to
    -- establish an initial stake distribution.
    initSnapShot :: SL.SnapShot (EraCrypto era)
    initSnapShot =
        SL.stakeDistr
          @era
          (SL._utxo (SL._utxoState ledgerState))
          dState'
          pState'

-- | Register the initial funds in the 'SL.NewEpochState'.
--
-- HERE BE DRAGONS! This function is intended to help in testing.
--
-- In production, the genesis should /not/ contain any initial funds.
--
-- The given funds are /added/ to the existing UTxO.
--
-- PRECONDITION: the given funds must not be part of the existing UTxO.
-- > forall (addr, _) in initialFunds.
-- >    Map.notElem (SL.initialFundsPseudoTxIn addr) existingUTxO
--
-- PROPERTY:
-- >    genesisUTxO genesis
-- > == <genesisUTxO'> (sgInitialFunds genesis)
-- > == <extractUTxO> (registerInitialFunds (sgInitialFunds genesis)
-- >                                        <empty NewEpochState>)
--
-- TODO move to @cardano-ledger-specs@.
registerInitialFunds ::
     forall era.
     ( ShelleyBasedEra era
     , HasCallStack
     )
  => Map (SL.Addr (EraCrypto era)) SL.Coin
  -> SL.NewEpochState era
  -> SL.NewEpochState era
registerInitialFunds initialFunds nes = nes {
      SL.nesEs = epochState {
          SL.esAccountState = accountState'
        , SL.esLState       = ledgerState'
        }
    }
  where
    epochState   = SL.nesEs          nes
    accountState = SL.esAccountState epochState
    ledgerState  = SL.esLState       epochState
    utxoState    = SL._utxoState     ledgerState
    utxo         = SL._utxo          utxoState
    reserves     = SL._reserves      accountState

    initialFundsUtxo :: SL.UTxO era
    initialFundsUtxo = SL.UTxO $ Map.fromList [
          (txIn, txOut)
        | (addr, amount) <- Map.toList initialFunds
        ,  let txIn  = SL.initialFundsPseudoTxIn addr
               txOut = SL.makeTxOut (Proxy @era) addr (inject amount)
        ]

    utxo' = mergeUtxoNoOverlap utxo initialFundsUtxo

    -- Update the reserves
    accountState' = accountState {
          SL._reserves = reserves <-> coin (SL.balance initialFundsUtxo)
        }

    ledgerState' = ledgerState {
          SL._utxoState = utxoState {
              SL._utxo = utxo'
            }
        }

    -- | Merge two UTxOs, throw an 'error' in case of overlap
    mergeUtxoNoOverlap ::
         HasCallStack
      => SL.UTxO era -> SL.UTxO era -> SL.UTxO era
    mergeUtxoNoOverlap (SL.UTxO m1) (SL.UTxO m2) = SL.UTxO $
        Map.unionWithKey
          (\k _ _ -> error $ "initial fund part of UTxO: " <> show k)
          m1
          m2
