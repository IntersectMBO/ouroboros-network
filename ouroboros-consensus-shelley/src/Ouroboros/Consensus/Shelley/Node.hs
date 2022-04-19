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
{-# LANGUAGE UndecidableInstances     #-}
{-# LANGUAGE UndecidableSuperClasses  #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Shelley.Node (
    MaxMajorProtVer (..)
  , ProtocolParamsAllegra (..)
  , ProtocolParamsAlonzo (..)
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
import           Ouroboros.Consensus.Mempool.TxLimits (TxLimits)
import qualified Ouroboros.Consensus.Mempool.TxLimits as TxLimits
import           Ouroboros.Consensus.Node.InitStorage
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Storage.ImmutableDB (simpleChunkInfo)
import           Ouroboros.Consensus.Util.Assert
import           Ouroboros.Consensus.Util.IOLike

import qualified Cardano.Ledger.Era as Core
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Ledger.Shelley.Constraints as SL (makeTxOut)
import qualified Cardano.Ledger.Shelley.LedgerState as SL
                     (incrementalStakeDistr, updateStakeDistribution)
import           Cardano.Ledger.Val (coin, inject, (<->))
import qualified Cardano.Protocol.TPraos.API as SL
import qualified Cardano.Protocol.TPraos.BHeader as SL
import qualified Cardano.Protocol.TPraos.OCert as Absolute (KESPeriod (..))
import qualified Cardano.Protocol.TPraos.OCert as SL

import qualified Data.Compact.SplitMap as SplitMap
import qualified Data.UMap as UM
import           Ouroboros.Consensus.Protocol.Ledger.HotKey (HotKey)
import qualified Ouroboros.Consensus.Protocol.Ledger.HotKey as HotKey
import           Ouroboros.Consensus.Protocol.TPraos
import           Ouroboros.Consensus.Shelley.Eras
import           Ouroboros.Consensus.Shelley.Ledger
import           Ouroboros.Consensus.Shelley.Ledger.Inspect ()
import           Ouroboros.Consensus.Shelley.Ledger.NetworkProtocolVersion ()
import           Ouroboros.Consensus.Shelley.Node.Serialisation ()

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
     forall m era. (ShelleyBasedEra era, TxLimits (ShelleyBlock era), IOLike m)
  => TPraosParams
  -> TxLimits.Overrides (ShelleyBlock era)
  -> TPraosLeaderCredentials (EraCrypto era)
  -> m (BlockForging m (ShelleyBlock era))
shelleyBlockForging tpraosParams maxTxCapacityOverrides credentials =
      aux <$> shelleySharedBlockForging
        (Proxy @'[era])
        tpraosParams
        credentials
        (Comp maxTxCapacityOverrides :* Nil)
  where
    aux ::
         NP (BlockForging m :.: ShelleyBlock) '[era]
      -> BlockForging m (ShelleyBlock era)
    aux = unComp . hd

-- | Needed in 'shelleySharedBlockForging' because we can't partially apply
-- equality constraints.
class    (ShelleyBasedEra era, TxLimits (ShelleyBlock era), EraCrypto era ~ c) => ShelleyEraWithCrypto c era
instance (ShelleyBasedEra era, TxLimits (ShelleyBlock era), EraCrypto era ~ c) => ShelleyEraWithCrypto c era

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
  -> NP    (TxLimits.Overrides :.: ShelleyBlock) eras
  -> m (NP (BlockForging m     :.: ShelleyBlock) eras)
shelleySharedBlockForging
                    _
                    TPraosParams {..}
                    TPraosLeaderCredentials {
                        tpraosLeaderCredentialsInitSignKey = initSignKey
                      , tpraosLeaderCredentialsCanBeLeader = canBeLeader
                      , tpraosLeaderCredentialsLabel       = label
                      }
                    maxTxCapacityOverridess = do
    hotKey <- HotKey.mkHotKey @m @c initSignKey startPeriod tpraosMaxKESEvo
    return $
      hcmap
        (Proxy @(ShelleyEraWithCrypto c))
        (aux hotKey)
        maxTxCapacityOverridess
  where
    aux ::
         forall era. ShelleyEraWithCrypto c era
      => HotKey c m
      -> (TxLimits.Overrides :.: ShelleyBlock) era
      -> (BlockForging m     :.: ShelleyBlock) era
    aux hotKey (Comp maxTxCapacityOverrides) = Comp $ BlockForging {
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
        , forgeBlock       = \cfg ->
            forgeShelleyBlock
              hotKey
              canBeLeader
              cfg
              maxTxCapacityOverrides
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
data ProtocolParamsShelley c = ProtocolParamsShelley {
      shelleyProtVer                :: SL.ProtVer
    , shelleyMaxTxCapacityOverrides :: TxLimits.Overrides (ShelleyBlock (ShelleyEra c))
    }

-- | Parameters needed to run Allegra
data ProtocolParamsAllegra c = ProtocolParamsAllegra {
      allegraProtVer                :: SL.ProtVer
    , allegraMaxTxCapacityOverrides :: TxLimits.Overrides (ShelleyBlock (AllegraEra c))
    }

-- | Parameters needed to run Mary
data ProtocolParamsMary c = ProtocolParamsMary {
      maryProtVer                :: SL.ProtVer
    , maryMaxTxCapacityOverrides :: TxLimits.Overrides (ShelleyBlock (MaryEra c))
    }

-- | Parameters needed to run Alonzo
data ProtocolParamsAlonzo c = ProtocolParamsAlonzo {
      alonzoProtVer                :: SL.ProtVer
    , alonzoMaxTxCapacityOverrides :: TxLimits.Overrides (ShelleyBlock (AlonzoEra c))
    }

protocolInfoShelley ::
     forall m c. (IOLike m, ShelleyBasedEra (ShelleyEra c), TxLimits (ShelleyBlock (ShelleyEra c)))
  => ProtocolParamsShelleyBased (ShelleyEra c)
  -> ProtocolParamsShelley c
  -> ProtocolInfo m (ShelleyBlock (ShelleyEra c))
protocolInfoShelley protocolParamsShelleyBased
                    ProtocolParamsShelley {
                        shelleyProtVer                = protVer
                      , shelleyMaxTxCapacityOverrides = maxTxCapacityOverrides
                      } =
    protocolInfoShelleyBased
      protocolParamsShelleyBased
      ()  -- trivial translation context
      protVer
      maxTxCapacityOverrides

protocolInfoShelleyBased ::
     forall m era. (IOLike m, ShelleyBasedEra era, TxLimits (ShelleyBlock era))
  => ProtocolParamsShelleyBased era
  -> Core.TranslationContext era
  -> SL.ProtVer
  -> TxLimits.Overrides (ShelleyBlock era)
  -> ProtocolInfo m     (ShelleyBlock era)
protocolInfoShelleyBased ProtocolParamsShelleyBased {
                             shelleyBasedGenesis           = genesis
                           , shelleyBasedInitialNonce      = initialNonce
                           , shelleyBasedLeaderCredentials = credentialss
                           }
                         transCtxt
                         protVer
                         maxTxCapacityOverrides =
    assertWithMsg (validateGenesis genesis) $
    ProtocolInfo {
        pInfoConfig       = topLevelConfig
      , pInfoInitLedger   = initExtLedgerState
      , pInfoBlockForging =
          traverse
            (shelleyBlockForging tpraosParams maxTxCapacityOverrides)
            credentialss
      }
  where

    -- | Currently for all existing eras in ledger-specs (Shelley, Allegra, Mary
    -- and Alonzo) it happens to be the case that AdditionalGenesisConfig and
    -- TranslationContext are instantiated to the same type.
    -- We take advantage of this fact below to simplify our code, but we are
    -- aware that this might change in future (for new eras), breaking this
    -- code.
    --
    -- see type equality constraint in
    -- Ouroboros.Consensus.Shelley.Eras.ShelleyBasedEra
    additionalGenesisConfig :: SL.AdditionalGenesisConfig era
    additionalGenesisConfig = transCtxt

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
    ledgerConfig = mkShelleyLedgerConfig genesis transCtxt epochInfo maxMajorProtVer

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
            SL.initialState genesis additionalGenesisConfig
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
  -- | Premature optimisation: we assume everywhere that metrics are
  -- cheap, so micro-optimise checking whether the issuer vkey is one of our
  -- own vkeys.
  --
  -- * Equality of vkeys takes roughly 40ns
  -- * Hashing a vkey takes roughly 850ns
  -- * Equality of hashes takes roughly 10ns
  --
  -- We want to avoid the hashing of a vkey as it is more expensive than
  -- simply doing a linear search, comparing vkeys for equality. Only when
  -- we have to do a linear search across a large number of vkeys does it
  -- become more efficient to first hash the vkey and look up its hash in
  -- the map.
  --
  -- We could try to be clever and estimate the number of keys after which
  -- we switch from a linear search to hashing + a O(log n) map lookup, but
  -- we keep it (relatively) simple and optimise for the common case: 0 or 1
  -- key.
  isSelfIssued cfg (ShelleyHeader shdr _) = case Map.size issuerVKeys of
      -- The most common case: a non-block producing node
      0 -> IsNotSelfIssued
      -- A block producing node with a single set of credentials: just do an
      -- equality check of the single VKey, skipping the more expensive
      -- computation of the hash.
      1 | SL.bheaderVk hdrBody `elem` issuerVKeys
        -> IsSelfIssued
        | otherwise
        -> IsNotSelfIssued
      -- When we are running with multiple sets of credentials, which should
      -- only happen when benchmarking, do a hash lookup, as the number of
      -- keys can grow to 100-250.
      _ | SL.hashKey (SL.bheaderVk hdrBody) `Map.member` issuerVKeys
        -> IsSelfIssued
        | otherwise
        -> IsNotSelfIssued
    where
      hdrBody :: SL.BHBody (EraCrypto era)
      hdrBody = SL.bhbody shdr

      issuerVKeys :: Map (SL.KeyHash 'SL.BlockIssuer (EraCrypto era))
                         (SL.VKey 'SL.BlockIssuer (EraCrypto era))
      issuerVKeys = shelleyBlockIssuerVKeys cfg


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
          SL.lsDPState = dpState {
              SL.dpsDState = dState'
            , SL.dpsPState = pState'
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
    dpState = SL.lsDPState ledgerState

    -- New delegation state. Since we're using base addresses, we only care
    -- about updating the '_delegations' field.
    --
    -- See STS DELEG for details
    dState' :: SL.DState (EraCrypto era)
    dState' = (SL.dpsDState dpState) {
          SL._unified = UM.unify
            ( Map.map (const $ SL.Coin 0)
                      . Map.mapKeys SL.KeyHashObj
                      $ sgsStake)
            ( Map.mapKeys SL.KeyHashObj sgsStake )
            mempty
        }
    -- We consider pools as having been registered in slot 0
    -- See STS POOL for details
    pState' :: SL.PState (EraCrypto era)
    pState' = (SL.dpsPState dpState) {
          SL._pParams = sgsPools
        }

    -- The new stake distribution is made on the basis of a snapshot taken
    -- during the previous epoch. We create a "fake" snapshot in order to
    -- establish an initial stake distribution.
    initSnapShot :: SL.SnapShot (EraCrypto era)
    initSnapShot =
        -- Since we build a stake from nothing, we first initialise an
        -- 'IncrementalStake' as empty, and then:
        --
        -- 1. Add the initial UTxO, whilst deleting nothing.
        -- 2. Update the stake map given the initial delegation.
        SL.incrementalStakeDistr
          -- Note that 'updateStakeDistribution' takes first the set of UTxO to
          -- delete, and then the set to add. In our case, there is nothing to
          -- delete, since this is an initial UTxO set.
          (SL.updateStakeDistribution mempty mempty (SL._utxo (SL.lsUTxOState ledgerState)))
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
    utxoState    = SL.lsUTxOState    ledgerState
    utxo         = SL._utxo          utxoState
    reserves     = SL._reserves      accountState

    initialFundsUtxo :: SL.UTxO era
    initialFundsUtxo = SL.UTxO $ SplitMap.fromList [
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

    -- Since we only add entries to our UTxO, rather than spending them, there
    -- is nothing to delete in the incremental update.
    utxoToDel     = SL.UTxO mempty
    ledgerState'  = ledgerState {
          SL.lsUTxOState = utxoState {
              SL._utxo        = utxo',
              -- Normally we would incrementally update here. But since we pass
              -- the full UTxO as "toAdd" rather than a delta, we simply
              -- reinitialise the full incremental stake.
              SL._stakeDistro = SL.updateStakeDistribution mempty utxoToDel utxo'
            }
        }

    -- | Merge two UTxOs, throw an 'error' in case of overlap
    mergeUtxoNoOverlap ::
         HasCallStack
      => SL.UTxO era -> SL.UTxO era -> SL.UTxO era
    mergeUtxoNoOverlap (SL.UTxO m1) (SL.UTxO m2) = SL.UTxO $
        SplitMap.unionWithKey
          (\k _ _ -> error $ "initial fund part of UTxO: " <> show k)
          m1
          m2
