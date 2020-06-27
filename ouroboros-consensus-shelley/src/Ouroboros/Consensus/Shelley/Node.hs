{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Shelley.Node (
    protocolInfoShelley
  , protocolClientInfoShelley
  , SL.ShelleyGenesis (..)
  , SL.ShelleyGenesisStaking (..)
  , TPraosLeaderCredentials (..)
  , SL.ProtVer
  , SL.Nonce (..)
  , SL.emptyGenesisStaking
  , shelleyMaintainForgeState
  , checkMaxKESEvolutions
  ) where

import           Control.Monad.Except (throwError)
import           Control.Monad.Reader (runReader)
import           Data.Functor.Identity (Identity)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           Cardano.Prelude (Natural)

import           Cardano.Crypto.KES.Class
import           Cardano.Slotting.EpochInfo

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Config.SecurityParam
import           Ouroboros.Consensus.Config.SupportsNode
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Storage.ImmutableDB (simpleChunkInfo)
import           Ouroboros.Consensus.Util.Assert
import           Ouroboros.Consensus.Util.IOLike

import qualified Shelley.Spec.Ledger.Address as SL
import qualified Shelley.Spec.Ledger.BaseTypes as SL
import qualified Shelley.Spec.Ledger.BlockChain as SL
import qualified Shelley.Spec.Ledger.Credential as SL
import qualified Shelley.Spec.Ledger.Delegation.Certificates as SL
import qualified Shelley.Spec.Ledger.EpochBoundary as SL
import qualified Shelley.Spec.Ledger.Genesis as SL
import qualified Shelley.Spec.Ledger.LedgerState as SL
import qualified Shelley.Spec.Ledger.OCert as SL
import qualified Shelley.Spec.Ledger.PParams as SL
import qualified Shelley.Spec.Ledger.STS.Chain as SL
import qualified Shelley.Spec.Ledger.STS.NewEpoch as SL
import qualified Shelley.Spec.Ledger.STS.Prtcl as SL
import qualified Shelley.Spec.Ledger.UTxO as SL

import           Ouroboros.Consensus.Shelley.Ledger
import qualified Ouroboros.Consensus.Shelley.Ledger.History as History
import           Ouroboros.Consensus.Shelley.Ledger.NetworkProtocolVersion ()
import           Ouroboros.Consensus.Shelley.Node.Serialisation ()
import           Ouroboros.Consensus.Shelley.Protocol
import           Ouroboros.Consensus.Shelley.Protocol.Crypto
import           Ouroboros.Consensus.Shelley.Protocol.Crypto.HotKey
                     (HotKey (..))
import qualified Ouroboros.Consensus.Shelley.Protocol.State as State

{-------------------------------------------------------------------------------
  ProtocolInfo
-------------------------------------------------------------------------------}

data TPraosLeaderCredentials c = TPraosLeaderCredentials {
    -- | The unevolved signing KES key (at evolution 0).
    --
    -- Note that this is not inside 'TPraosIsCoreNode' since it gets evolved
    -- automatically, whereas 'TPraosIsCoreNode' does not change.
    tpraosLeaderCredentialsSignKey    :: SignKeyKES (KES c)
  , tpraosLeaderCredentialsIsCoreNode :: TPraosIsCoreNode c
  }


shelleyMaintainForgeState
  :: forall m c. (IOLike m, TPraosCrypto c)
  => TPraosParams
  -> TPraosLeaderCredentials c
  -> MaintainForgeState m (ShelleyBlock c)
shelleyMaintainForgeState TPraosParams{..} (TPraosLeaderCredentials signKeyKES icn) =
    defaultMaintainNoExtraForgeState initHotKey
  where
    SL.KESPeriod start = SL.ocertKESPeriod $ tpraosIsCoreNodeOpCert icn

    initHotKey = HotKey {
        hkStart     = SL.KESPeriod start
      , hkEnd       = SL.KESPeriod (start + fromIntegral tpraosMaxKESEvo)
        -- We get an unevolved KES key
      , hkEvolution = 0
      , hkKey       = signKeyKES
      }

-- | TODO this should be done as part of
-- <https://github.com/input-output-hk/cardano-ledger-specs/issues/1516>
checkMaxKESEvolutions
  :: forall c. TPraosCrypto c
  => SL.ShelleyGenesis c
  -> Either String ()
checkMaxKESEvolutions genesis
    | configuredMaxKESEvo > fromIntegral supportedMaxKESEvo
    = throwError $ mconcat [
          "sgMaxKESEvolutions greater than the supported evolutions"
        , " by the KES algorithm: "
        , show configuredMaxKESEvo
        , " > "
        , show supportedMaxKESEvo
        ]
    | otherwise
    = return ()
  where
    configuredMaxKESEvo = SL.sgMaxKESEvolutions genesis
    -- Max evolutions supported by the chosen KES algorithm
    supportedMaxKESEvo  = totalPeriodsKES (Proxy @(KES c))

protocolInfoShelley
  :: forall m c. (IOLike m, TPraosCrypto c)
  => SL.ShelleyGenesis c
  -> SL.Nonce
     -- ^ The initial nonce, typically derived from the hash of Genesis config
     -- JSON file.
  -> Natural -- ^ Max major protocol version
  -> SL.ProtVer
  -> Maybe (TPraosLeaderCredentials c)
  -> ProtocolInfo m (ShelleyBlock c)
protocolInfoShelley genesis initialNonce maxMajorPV protVer mbCredentials =
    assertWithMsg (checkMaxKESEvolutions genesis) $
    ProtocolInfo {
        pInfoConfig      = topLevelConfig
      , pInfoInitLedger  = initExtLedgerState
      , pInfoLeaderCreds = mkLeaderCreds <$> mbCredentials
      }
  where
    topLevelConfig :: TopLevelConfig (ShelleyBlock c)
    topLevelConfig = TopLevelConfig {
        configConsensus = consensusConfig
      , configIndep     = tpraosParams
      , configLedger    = ledgerConfig
      , configBlock     = blockConfig
      , configCodec     = ShelleyCodecConfig
      }

    consensusConfig :: ConsensusConfig (BlockProtocol (ShelleyBlock c))
    consensusConfig = TPraosConfig {
        tpraosParams
      , tpraosEpochInfo = epochInfo
      }

    ledgerConfig :: LedgerConfig (ShelleyBlock c)
    ledgerConfig = mkShelleyLedgerConfig genesis epochInfo maxMajorPV

    epochInfo :: EpochInfo Identity
    epochInfo = fixedSizeEpochInfo $ SL.sgEpochLength genesis

    tpraosParams :: TPraosParams
    tpraosParams = mkTPraosParams maxMajorPV initialNonce genesis

    mkLeaderCreds :: TPraosLeaderCredentials c
                  -> (TPraosIsCoreNode c, MaintainForgeState m (ShelleyBlock c))
    mkLeaderCreds creds@(TPraosLeaderCredentials _ isACoreNode) = (
          isACoreNode
        , shelleyMaintainForgeState tpraosParams creds
        )

    blockConfig :: BlockConfig (ShelleyBlock c)
    blockConfig = mkShelleyBlockConfig protVer genesis

    initLedgerState :: LedgerState (ShelleyBlock c)
    initLedgerState = ShelleyLedgerState {
        ledgerTip    = GenesisPoint
      , history      = History.empty
      , shelleyState = SL.chainNes initShelleyState
      }

    initChainDepState :: State.TPraosState c
    initChainDepState = State.empty Origin $
      SL.PrtclState
         (SL.chainOCertIssue     initShelleyState)
         (SL.chainEpochNonce     initShelleyState)
         (SL.chainEvolvingNonce  initShelleyState)
         (SL.chainCandidateNonce initShelleyState)
         (SL.chainPrevEpochNonce initShelleyState)

    initialEpochNo :: EpochNo
    initialEpochNo = 0

    initialUtxo :: SL.UTxO c
    initialUtxo = SL.genesisUtxO genesis

    initShelleyState :: SL.ChainState c
    initShelleyState = registerGenesisStaking $ SL.initialShelleyState
      Origin
      initialEpochNo
      initialUtxo
      (fromIntegral (SL.sgMaxLovelaceSupply genesis) - SL.balance initialUtxo)
      (SL.sgGenDelegs genesis)
      oSched
      (SL.sgProtocolParams genesis)
      initialNonce

    initExtLedgerState :: ExtLedgerState (ShelleyBlock c)
    initExtLedgerState = ExtLedgerState {
        ledgerState = initLedgerState
      , headerState = genesisHeaderState initChainDepState
      }

    runShelleyBase :: SL.ShelleyBase a -> a
    runShelleyBase sb = runReader sb (shelleyLedgerGlobals ledgerConfig)

    oSched :: Map SlotNo (SL.OBftSlot c)
    oSched = runShelleyBase $
      SL.overlaySchedule
        initialEpochNo
        (Map.keysSet (SL.sgGenDelegs genesis))
        (SL.sgProtocolParams genesis)

    -- Register the initial staking.
    --
    -- This function embodies a little more logic than ideal. We might want to
    -- move it into `cardano-ledger-specs.`
    registerGenesisStaking :: SL.ChainState c -> SL.ChainState c
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
          { SL.esLState = newLedgerState }
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
        newDState :: SL.DState c
        newDState = (SL._dstate oldDPState) {
          SL._delegations = Map.mapKeys SL.KeyHashObj sgsStake
        }

        -- We consider pools as having been registered in slot 0
        -- See STS POOL for details
        newPState :: SL.PState c
        newPState = (SL._pstate oldDPState) {
          SL._stPools = SL.StakePools $ Map.map (const $ SlotNo 0) $ sgsPools
        , SL._pParams = sgsPools
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

protocolClientInfoShelley :: ProtocolClientInfo (ShelleyBlock c)
protocolClientInfoShelley =
    ProtocolClientInfo {
      -- No particular codec configuration is needed for Shelley
      pClientInfoCodecConfig = ShelleyCodecConfig
    }


{-------------------------------------------------------------------------------
  ConfigSupportsNode instance
-------------------------------------------------------------------------------}

instance ConfigSupportsNode (ShelleyBlock c) where
  getSystemStart     = shelleySystemStart
  getNetworkMagic    = shelleyNetworkMagic
  getProtocolMagicId = shelleyProtocolMagicId

{-------------------------------------------------------------------------------
  RunNode instance
-------------------------------------------------------------------------------}

instance TPraosCrypto c => RunNode (ShelleyBlock c) where
  nodeBlockFetchSize = fromIntegral . SL.bsize . SL.bhbody . shelleyHeaderRaw

  -- We fix the chunk size to 10k
  nodeImmDbChunkInfo =
      simpleChunkInfo
    . EpochSize
    . (* 10)
    . maxRollbacks
    . tpraosSecurityParam
    . tpraosParams
    . configConsensus

  nodeCheckIntegrity cfg = verifyBlockIntegrity tpraosSlotsPerKESPeriod
    where
      TPraosParams { tpraosSlotsPerKESPeriod } =
        tpraosParams $ configConsensus cfg

  nodeGetBinaryBlockInfo   = shelleyBinaryBlockInfo
