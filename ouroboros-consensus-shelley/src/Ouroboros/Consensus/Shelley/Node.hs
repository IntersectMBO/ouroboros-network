{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeFamilies             #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Shelley.Node (
    protocolInfoShelley
  , protocolClientInfoShelley
  , SL.ShelleyGenesis (..)
  , SL.ShelleyGenesisStaking (..)
  , TPraosLeaderCredentials (..)
  , SL.ProtVer
  , SL.emptyGenesisStaking
  ) where

import           Control.Monad.Reader (runReader)
import           Crypto.Random (MonadRandom)
import           Data.Functor.Identity (Identity)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           Cardano.Prelude (Natural)

import           Cardano.Slotting.EpochInfo
import           Cardano.Slotting.Slot (EpochNo (..), EpochSize (..),
                     SlotNo (..), WithOrigin (Origin))

import           Ouroboros.Network.Block (genesisPoint)
import           Ouroboros.Network.Magic

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime.WallClock.Types
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Config.SecurityParam
import           Ouroboros.Consensus.Config.SupportsNode
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Storage.ImmutableDB (simpleChunkInfo)

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
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (HotKey)
import qualified Ouroboros.Consensus.Shelley.Protocol.State as State

{-------------------------------------------------------------------------------
  ProtocolInfo
-------------------------------------------------------------------------------}

data TPraosLeaderCredentials c = TPraosLeaderCredentials {
    -- | Signing KES key. Note that this is not inside 'TPraosIsCoreNode' since
    --   it gets evolved automatically, whereas 'TPraosIsCoreNode' does not
    --   change.
    tpraosLeaderCredentialsSignKey    :: HotKey c
  , tpraosLeaderCredentialsIsCoreNode :: TPraosIsCoreNode c
  }

protocolInfoShelley
  :: forall m c. (MonadRandom m, TPraosCrypto c)
  => SL.ShelleyGenesis c
  -> Natural -- ^ Max major protocol version
  -> SL.ProtVer
  -> Maybe (TPraosLeaderCredentials c)
  -> ProtocolInfo m (ShelleyBlock c)
protocolInfoShelley genesis maxMajorPV protVer mbCredentials =
    ProtocolInfo {
        pInfoConfig      = topLevelConfig
      , pInfoInitLedger  = initExtLedgerState
      , pInfoLeaderCreds = mkLeaderCreds <$> mbCredentials
      }
  where
    topLevelConfig :: TopLevelConfig (ShelleyBlock c)
    topLevelConfig = TopLevelConfig {
        configConsensus = consensusConfig
      , configLedger    = ledgerConfig
      , configBlock     = blockConfig
      }

    consensusConfig :: ConsensusConfig (BlockProtocol (ShelleyBlock c))
    consensusConfig = TPraosConfig {
        tpraosParams
      , tpraosEpochInfo = epochInfo
      }

    ledgerConfig :: LedgerConfig (ShelleyBlock c)
    ledgerConfig = mkShelleyLedgerConfig genesis epochInfo maxMajorPV

    -- TODO: This must instead be derived from the hard fork history.
    -- <https://github.com/input-output-hk/ouroboros-network/issues/1205>
    epochInfo :: EpochInfo Identity
    epochInfo = fixedSizeEpochInfo $ SL.sgEpochLength genesis

    securityParam :: SecurityParam
    securityParam = SecurityParam $ SL.sgSecurityParam genesis

    tpraosParams :: TPraosParams
    tpraosParams = TPraosParams {
        tpraosSlotsPerKESPeriod = SL.sgSlotsPerKESPeriod genesis
      , tpraosLeaderF           = SL.sgActiveSlotCoeff   genesis
      , tpraosSecurityParam     = securityParam
      , tpraosMaxKESEvo         = SL.sgMaxKESEvolutions  genesis
      , tpraosQuorum            = SL.sgUpdateQuorum      genesis
      , tpraosMaxMajorPV        = maxMajorPV
      , tpraosMaxLovelaceSupply = SL.sgMaxLovelaceSupply genesis
      , tpraosNetworkId         = SL.sgNetworkId         genesis
      }

    mkLeaderCreds :: TPraosLeaderCredentials c
                  -> (TPraosIsCoreNode c, MaintainForgeState m (ShelleyBlock c))
    mkLeaderCreds (TPraosLeaderCredentials key isACoreNode) = (
          isACoreNode
        , MaintainForgeState {
              initForgeState   = TPraosForgeState key
            , updateForgeState = evolveKey isACoreNode
            }
        )

    evolveKey :: TPraosIsCoreNode c
              -> Update m (TPraosForgeState c) -> SlotNo -> m ()
    evolveKey TPraosIsCoreNode{..} upd curSlot =
        evolveKESKeyIfNecessary upd (SL.KESPeriod kesEvolution)
      where
        TPraosParams{..} = tpraosParams

        kesPeriodNat = fromIntegral $ unSlotNo curSlot `div` tpraosSlotsPerKESPeriod
        SL.OCert _ _ (SL.KESPeriod c0) _ = tpraosIsCoreNodeOpCert
        kesEvolution = if kesPeriodNat >= c0 then kesPeriodNat - c0 else 0

    blockConfig :: BlockConfig (ShelleyBlock c)
    blockConfig = ShelleyConfig {
        shelleyProtocolVersion = protVer
      , shelleySystemStart     = SystemStart $ SL.sgSystemStart genesis
      , shelleyNetworkMagic    = NetworkMagic $ SL.sgNetworkMagic  genesis
      , shelleyProtocolMagicId = SL.sgProtocolMagicId genesis
      }

    initLedgerState :: LedgerState (ShelleyBlock c)
    initLedgerState = ShelleyLedgerState {
        ledgerTip    = genesisPoint
      , history      = History.empty
      , shelleyState = SL.chainNes initShelleyState
      }

    initConsensusState :: State.TPraosState c
    initConsensusState = State.empty Origin $
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
      -- We can start without entropy, throughout the epoch(s) we'll obtain
      -- entropy.
      SL.NeutralNonce

    initExtLedgerState :: ExtLedgerState (ShelleyBlock c)
    initExtLedgerState = ExtLedgerState {
        ledgerState = initLedgerState
      , headerState = genesisHeaderState initConsensusState
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
