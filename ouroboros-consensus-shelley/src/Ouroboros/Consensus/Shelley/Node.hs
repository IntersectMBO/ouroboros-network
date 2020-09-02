{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields    #-}
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
  , protocolClientInfoShelley
  , SL.ShelleyGenesis (..)
  , SL.ShelleyGenesisStaking (..)
  , TPraosLeaderCredentials (..)
  , shelleyBlockForging
  , tpraosBlockIssuerVKey
  , SL.ProtVer
  , SL.Nonce (..)
  , SL.emptyGenesisStaking
  , validateGenesis
  ) where

import           Control.Monad.Reader (runReader)
import           Data.Bifunctor (first)
import           Data.Functor.Identity (Identity)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

import qualified Cardano.Crypto.VRF as VRF
import           Cardano.Prelude (Natural)
import           Cardano.Slotting.EpochInfo

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Config.SupportsNode
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Inspect
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Storage.ImmutableDB (simpleChunkInfo)
import           Ouroboros.Consensus.Util.Assert
import           Ouroboros.Consensus.Util.IOLike

import qualified Shelley.Spec.Ledger.API as SL
import qualified Shelley.Spec.Ledger.BaseTypes as SL
import qualified Shelley.Spec.Ledger.BlockChain as SL
import qualified Shelley.Spec.Ledger.EpochBoundary as SL
import qualified Shelley.Spec.Ledger.Genesis as SL
import qualified Shelley.Spec.Ledger.Keys as SL
import qualified Shelley.Spec.Ledger.OCert as SL
import qualified Shelley.Spec.Ledger.OCert as Absolute (KESPeriod (..))
import qualified Shelley.Spec.Ledger.OverlaySchedule as SL
import qualified Shelley.Spec.Ledger.PParams as SL
import qualified Shelley.Spec.Ledger.STS.Chain as SL
import qualified Shelley.Spec.Ledger.STS.NewEpoch as SL
import qualified Shelley.Spec.Ledger.STS.Prtcl as SL
import qualified Shelley.Spec.Ledger.STS.Tickn as SL
import qualified Shelley.Spec.Ledger.UTxO as SL

import           Ouroboros.Consensus.Shelley.Ledger
import           Ouroboros.Consensus.Shelley.Ledger.NetworkProtocolVersion ()
import           Ouroboros.Consensus.Shelley.Node.Serialisation ()
import           Ouroboros.Consensus.Shelley.Protocol
import qualified Ouroboros.Consensus.Shelley.Protocol.HotKey as HotKey
import qualified Ouroboros.Consensus.Shelley.Protocol.State as State

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
  }

tpraosBlockIssuerVKey :: Maybe (TPraosLeaderCredentials era) -> BlockIssuerVKey era
tpraosBlockIssuerVKey mbCredentials =
    case tpraosCanBeLeaderColdVerKey . tpraosLeaderCredentialsCanBeLeader
           <$> mbCredentials of
      Nothing   -> NotABlockIssuer
      Just vkey -> BlockIssuerVKey vkey

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
                      } = do
    hotKey <- HotKey.mkHotKey initSignKey startPeriod tpraosMaxKESEvo
    return BlockForging {
        canBeLeader      = canBeLeader
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

protocolInfoShelley
  :: forall m era. (IOLike m, TPraosCrypto era)
  => SL.ShelleyGenesis era
  -> SL.Nonce
     -- ^ The initial nonce, typically derived from the hash of Genesis config
     -- JSON file.
  -> Natural -- ^ Max major protocol version
  -> SL.ProtVer
  -> Maybe (TPraosLeaderCredentials era)
  -> ProtocolInfo m (ShelleyBlock era)
protocolInfoShelley genesis initialNonce maxMajorPV protVer mbCredentials =
    assertWithMsg (validateGenesis genesis) $
    ProtocolInfo {
        pInfoConfig       = topLevelConfig
      , pInfoInitLedger   = initExtLedgerState
      , pInfoBlockForging = shelleyBlockForging tpraosParams <$> mbCredentials
      }
  where
    topLevelConfig :: TopLevelConfig (ShelleyBlock era)
    topLevelConfig = TopLevelConfig {
        topLevelConfigProtocol = consensusConfig
      , topLevelConfigBlock = FullBlockConfig {
            blockConfigLedger = ledgerConfig
          , blockConfigBlock  = blockConfig
          , blockConfigCodec  = ShelleyCodecConfig
          }
      }

    consensusConfig :: ConsensusConfig (BlockProtocol (ShelleyBlock era))
    consensusConfig = TPraosConfig {
        tpraosParams
      , tpraosEpochInfo = epochInfo
      }

    ledgerConfig :: LedgerConfig (ShelleyBlock era)
    ledgerConfig = mkShelleyLedgerConfig genesis epochInfo maxMajorPV

    epochInfo :: EpochInfo Identity
    epochInfo = fixedSizeEpochInfo $ SL.sgEpochLength genesis

    tpraosParams :: TPraosParams
    tpraosParams = mkTPraosParams maxMajorPV initialNonce genesis

    blockConfig :: BlockConfig (ShelleyBlock era)
    blockConfig =
        mkShelleyBlockConfig
          protVer
          genesis
          (tpraosBlockIssuerVKey mbCredentials)

    initLedgerState :: LedgerState (ShelleyBlock era)
    initLedgerState = ShelleyLedgerState {
        ledgerTip    = GenesisPoint
      , shelleyState = SL.chainNes initShelleyState
      }

    initChainDepState :: State.TPraosState era
    initChainDepState = State.empty Origin $
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

    initialUtxo :: SL.UTxO era
    initialUtxo = SL.genesisUtxO genesis

    initShelleyState :: SL.ChainState era
    initShelleyState = registerGenesisStaking $ SL.initialShelleyState
      Origin
      initialEpochNo
      initialUtxo
      (fromIntegral (SL.sgMaxLovelaceSupply genesis) - SL.balance initialUtxo)
      (SL.sgGenDelegs genesis)
      oSched
      (SL.sgProtocolParams genesis)
      initialNonce

    initExtLedgerState :: ExtLedgerState (ShelleyBlock era)
    initExtLedgerState = ExtLedgerState {
        ledgerState = initLedgerState
      , headerState = genesisHeaderState initChainDepState
      }

    runShelleyBase :: SL.ShelleyBase a -> a
    runShelleyBase sb = runReader sb (shelleyLedgerGlobals ledgerConfig)

    oSched :: SL.OverlaySchedule era
    oSched = runShelleyBase $
      SL.overlaySchedule
        initialEpochNo
        (Map.keysSet (SL.sgGenDelegs genesis))
        (SL.sgProtocolParams genesis)

    -- Register the initial staking.
    --
    -- This function embodies a little more logic than ideal. We might want to
    -- move it into `cardano-ledger-specs.`
    --
    -- HERE BE DRAGONS! This function is intended to help in testing. It should
    -- not be called with anything other than 'emptyGenesisStaking' in
    -- production.
    registerGenesisStaking :: SL.ChainState era -> SL.ChainState era
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
        newDState :: SL.DState era
        newDState = (SL._dstate oldDPState) {
          SL._rewards = Map.map (const $ SL.Coin 0)
                      . Map.mapKeys SL.KeyHashObj
                      $ sgsStake
        , SL._delegations = Map.mapKeys SL.KeyHashObj sgsStake
        }

        -- We consider pools as having been registered in slot 0
        -- See STS POOL for details
        newPState :: SL.PState era
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
  Inspection
-------------------------------------------------------------------------------}

-- TODO: This should be updated as soon as we start preparing for the
-- hard fork transition out of Shelley.
instance InspectLedger (ShelleyBlock era) where
  -- Use defaults

{-------------------------------------------------------------------------------
  ConfigSupportsNode instance
-------------------------------------------------------------------------------}

instance ConfigSupportsNode (ShelleyBlock era) where
  getSystemStart  = shelleySystemStart
  getNetworkMagic = shelleyNetworkMagic

{-------------------------------------------------------------------------------
  RunNode instance
-------------------------------------------------------------------------------}

instance TPraosCrypto era => RunNode (ShelleyBlock era) where
  nodeBlockFetchSize hdr = overhead + headerSize + bodySize
    where
      -- The maximum block size is 65536, the CBOR-in-CBOR tag for this block
      -- is:
      --
      -- > D8 18          # tag(24)
      -- >    1A 00010000 # bytes(65536)
      --
      -- Which is 7 bytes, enough for up to 4294967295 bytes.
      overhead   = 7 {- CBOR-in-CBOR -} + 1 {- encodeListLen -}
      bodySize   = fromIntegral . SL.bsize . SL.bhbody . shelleyHeaderRaw $ hdr
      headerSize = fromIntegral . SL.bHeaderSize . shelleyHeaderRaw $ hdr

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
