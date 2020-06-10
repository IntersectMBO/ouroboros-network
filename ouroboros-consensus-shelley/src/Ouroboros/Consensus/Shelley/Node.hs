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
  , ShelleyGenesis (..)
  , initialFundsPseudoTxIn
  , ShelleyGenesisStaking (..)
  , TPraosLeaderCredentials (..)
  , SL.ProtVer
  , emptyGenesisStaking
  ) where

import           Control.Monad.Reader (runReader)
import           Crypto.Random (MonadRandom)
import           Data.Functor.Identity (Identity)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import qualified Cardano.Crypto.Hash.Class as Crypto (Hash (..))
import           Cardano.Slotting.EpochInfo
import           Cardano.Slotting.Slot (EpochNo (..), EpochSize (..),
                     SlotNo (..), WithOrigin (Origin))

import           Ouroboros.Network.Block (genesisPoint)

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

import qualified Shelley.Spec.Ledger.Address as SL
import qualified Shelley.Spec.Ledger.BaseTypes as SL
import qualified Shelley.Spec.Ledger.BlockChain as SL
import qualified Shelley.Spec.Ledger.Credential as SL
import           Shelley.Spec.Ledger.Crypto (HASH)
import qualified Shelley.Spec.Ledger.Delegation.Certificates as SL
import qualified Shelley.Spec.Ledger.EpochBoundary as SL
import qualified Shelley.Spec.Ledger.Keys as SL
import qualified Shelley.Spec.Ledger.LedgerState as SL
import qualified Shelley.Spec.Ledger.OCert as SL
import qualified Shelley.Spec.Ledger.PParams as SL
import qualified Shelley.Spec.Ledger.Scripts as SL
import qualified Shelley.Spec.Ledger.STS.Chain as SL
import qualified Shelley.Spec.Ledger.STS.NewEpoch as SL
import qualified Shelley.Spec.Ledger.STS.Prtcl as SL
import qualified Shelley.Spec.Ledger.Tx as SL
import qualified Shelley.Spec.Ledger.UTxO as SL

import           Ouroboros.Consensus.Shelley.Genesis
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

-- | Compute the 'SL.TxIn' of the initial UTxO pseudo-transaction corresponding
-- to the given address in the genesis initial funds.
--
-- The Shelley initial UTxO is constructed from the 'sgInitialFunds' which
-- is not a full UTxO but just a map from addresses to coin values.
--
-- This gets turned into a UTxO by making a pseudo-transaction for each address,
-- with the 0th output being the coin value. So to spend from the initial UTxO
-- we need this same 'SL.TxIn' to use as an input to the spending transaction.
--
initialFundsPseudoTxIn :: forall c. SL.Addr c -> SL.TxIn c
initialFundsPseudoTxIn addr =
    case addr of
      SL.Addr _networkId (SL.KeyHashObj    (SL.KeyHash    h)) _sref -> pseudoTxIn h
      SL.Addr _networkId (SL.ScriptHashObj (SL.ScriptHash h)) _sref -> pseudoTxIn h
      SL.AddrBootstrap byronAddr -> error $
        "Unsupported Byron address in the genesis UTxO: " <> show byronAddr
  where
    pseudoTxIn :: Crypto.Hash (HASH c) a -> SL.TxIn c
    pseudoTxIn h = SL.TxIn (pseudoTxId h) 0

    pseudoTxId :: Crypto.Hash (HASH c) a -> SL.TxId c
    pseudoTxId = SL.TxId . castHash

    --TODO: move this to the hash API module
    castHash :: Crypto.Hash (HASH c) a -> Crypto.Hash (HASH c) b
    castHash (Crypto.UnsafeHash h) = Crypto.UnsafeHash h

protocolInfoShelley
  :: forall m c. (MonadRandom m, TPraosCrypto c)
  => ShelleyGenesis c
  -> SL.ProtVer
  -> Maybe (TPraosLeaderCredentials c)
  -> ProtocolInfo m (ShelleyBlock c)
protocolInfoShelley genesis protVer mbCredentials =
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
    ledgerConfig = mkShelleyLedgerConfig genesis epochInfo

    -- TODO: This must instead be derived from the hard fork history.
    -- <https://github.com/input-output-hk/ouroboros-network/issues/1205>
    epochInfo :: EpochInfo Identity
    epochInfo = fixedSizeEpochInfo $ sgEpochLength genesis

    tpraosParams :: TPraosParams
    tpraosParams = TPraosParams {
        tpraosSlotsPerKESPeriod = sgSlotsPerKESPeriod genesis
      , tpraosLeaderF           = sgActiveSlotCoeff   genesis
      , tpraosSecurityParam     = sgSecurityParam     genesis
      , tpraosMaxKESEvo         = sgMaxKESEvolutions  genesis
      , tpraosQuorum            = sgUpdateQuorum      genesis
      , tpraosMaxMajorPV        = sgMaxMajorPV        genesis
      , tpraosMaxLovelaceSupply = sgMaxLovelaceSupply genesis
      , tpraosNetworkId         = sgNetworkId         genesis
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
      , shelleySystemStart     = sgSystemStart     genesis
      , shelleyNetworkMagic    = sgNetworkMagic    genesis
      , shelleyProtocolMagicId = sgProtocolMagicId genesis
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

    initShelleyState :: SL.ChainState c
    initShelleyState = registerGenesisStaking $ SL.initialShelleyState
      Origin
      initialEpochNo
      genesisUtxO
      (fromIntegral (sgMaxLovelaceSupply genesis) - SL.balance genesisUtxO)
      (sgGenDelegs genesis)
      oSched
      (sgProtocolParams genesis)
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
        (Map.keysSet (sgGenDelegs genesis))
        (sgProtocolParams genesis)

    genesisUtxO :: SL.UTxO c
    genesisUtxO = SL.UTxO $ Map.fromList
        [ (txIn, txOut)
        | (addr, amount) <- Map.toList (sgInitialFunds genesis)
        , let txIn  = initialFundsPseudoTxIn addr
              txOut = SL.TxOut addr amount
        ]

    -- Register the initial staking.
    --
    -- This function embodies a little more logic than ideal. We might want to
    -- move it into `cardano-ledger-specs.`
    registerGenesisStaking :: SL.ChainState c -> SL.ChainState c
    registerGenesisStaking cs@(SL.ChainState {chainNes = oldChainNes} ) = cs
        { SL.chainNes = newChainNes }
      where
        ShelleyGenesisStaking { sgsPools, sgsStake } = sgStaking genesis
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
              | (addr, stake) <- Map.toList (sgInitialFunds genesis)
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
