{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeApplications         #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Shelley.Node (
    protocolInfoShelley
  , ShelleyGenesis (..)
  , ShelleyGenesisStaking (..)
  , TPraosLeaderCredentials (..)
  , SL.ProtVer
  , emptyGenesisStaking
  ) where

import           Codec.Serialise (decode, encode)
import           Control.Monad.Reader (runReader)
import           Data.ByteString (ByteString)
import           Data.Coerce (coerce)
import           Data.Functor.Identity (Identity)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Proxy (Proxy (..))
import           Data.Word (Word64)
import           GHC.Generics (Generic)

import           Cardano.Binary (Annotator (..), FullByteString (..), fromCBOR,
                     toCBOR)
import           Cardano.Crypto (ProtocolMagicId)
import           Cardano.Crypto.KES.Class (SignKeyKES)
import           Cardano.Prelude (Natural)
import           Cardano.Slotting.EpochInfo
import           Cardano.Slotting.Slot (EpochNo (..), EpochSize (..),
                     SlotNo (..), WithOrigin (Origin))

import           Ouroboros.Network.Block (genesisPoint)
import           Ouroboros.Network.Magic (NetworkMagic)

import           Ouroboros.Consensus.Block (BlockProtocol)
import           Ouroboros.Consensus.BlockchainTime (SlotLength, SystemStart)
import           Ouroboros.Consensus.Config
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Node.State
import           Ouroboros.Consensus.Protocol.Abstract (SecurityParam (..))
import           Ouroboros.Consensus.Storage.ImmutableDB (simpleChunkInfo)

import qualified Shelley.Spec.Ledger.BaseTypes as SL
import qualified Shelley.Spec.Ledger.BlockChain as SL
import qualified Shelley.Spec.Ledger.Coin as SL
import           Shelley.Spec.Ledger.Crypto (HASH)
import qualified Shelley.Spec.Ledger.Delegation.Certificates as SL
import qualified Shelley.Spec.Ledger.EpochBoundary as SL
import qualified Shelley.Spec.Ledger.Keys as SL
import qualified Shelley.Spec.Ledger.LedgerState as SL
import qualified Shelley.Spec.Ledger.PParams as SL
import qualified Shelley.Spec.Ledger.STS.Chain as SL
import qualified Shelley.Spec.Ledger.STS.NewEpoch as SL
import qualified Shelley.Spec.Ledger.STS.Prtcl as SL
import qualified Shelley.Spec.Ledger.TxData as SL
import qualified Shelley.Spec.Ledger.UTxO as SL

import           Ouroboros.Consensus.Shelley.Ledger
import qualified Ouroboros.Consensus.Shelley.Ledger.History as History
import           Ouroboros.Consensus.Shelley.Ledger.NetworkProtocolVersion ()
import           Ouroboros.Consensus.Shelley.Protocol
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (KES)
import qualified Ouroboros.Consensus.Shelley.Protocol.State as State

{-------------------------------------------------------------------------------
  ProtocolInfo
-------------------------------------------------------------------------------}

data TPraosLeaderCredentials c = TPraosLeaderCredentials {
    -- | Signing KES key. Note that this is not inside 'TPraosIsCoreNode' since
    --   it gets evolved automatically, whereas 'TPraosIsCoreNode' does not
    --   change.
    tpraosLeaderCredentialsSignKey    :: SignKeyKES (KES c)
  , tpraosLeaderCredentialsIsCoreNode :: TPraosIsCoreNode c
  }

-- | Genesis Shelley staking configuration.
--
-- This allows us to configure some initial stake pools and delegation to them,
-- in order to test Praos in a static configuration, without requiring on-chain
-- registration and delegation.
--
-- For simplicity, pools defined in the genesis staking do not pay deposits for
-- their registration.
data ShelleyGenesisStaking c = ShelleyGenesisStaking {
    -- | Pools to register
    --
    --   The key in this map is the hash of the public key of the _pool_. This
    --   need not correspond to any payment or staking key, but must correspond
    --   to the cold key held by 'TPraosIsCoreNode'.
    sgsPools :: !(Map (SL.KeyHash c) (SL.PoolParams c))
    -- | Stake-holding key hash credentials and the pools to delegate that stake
    -- to. We require the raw staking key hash in order to:
    --
    -- - Avoid pointer addresses, which would be tricky when there's no slot or
    --   transaction to point to.
    -- - Avoid script credentials.
  , sgsStake :: !(Map (SL.KeyHash c) (SL.KeyHash c))
  } deriving (Eq, Show, Generic)

-- | Empty genesis staking
emptyGenesisStaking :: ShelleyGenesisStaking c
emptyGenesisStaking = ShelleyGenesisStaking
  { sgsPools = Map.empty
  , sgsStake = Map.empty
  }

-- | Shelley genesis information
--
-- Note that this is needed only for a pure Shelley network, hence it being
-- defined here rather than in its own module. In mainnet, Shelley will
-- transition naturally from Byron, and thus will never have its own genesis
-- information.
data ShelleyGenesis c = ShelleyGenesis {
      sgStartTime             :: !SystemStart
    , sgNetworkMagic          :: !NetworkMagic
    , sgProtocolMagicId       :: !ProtocolMagicId
    , sgActiveSlotsCoeff      :: !Double
    , sgDecentralisationParam :: !Double
    , sgSecurityParam         :: !SecurityParam
    , sgEpochLength           :: !EpochSize
    , sgSlotsPerKESPeriod     :: !Word64
    , sgMaxKESEvolutions      :: !Word64
    , sgSlotLength            :: !SlotLength
    , sgUpdateQuorum          :: !Word64
    , sgMaxMajorPV            :: !Natural
    , sgMaxLovelaceSupply     :: !Word64
    , sgMaxBodySize           :: !Natural
    , sgMaxHeaderSize         :: !Natural
    , sgGenDelegs             :: !(Map (SL.GenKeyHash c) (SL.KeyHash c))
    , sgInitialFunds          :: !(Map (SL.Addr c) SL.Coin)
    , sgStaking               :: !(ShelleyGenesisStaking c)
    }
  deriving (Eq, Show, Generic)

protocolInfoShelley
  :: forall c. Crypto c
  => ShelleyGenesis c
  -> SL.ProtVer
  -> Maybe (TPraosLeaderCredentials c)
  -> ProtocolInfo (ShelleyBlock c)
protocolInfoShelley genesis protVer mbCredentials =
    ProtocolInfo {
        pInfoConfig     = topLevelConfig
      , pInfoInitState  = initNodeState
      , pInfoInitLedger = initExtLedgerState
      }
  where
    topLevelConfig = TopLevelConfig {
        configConsensus = consensusConfig
      , configLedger    = ledgerConfig
      , configBlock     = blockConfig
      }

    consensusConfig :: ConsensusConfig (BlockProtocol (ShelleyBlock c))
    consensusConfig = TPraosConfig {
        tpraosParams
      , tpraosIsCoreNodeOrNot
      }

    -- TODO: This must instead be derived from the hard fork history.
    -- <https://github.com/input-output-hk/ouroboros-network/issues/1205>
    epochInfo :: EpochInfo Identity
    epochInfo = fixedSizeEpochInfo $ sgEpochLength genesis

    tpraosParams :: TPraosParams
    tpraosParams = TPraosParams {
        tpraosEpochInfo         = epochInfo
      , tpraosSlotsPerKESPeriod = sgSlotsPerKESPeriod genesis
      , tpraosLeaderF           = SL.mkActiveSlotCoeff
                                . SL.truncateUnitInterval
                                . toRational
                                $ sgActiveSlotsCoeff  genesis
      , tpraosSecurityParam     = sgSecurityParam     genesis
      , tpraosMaxKESEvo         = sgMaxKESEvolutions  genesis
      , tpraosQuorum            = sgUpdateQuorum      genesis
      , tpraosMaxMajorPV        = sgMaxMajorPV        genesis
      , tpraosMaxLovelaceSupply = sgMaxLovelaceSupply genesis
      }

    initNodeState :: NodeState (ShelleyBlock c)
    tpraosIsCoreNodeOrNot :: TPraosIsCoreNodeOrNot c
    (initNodeState, tpraosIsCoreNodeOrNot) = case mbCredentials of
      Nothing -> (TPraosNoKey, TPraosIsNotACoreNode)
      Just (TPraosLeaderCredentials key isACoreNode) ->
        (TPraosKeyAvailable key, TPraosIsACoreNode isACoreNode)

    shelleyGlobals :: SL.Globals
    shelleyGlobals = mkShelleyGlobals tpraosParams

    ledgerConfig :: LedgerConfig (ShelleyBlock c)
    ledgerConfig = shelleyGlobals

    SecurityParam k = sgSecurityParam genesis

    eraParams :: HardFork.EraParams
    eraParams = HardFork.EraParams {
        eraEpochSize  = sgEpochLength genesis
      , eraSlotLength = sgSlotLength  genesis
      , eraSafeZone   = HardFork.SafeZone (k * 2) HardFork.NoLowerBound
      }

    blockConfig :: BlockConfig (ShelleyBlock c)
    blockConfig = ShelleyConfig {
        shelleyProtocolVersion = protVer
      , shelleyStartTime       = sgStartTime       genesis
      , shelleyNetworkMagic    = sgNetworkMagic    genesis
      , shelleyProtocolMagicId = sgProtocolMagicId genesis
      , shelleyEraParams       = eraParams
      }

    initLedgerState :: LedgerState (ShelleyBlock c)
    initLedgerState = ShelleyLedgerState {
        ledgerTip    = genesisPoint
      , history      = History.empty
      , shelleyState = SL.chainNes initShelleyState
      }

    initConsensusState :: State.TPraosState c
    initConsensusState = State.empty $
      SL.PrtclState
         (SL.chainOCertIssue     initShelleyState)
         Origin
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
      pparams
      -- TODO initial nonce, typically created from the hash of
      -- the last Byron block using 'SL.hashHeaderToNonce'.
      (SL.mkNonce 0)

    initExtLedgerState :: ExtLedgerState (ShelleyBlock c)
    initExtLedgerState = ExtLedgerState {
        ledgerState = initLedgerState
      , headerState = genesisHeaderState initConsensusState
      }

    runShelleyBase :: SL.ShelleyBase a -> a
    runShelleyBase sb = runReader sb shelleyGlobals

    oSched :: Map SlotNo (SL.OBftSlot c)
    oSched = runShelleyBase $
      SL.overlaySchedule
        initialEpochNo
        (Map.keysSet (sgGenDelegs genesis))
        pparams

    pparams :: SL.PParams
    pparams = SL.emptyPParams {
        SL._d =
            SL.truncateUnitInterval
          . realToFrac
          $ sgDecentralisationParam genesis
      , SL._maxBBSize = sgMaxBodySize genesis
      , SL._maxBHSize = sgMaxHeaderSize genesis
      }

    genesisUtxO :: SL.UTxO c
    genesisUtxO = SL.UTxO $ Map.fromList
        [ (magicTxIn, txOut)
        | (addr, amount) <- Map.toList (sgInitialFunds genesis)
        , let txOut = SL.TxOut addr amount
        ]
      where
        -- TODO
        magicTxInId =
            SL.TxId
          . coerce
          $ SL.hash
            @(HASH c)
            @ByteString
            "In the beginning"

        magicTxIn = SL.TxIn magicTxInId 0

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
          { SL._stake = SL.Stake . Map.mapKeys addrKeyHash $ sgInitialFunds genesis
          , SL._delegations = Map.mapKeys SL.KeyHashObj sgsStake
          , SL._poolParams = sgsPools
          }
          where
            addrKeyHash (SL.AddrBootstrap kh) = SL.KeyHashObj kh
            addrKeyHash (SL.Addr _ sr) = case sr of
              SL.StakeRefBase kh -> kh
              _ -> error "Pointer stake addresses not allowed in initial snapshot"


{-------------------------------------------------------------------------------
  RunNode instance
-------------------------------------------------------------------------------}

instance TPraosCrypto c => RunNode (ShelleyBlock c) where
  nodeForgeBlock = forgeShelleyBlock

  nodeBlockMatchesHeader = verifyBlockMatchesHeader

  nodeBlockFetchSize = fromIntegral . SL.bsize . SL.bhbody . shelleyHeaderRaw

  nodeIsEBB = const Nothing

  -- We fix the chunk size to 10k
  nodeImmDbChunkInfo _proxy =
      simpleChunkInfo
    . EpochSize
    . (* 10)
    . maxRollbacks
    . tpraosSecurityParam
    . tpraosParams
    . configConsensus

  nodeStartTime       = const $ shelleyStartTime       . configBlock
  nodeNetworkMagic    = const $ shelleyNetworkMagic    . configBlock
  nodeProtocolMagicId = const $ shelleyProtocolMagicId . configBlock

  nodeHashInfo = const shelleyHashInfo

  nodeMaxBlockSize (ShelleyLedgerState _ _ shelleyState) =
    fromIntegral $ maxBlockHeaderSize + maxBlockBodySize
    where
     SL.PParams {
         _maxBBSize = maxBlockBodySize
       , _maxBHSize = maxBlockHeaderSize
       } = getPParams shelleyState

  -- TODO
  nodeBlockEncodingOverhead = const 1 -- Single list tag.
  -- Check this isn't altered by the TxWits stuff

  nodeCheckIntegrity cfg = verifyBlockIntegrity tpraosSlotsPerKESPeriod
    where
      TPraosParams { tpraosSlotsPerKESPeriod } =
        tpraosParams $ configConsensus cfg

  nodeAddHeaderEnvelope _ _isEBB _blockSize = shelleyAddHeaderEnvelope

  nodeEncodeBlockWithInfo  = \_cfg -> encodeShelleyBlockWithInfo
  nodeEncodeHeader         = \_cfg _version -> toCBOR
  nodeEncodeWrappedHeader  = \_cfg _version -> encode
  nodeEncodeGenTx          = toCBOR
  nodeEncodeGenTxId        = toCBOR
  nodeEncodeHeaderHash     = \Proxy -> toCBOR
  nodeEncodeLedgerState    = encodeShelleyLedgerState
  nodeEncodeConsensusState = \Proxy _cfg -> toCBOR
  nodeEncodeApplyTxError   = \Proxy -> toCBOR
  nodeEncodeTipInfo        = \Proxy -> toCBOR
  nodeEncodeQuery          = encodeShelleyQuery
  nodeEncodeResult         = encodeShelleyResult

  nodeDecodeHeader         = \_cfg _version -> (. Full) . runAnnotator <$> fromCBOR
  nodeDecodeWrappedHeader  = \_cfg _version -> decode
  nodeDecodeBlock          = \_cfg -> (. Full) . runAnnotator <$> fromCBOR
  nodeDecodeGenTx          = fromCBOR
  nodeDecodeGenTxId        = fromCBOR
  nodeDecodeHeaderHash     = \Proxy -> fromCBOR
  nodeDecodeLedgerState    = decodeShelleyLedgerState
  nodeDecodeConsensusState = \Proxy _cfg -> fromCBOR
  nodeDecodeApplyTxError   = \Proxy -> fromCBOR
  nodeDecodeTipInfo        = \Proxy -> fromCBOR
  nodeDecodeQuery          = decodeShelleyQuery
  nodeDecodeResult         = decodeShelleyResult
