{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeApplications         #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Shelley.Node (
    protocolInfoShelley
  , ShelleyGenesis (..)
  , TPraosLeaderCredentials (..)
  , SL.ProtVer
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
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Node.State
import           Ouroboros.Consensus.Protocol.Abstract (SecurityParam (..))
import           Ouroboros.Consensus.Storage.ImmutableDB (simpleChunkInfo)

import qualified Shelley.Spec.Ledger.BaseTypes as SL
import qualified Shelley.Spec.Ledger.BlockChain as SL
import qualified Shelley.Spec.Ledger.Coin as SL
import           Shelley.Spec.Ledger.Crypto (HASH)
import qualified Shelley.Spec.Ledger.Keys as SL
import qualified Shelley.Spec.Ledger.LedgerState as SL
import qualified Shelley.Spec.Ledger.PParams as SL
import qualified Shelley.Spec.Ledger.STS.Chain as SL
import qualified Shelley.Spec.Ledger.STS.Prtcl as SL
import qualified Shelley.Spec.Ledger.TxData as SL
import qualified Shelley.Spec.Ledger.UTxO as SL

import           Ouroboros.Consensus.Shelley.Ledger
import qualified Ouroboros.Consensus.Shelley.Ledger.History as History
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

    epochInfo :: EpochInfo Identity
    epochInfo = fixedSizeEpochInfo $ sgEpochLength genesis

    tpraosParams :: TPraosParams
    tpraosParams = TPraosParams {
        tpraosEpochInfo         = epochInfo
      , tpraosSlotsPerKESPeriod = sgSlotsPerKESPeriod genesis
      , tpraosLeaderF           = sgActiveSlotsCoeff  genesis
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
    initShelleyState = SL.initialShelleyState
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
        SL._activeSlotCoeff =
            SL.mkActiveSlotCoeff
          . SL.truncateUnitInterval
          . realToFrac
          $ sgActiveSlotsCoeff genesis
      , SL._d =
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

{-------------------------------------------------------------------------------
  RunNode instance
-------------------------------------------------------------------------------}

instance HasNetworkProtocolVersion (ShelleyBlock c) where
  -- Use defaults

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
