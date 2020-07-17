{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE DisambiguateRecordFields   #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Shelley.Ledger.Ledger (
    ShelleyLedgerError (..)
  , LedgerState (..)
  , Ticked(..)
  , QueryLedger (..)
  , Query (..)
  , NonMyopicMemberRewards (..)
    -- * Ledger config
  , ShelleyLedgerConfig (..)
  , mkShelleyLedgerConfig
  , shelleyEraParams
    -- * Auxiliary
  , getPParams
    -- * Serialisation
  , encodeShelleyAnnTip
  , decodeShelleyAnnTip
  , decodeShelleyLedgerState
  , encodeShelleyLedgerState
  , encodeShelleyQuery
  , decodeShelleyQuery
  , encodeShelleyResult
  , decodeShelleyResult
  , encodeShelleyExtLedgerState
  , encodeShelleyHeaderState
  ) where

import           Codec.CBOR.Decoding (Decoder)
import qualified Codec.CBOR.Decoding as CBOR
import           Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as CBOR
import           Codec.Serialise (Serialise, decode, encode)
import           Control.Monad.Except
import           Data.Functor.Identity
import           Data.Kind (Type)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Type.Equality ((:~:) (Refl), apply)
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)

import           Cardano.Binary (FromCBOR (..), ToCBOR (..), enforceSize)
import           Cardano.Prelude (Natural, NoUnexpectedThunks (..))
import           Cardano.Slotting.EpochInfo

import           Ouroboros.Network.Block (Serialised (..), decodePoint,
                     encodePoint, mkSerialised)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime.WallClock.Types
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Forecast
import           Ouroboros.Consensus.HardFork.Abstract
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.CommonProtocolParams
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Util (ShowProxy (..), (...:), (..:))
import           Ouroboros.Consensus.Util.Versioned

import qualified Control.State.Transition as STS
import qualified Shelley.Spec.Ledger.Address as SL
import qualified Shelley.Spec.Ledger.API as SL
import qualified Shelley.Spec.Ledger.BaseTypes as SL
import qualified Shelley.Spec.Ledger.BlockChain as SL
import qualified Shelley.Spec.Ledger.Coin as SL
import qualified Shelley.Spec.Ledger.Credential as SL
import qualified Shelley.Spec.Ledger.Delegation.Certificates as SL
import qualified Shelley.Spec.Ledger.Genesis as SL
import qualified Shelley.Spec.Ledger.Keys as SL
import qualified Shelley.Spec.Ledger.LedgerState as SL
import qualified Shelley.Spec.Ledger.PParams as SL
import qualified Shelley.Spec.Ledger.STS.Chain as STS
import qualified Shelley.Spec.Ledger.UTxO as SL

import           Ouroboros.Consensus.Shelley.Ledger.Block
import qualified Ouroboros.Consensus.Shelley.Ledger.History as History
import           Ouroboros.Consensus.Shelley.Ledger.TPraos ()
import           Ouroboros.Consensus.Shelley.Protocol

{-------------------------------------------------------------------------------
  Ledger errors
-------------------------------------------------------------------------------}

data ShelleyLedgerError c
  = TickError  !(SL.TickTransitionError  c)
  | BBodyError !(SL.BlockTransitionError c)
  deriving (Eq, Generic, Show)

instance Crypto c => NoUnexpectedThunks (ShelleyLedgerError c)

{-------------------------------------------------------------------------------
  Config
-------------------------------------------------------------------------------}

data ShelleyLedgerConfig c = ShelleyLedgerConfig {
      shelleyLedgerGenesis   :: !(SL.ShelleyGenesis c)
      -- | Derived from 'shelleyLedgerGenesis' but we store a cached version
      -- because it used very often.
    , shelleyLedgerGlobals   :: !SL.Globals
      -- | Derived from 'shelleyLedgerGenesis' but we store a cached version
      -- because it used very often.
    , shelleyLedgerEraParams :: !HardFork.EraParams
    }
  deriving (Generic, NoUnexpectedThunks)

shelleyEraParams :: SL.ShelleyGenesis c -> HardFork.EraParams
shelleyEraParams genesis = HardFork.EraParams {
      eraEpochSize  = SL.sgEpochLength genesis
    , eraSlotLength = mkSlotLength $ SL.sgSlotLength genesis
    , eraSafeZone   = HardFork.StandardSafeZone
                        stabilityWindow
                        HardFork.NoLowerBound
    }
  where
    stabilityWindow =
      computeStabilityWindow
        (SecurityParam (SL.sgSecurityParam genesis))
        (SL.sgActiveSlotCoeff genesis)

mkShelleyLedgerConfig
  :: SL.ShelleyGenesis c
  -> EpochInfo Identity
  -> Natural
  -> ShelleyLedgerConfig c
mkShelleyLedgerConfig genesis epochInfo maxMajorPV = ShelleyLedgerConfig {
      shelleyLedgerGenesis   = genesis
    , shelleyLedgerGlobals   = shelleyGlobals
    , shelleyLedgerEraParams = shelleyEraParams genesis
    }
  where
    SecurityParam k = SecurityParam $ SL.sgSecurityParam genesis

    shelleyGlobals :: SL.Globals
    shelleyGlobals = SL.Globals {
          epochInfo         = epochInfo
        , slotsPerKESPeriod = SL.sgSlotsPerKESPeriod genesis
        , stabilityWindow
        , randomnessStabilisationWindow
        , securityParameter = k
        , maxMajorPV        = maxMajorPV
        , maxKESEvo         = SL.sgMaxKESEvolutions  genesis
        , quorum            = SL.sgUpdateQuorum      genesis
        , maxLovelaceSupply = SL.sgMaxLovelaceSupply genesis
        , activeSlotCoeff   = SL.sgActiveSlotCoeff   genesis
        , networkId         = SL.sgNetworkId         genesis
        }

    stabilityWindow =
      computeStabilityWindow (SecurityParam $ SL.sgSecurityParam genesis)
                             (SL.sgActiveSlotCoeff genesis)

    randomnessStabilisationWindow =
      computeRandomnessStabilisationWindow (SecurityParam $ SL.sgSecurityParam genesis)
                                           (SL.sgActiveSlotCoeff genesis)


type instance LedgerCfg (LedgerState (ShelleyBlock c)) = ShelleyLedgerConfig c

{-------------------------------------------------------------------------------
  LedgerState
-------------------------------------------------------------------------------}

data instance LedgerState (ShelleyBlock c) = ShelleyLedgerState {
      ledgerTip    :: !(Point (ShelleyBlock c))
    , history      :: !(History.LedgerViewHistory c)
    , shelleyState :: !(SL.ShelleyState c)
    }
  deriving (Eq, Show, Generic, NoUnexpectedThunks)

instance TPraosCrypto c => UpdateLedger (ShelleyBlock c)

{-------------------------------------------------------------------------------
  GetTip
-------------------------------------------------------------------------------}

instance GetTip (LedgerState (ShelleyBlock c)) where
  getTip = castPoint . ledgerTip

instance GetTip (Ticked (LedgerState (ShelleyBlock c))) where
  getTip = castPoint . untickedLedgerTip

{-------------------------------------------------------------------------------
  Ticking
-------------------------------------------------------------------------------}

-- | Ticking only affects the state itself
data instance Ticked (LedgerState (ShelleyBlock c)) = TickedShelleyLedgerState {
      untickedLedgerTip  :: !(Point (ShelleyBlock c))
    , untickedHistory    :: !(History.LedgerViewHistory c)
    , tickedShelleyState :: !(SL.ShelleyState c)
    }
  deriving (Generic, NoUnexpectedThunks)

instance TPraosCrypto c => IsLedger (LedgerState (ShelleyBlock c)) where
  type LedgerErr (LedgerState (ShelleyBlock c)) = ShelleyLedgerError c

  applyChainTick cfg
                 slotNo
                 (ShelleyLedgerState pt history bhState) =
      TickedShelleyLedgerState {
          untickedLedgerTip  = pt
        , untickedHistory    = history
        , tickedShelleyState = SL.applyTickTransition
                                 (shelleyLedgerGlobals cfg)
                                 bhState
                                 slotNo
        }

instance TPraosCrypto c
      => ApplyBlock (LedgerState (ShelleyBlock c)) (ShelleyBlock c) where
  -- Note: in the Shelley ledger, the @CHAIN@ rule is used to apply a whole
  -- block. In consensus, we split up the application of a block to the ledger
  -- into separate steps that are performed together by 'applyExtLedgerState':
  --
  -- + 'applyChainTick': executes the @TICK@ transition
  -- + 'validateHeader':
  --    - 'validateEnvelope': executes the @chainChecks@
  --    - 'updateChainDepState': executes the @PRTCL@ transition
  -- + 'applyLedgerBlock': executes the @BBODY@ transition
  --
  applyLedgerBlock =
      applyHelper $
        -- Apply the BBODY transition using the ticked state
        withExcept BBodyError ..: SL.applyBlockTransition

  reapplyLedgerBlock = runIdentity ...:
      applyHelper $
        -- Reapply the BBODY transition using the ticked state
        Identity ..: SL.reapplyBlockTransition

applyHelper ::
     (Crypto c, Monad m)
  => (SL.Globals -> SL.ShelleyState c -> SL.Block c -> m (SL.ShelleyState c))
  -> FullBlockConfig (LedgerState (ShelleyBlock c)) (ShelleyBlock c)
  -> ShelleyBlock c
  -> Ticked (LedgerState (ShelleyBlock c))
  -> m (LedgerState (ShelleyBlock c))
applyHelper f cfg blk
            TickedShelleyLedgerState {
                tickedShelleyState = oldShelleyState
              , untickedHistory    = history
              } = do

    newShelleyState <- f globals oldShelleyState (shelleyBlockRaw blk)

    let history'
          -- TODO how expensive is this check?
          | SL.currentLedgerView oldShelleyState ==
            SL.currentLedgerView newShelleyState
          = history
          | otherwise
          = History.snapOld
              (SL.securityParameter globals)
              (blockSlot blk)
              (SL.currentLedgerView oldShelleyState)
              history

    return ShelleyLedgerState {
        ledgerTip    = blockPoint blk
      , history      = history'
      , shelleyState = newShelleyState
      }
  where
    globals = shelleyLedgerGlobals (blockConfigLedger cfg)

instance TPraosCrypto c => LedgerSupportsProtocol (ShelleyBlock c) where
  protocolLedgerView _cfg = TickedPraosLedgerView
                          . SL.currentLedgerView
                          . tickedShelleyState

  ledgerViewForecastAt cfg ledgerState at = do
      guard (at >= minLo)
      return $ Forecast at $ \for ->
        case History.find (NotOrigin for) history of
          Just lv -> return (TickedPraosLedgerView lv)
          Nothing -> do
            when (for >= maxHi) $
              throwError $ OutsideForecastRange {
                  outsideForecastAt     = at
                , outsideForecastMaxFor = maxHi
                , outsideForecastFor    = for
                }
            -- 'futureLedgerView' imposes its own bounds, but those bounds are
            -- set assuming that we are looking forward from the " current "
            -- ledger state ('shelleyState'), not from the intersection point
            -- ('at'). Those bounds could /exceed/ the 'maxHi' we have computed,
            -- but should never be /less/.
            return $ either (error "futureLedgerView failed") TickedPraosLedgerView $
                       SL.futureLedgerView globals shelleyState for
    where
      ShelleyLedgerState {history , shelleyState} = ledgerState
      globals = shelleyLedgerGlobals cfg
      swindow = SL.stabilityWindow globals
      tip     = ledgerTipSlot ledgerState

      -- Inclusive lower bound
      minLo :: WithOrigin SlotNo
      minLo = case tip of
                NotOrigin (SlotNo s) | s >= swindow -> NotOrigin (SlotNo (s - swindow))
                _otherwise                          -> Origin

      -- Exclusive upper bound
      maxHi :: SlotNo
      maxHi = case at of
                Origin      -> SlotNo swindow
                NotOrigin s -> SlotNo $ unSlotNo s + 1 + swindow

instance HasHardForkHistory (ShelleyBlock c) where
  type HardForkIndices (ShelleyBlock c) = '[ShelleyBlock c]
  hardForkSummary = neverForksHardForkSummary shelleyLedgerEraParams

{-------------------------------------------------------------------------------
  QueryLedger
-------------------------------------------------------------------------------}

newtype NonMyopicMemberRewards c = NonMyopicMemberRewards {
      unNonMyopicMemberRewards ::
        Map (Either SL.Coin (SL.Credential 'SL.Staking c))
            (Map (SL.KeyHash 'SL.StakePool c) SL.Coin)
    }
  deriving stock   (Show)
  deriving newtype (Eq)

type Delegations c = Map (SL.Credential 'SL.Staking c) (SL.KeyHash 'SL.StakePool c)

instance Crypto c => Serialise (NonMyopicMemberRewards c) where
  encode = toCBOR . unNonMyopicMemberRewards
  decode = NonMyopicMemberRewards <$> fromCBOR

data instance Query (ShelleyBlock c) :: Type -> Type where
  GetLedgerTip :: Query (ShelleyBlock c) (Point (ShelleyBlock c))
  GetEpochNo :: Query (ShelleyBlock c) EpochNo
  -- | Calculate the Non-Myopic Pool Member Rewards for a set of
  -- credentials. See 'SL.getNonMyopicMemberRewards'
  GetNonMyopicMemberRewards
    :: Set (Either SL.Coin (SL.Credential 'SL.Staking c))
    -> Query (ShelleyBlock c) (NonMyopicMemberRewards c)
  GetCurrentPParams
    :: Query (ShelleyBlock c) SL.PParams
  GetProposedPParamsUpdates
    :: Query (ShelleyBlock c) (SL.ProposedPPUpdates c)
  GetStakeDistribution
    :: Query (ShelleyBlock c) (SL.PoolDistr c)
  GetFilteredUTxO
    :: Set (SL.Addr c)
    -> Query (ShelleyBlock c) (SL.UTxO c)
  GetUTxO
    :: Query (ShelleyBlock c) (SL.UTxO c)

  -- | Only for debugging purposes, we don't guarantee binary compatibility.
  -- Moreover, it is huge.
  GetCurrentEpochState
    :: Query (ShelleyBlock c) (SL.EpochState c)

  -- | Wrap the result of the query using CBOR-in-CBOR.
  --
  -- For example, when a client is running a different version than the
  -- server and it sends a 'GetCurrentEpochState' query, the client's
  -- decoder might fail to deserialise the epoch state as it might have
  -- changed between the two different versions. The client will then
  -- disconnect.
  --
  -- By using CBOR-in-CBOR, the client always successfully decodes the outer
  -- CBOR layer (so no disconnect) and can then manually try to decode the
  -- inner result. When the client's decoder is able to decode the inner
  -- result, it has access to the deserialised epoch state. When it fails to
  -- decode it, the client can fall back to pretty printing the actual CBOR,
  -- which is better than no output at all.
  GetCBOR
    :: Query (ShelleyBlock c) result
    -> Query (ShelleyBlock c) (Serialised result)

  GetFilteredDelegationsAndRewardAccounts
    :: Set (SL.Credential 'SL.Staking c)
    -> Query (ShelleyBlock c) (Delegations c, SL.RewardAccounts c)

instance Typeable c => ShowProxy (Query (ShelleyBlock c)) where

instance TPraosCrypto c => QueryLedger (ShelleyBlock c) where
  answerQuery cfg query st = case query of
      GetLedgerTip -> ledgerTip st
      GetEpochNo -> SL.nesEL $ shelleyState st
      GetNonMyopicMemberRewards creds -> NonMyopicMemberRewards $
          SL.getNonMyopicMemberRewards globals (shelleyState st) creds
      GetCurrentPParams -> getPParams $ shelleyState st
      GetProposedPParamsUpdates -> getProposedPPUpdates $ shelleyState st
      GetStakeDistribution -> SL.nesPd $ shelleyState st
      GetFilteredUTxO addrs -> SL.getFilteredUTxO (shelleyState st) addrs
      GetUTxO -> SL.getUTxO $ shelleyState st
      GetCurrentEpochState -> getCurrentEpochState $ shelleyState st
      GetCBOR query' -> mkSerialised (encodeShelleyResult query') $
          answerQuery cfg query' st
      GetFilteredDelegationsAndRewardAccounts creds ->
        getFilteredDelegationsAndRewardAccounts
          (shelleyLedgerGlobals cfg)
          (shelleyState st)
          creds
    where
      globals = shelleyLedgerGlobals cfg

instance SameDepIndex (Query (ShelleyBlock c)) where
  sameDepIndex GetLedgerTip GetLedgerTip
    = Just Refl
  sameDepIndex GetLedgerTip _
    = Nothing
  sameDepIndex GetEpochNo GetEpochNo
    = Just Refl
  sameDepIndex GetEpochNo _
    = Nothing
  sameDepIndex (GetNonMyopicMemberRewards creds) (GetNonMyopicMemberRewards creds')
    | creds == creds'
    = Just Refl
    | otherwise
    = Nothing
  sameDepIndex (GetNonMyopicMemberRewards _) _
    = Nothing
  sameDepIndex GetCurrentPParams GetCurrentPParams
    = Just Refl
  sameDepIndex GetCurrentPParams _
    = Nothing
  sameDepIndex GetProposedPParamsUpdates GetProposedPParamsUpdates
    = Just Refl
  sameDepIndex GetProposedPParamsUpdates _
    = Nothing
  sameDepIndex GetStakeDistribution GetStakeDistribution
    = Just Refl
  sameDepIndex GetStakeDistribution _
    = Nothing
  sameDepIndex (GetFilteredUTxO addrs) (GetFilteredUTxO addrs')
    | addrs == addrs'
    = Just Refl
    | otherwise
    = Nothing
  sameDepIndex (GetFilteredUTxO _) _
    = Nothing
  sameDepIndex GetUTxO GetUTxO
    = Just Refl
  sameDepIndex GetUTxO _
    = Nothing
  sameDepIndex GetCurrentEpochState GetCurrentEpochState
    = Just Refl
  sameDepIndex GetCurrentEpochState _
    = Nothing
  sameDepIndex (GetCBOR q) (GetCBOR q')
    = apply Refl <$> sameDepIndex q q'
  sameDepIndex (GetCBOR _) _
    = Nothing
  sameDepIndex (GetFilteredDelegationsAndRewardAccounts creds)
          (GetFilteredDelegationsAndRewardAccounts creds')
    | creds == creds'
    = Just Refl
    | otherwise
    = Nothing
  sameDepIndex (GetFilteredDelegationsAndRewardAccounts _) _
    = Nothing

deriving instance Eq   (Query (ShelleyBlock c) result)
deriving instance Show (Query (ShelleyBlock c) result)

instance Crypto c => ShowQuery (Query (ShelleyBlock c)) where
  showResult GetLedgerTip                                 = show
  showResult GetEpochNo                                   = show
  showResult (GetNonMyopicMemberRewards {})               = show
  showResult GetCurrentPParams                            = show
  showResult GetProposedPParamsUpdates                    = show
  showResult GetStakeDistribution                         = show
  showResult (GetFilteredUTxO {})                         = show
  showResult GetUTxO                                      = show
  showResult GetCurrentEpochState                         = show
  showResult (GetCBOR {})                                 = show
  showResult (GetFilteredDelegationsAndRewardAccounts {}) = show

instance TPraosCrypto c => CommonProtocolParams (ShelleyBlock c) where
  maxHeaderSize = fromIntegral . SL._maxBHSize . getPParams . shelleyState
  maxTxSize     = fromIntegral . SL._maxTxSize . getPParams . shelleyState

{-------------------------------------------------------------------------------
  ValidateEnvelope
-------------------------------------------------------------------------------}

instance Crypto c => BasicEnvelopeValidation (ShelleyBlock c) where
  -- defaults all OK

instance Crypto c => ValidateEnvelope (ShelleyBlock c) where
  type OtherHeaderEnvelopeError (ShelleyBlock c) =
    STS.PredicateFailure (STS.CHAIN c)

  additionalEnvelopeChecks cfg (TickedPraosLedgerView ledgerView) hdr =
      SL.chainChecks globals pparams (shelleyHeaderRaw hdr)
    where
      pparams = SL.lvProtParams ledgerView
      globals = shelleyLedgerGlobals (configLedger cfg)

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

getPParams :: SL.ShelleyState c -> SL.PParams
getPParams = SL.esPp . SL.nesEs

getProposedPPUpdates :: SL.ShelleyState c -> SL.ProposedPPUpdates c
getProposedPPUpdates = SL.proposals . SL._ppups
                     . SL._utxoState . SL.esLState . SL.nesEs

-- Get the current EpochState. This is mainly for debugging.
getCurrentEpochState :: SL.ShelleyState c -> SL.EpochState c
getCurrentEpochState = SL.nesEs

getDState :: SL.ShelleyState c -> SL.DState c
getDState = SL._dstate . SL._delegationState . SL.esLState . SL.nesEs

getFilteredDelegationsAndRewardAccounts :: SL.Globals
                                        -> SL.ShelleyState c
                                        -> Set (SL.Credential 'SL.Staking c)
                                        -> (Delegations c, SL.RewardAccounts c)
getFilteredDelegationsAndRewardAccounts globals ss creds =
    (filteredDelegations, filteredRwdAcnts)
  where
    network = SL.networkId globals
    rwdAcnts = Set.map (SL.RewardAcnt network) creds
    dstate = getDState ss
    filteredDelegations = Map.restrictKeys (SL._delegations dstate) creds
    filteredRwdAcnts = Map.restrictKeys (SL._rewards dstate) rwdAcnts

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

serialisationFormatVersion0 :: VersionNumber
serialisationFormatVersion0 = 0

encodeShelleyAnnTip :: Crypto c => AnnTip (ShelleyBlock c) -> Encoding
encodeShelleyAnnTip = defaultEncodeAnnTip toCBOR

decodeShelleyAnnTip :: Crypto c => Decoder s (AnnTip (ShelleyBlock c))
decodeShelleyAnnTip = defaultDecodeAnnTip fromCBOR

encodeShelleyExtLedgerState :: Crypto c
                            => ExtLedgerState (ShelleyBlock c)
                            -> Encoding
encodeShelleyExtLedgerState = encodeExtLedgerState
    encodeShelleyLedgerState
    toCBOR
    encodeShelleyAnnTip

encodeShelleyHeaderState :: Crypto c
                         => HeaderState (ShelleyBlock c)
                         -> Encoding
encodeShelleyHeaderState = encodeHeaderState
    toCBOR
    encodeShelleyAnnTip

encodeShelleyLedgerState :: Crypto c => LedgerState (ShelleyBlock c) -> Encoding
encodeShelleyLedgerState
    ShelleyLedgerState { ledgerTip, history, shelleyState } =
    encodeVersion serialisationFormatVersion0 $ mconcat
      [ CBOR.encodeListLen 3
      , encode ledgerTip
      , History.encodeLedgerViewHistory history
      , toCBOR shelleyState
      ]

decodeShelleyLedgerState :: Crypto c => Decoder r (LedgerState (ShelleyBlock c))
decodeShelleyLedgerState = decodeVersion
    [(serialisationFormatVersion0, Decode decodeShelleyLedgerState0)]
  where
    decodeShelleyLedgerState0 = do
      enforceSize "LedgerState ShelleyBlock" 3
      ShelleyLedgerState
        <$> decode
        <*> History.decodeLedgerViewHistory
        <*> fromCBOR

encodeShelleyQuery :: Crypto c => Query (ShelleyBlock c) result -> Encoding
encodeShelleyQuery query = case query of
    GetLedgerTip ->
      CBOR.encodeListLen 1 <> CBOR.encodeWord8 0
    GetEpochNo ->
      CBOR.encodeListLen 1 <> CBOR.encodeWord8 1
    GetNonMyopicMemberRewards creds ->
      CBOR.encodeListLen 2 <> CBOR.encodeWord8 2 <> toCBOR creds
    GetCurrentPParams ->
      CBOR.encodeListLen 1 <> CBOR.encodeWord8 3
    GetProposedPParamsUpdates ->
      CBOR.encodeListLen 1 <> CBOR.encodeWord8 4
    GetStakeDistribution ->
      CBOR.encodeListLen 1 <> CBOR.encodeWord8 5
    GetFilteredUTxO addrs ->
      CBOR.encodeListLen 2 <> CBOR.encodeWord8 6 <> toCBOR addrs
    GetUTxO ->
      CBOR.encodeListLen 1 <> CBOR.encodeWord8 7
    GetCurrentEpochState ->
      CBOR.encodeListLen 1 <> CBOR.encodeWord8 8
    GetCBOR query' ->
      CBOR.encodeListLen 2 <> CBOR.encodeWord8 9 <> encodeShelleyQuery query'
    GetFilteredDelegationsAndRewardAccounts creds ->
      CBOR.encodeListLen 2 <> CBOR.encodeWord8 10 <> toCBOR creds

decodeShelleyQuery :: Crypto c => Decoder s (SomeBlock Query (ShelleyBlock c))
decodeShelleyQuery = do
    len <- CBOR.decodeListLen
    tag <- CBOR.decodeWord8
    case (len, tag) of
      (1, 0)  -> return $ SomeBlock GetLedgerTip
      (1, 1)  -> return $ SomeBlock GetEpochNo
      (2, 2)  -> SomeBlock . GetNonMyopicMemberRewards <$> fromCBOR
      (1, 3)  -> return $ SomeBlock GetCurrentPParams
      (1, 4)  -> return $ SomeBlock GetProposedPParamsUpdates
      (1, 5)  -> return $ SomeBlock GetStakeDistribution
      (2, 6)  -> SomeBlock . GetFilteredUTxO <$> fromCBOR
      (1, 7)  -> return $ SomeBlock GetUTxO
      (1, 8)  -> return $ SomeBlock GetCurrentEpochState
      (2, 9)  -> (\(SomeBlock q) -> SomeBlock (GetCBOR q)) <$> decodeShelleyQuery
      (2, 10) -> SomeBlock . GetFilteredDelegationsAndRewardAccounts <$> fromCBOR
      _       -> fail $
        "decodeShelleyQuery: invalid (len, tag): (" <>
        show len <> ", " <> show tag <> ")"

encodeShelleyResult
  :: Crypto c
  => Query (ShelleyBlock c) result -> result -> Encoding
encodeShelleyResult query = case query of
    GetLedgerTip                               -> encodePoint encode
    GetEpochNo                                 -> encode
    GetNonMyopicMemberRewards {}               -> encode
    GetCurrentPParams                          -> toCBOR
    GetProposedPParamsUpdates                  -> toCBOR
    GetStakeDistribution                       -> toCBOR
    GetFilteredUTxO {}                         -> toCBOR
    GetUTxO                                    -> toCBOR
    GetCurrentEpochState                       -> toCBOR
    GetCBOR {}                                 -> encode
    GetFilteredDelegationsAndRewardAccounts {} -> toCBOR

decodeShelleyResult
  :: Crypto c
  => Query (ShelleyBlock c) result
  -> forall s. Decoder s result
decodeShelleyResult query = case query of
    GetLedgerTip                               -> decodePoint decode
    GetEpochNo                                 -> decode
    GetNonMyopicMemberRewards {}               -> decode
    GetCurrentPParams                          -> fromCBOR
    GetProposedPParamsUpdates                  -> fromCBOR
    GetStakeDistribution                       -> fromCBOR
    GetFilteredUTxO {}                         -> fromCBOR
    GetUTxO                                    -> fromCBOR
    GetCurrentEpochState                       -> fromCBOR
    GetCBOR {}                                 -> decode
    GetFilteredDelegationsAndRewardAccounts {} -> fromCBOR
