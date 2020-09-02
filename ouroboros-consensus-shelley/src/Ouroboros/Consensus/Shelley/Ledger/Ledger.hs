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
{-# LANGUAGE MultiWayIf                 #-}
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
import           Data.Sequence.Strict (StrictSeq)
import           Data.Set (Set)
import           Data.Type.Equality (apply)
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)

import           Cardano.Binary (FromCBOR (..), ToCBOR (..), enforceSize)
import           Cardano.Prelude (NoUnexpectedThunks (..))
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
import qualified Ouroboros.Consensus.Ledger.History as History
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Util (ShowProxy (..), (...:), (..:))
import           Ouroboros.Consensus.Util.CBOR (decodeSeq)
import           Ouroboros.Consensus.Util.Versioned

import qualified Control.State.Transition as STS
import qualified Shelley.Spec.Ledger.API as SL
import qualified Shelley.Spec.Ledger.BaseTypes as SL
import qualified Shelley.Spec.Ledger.Genesis as SL
import qualified Shelley.Spec.Ledger.LedgerState as SL
import qualified Shelley.Spec.Ledger.StabilityWindow as SL
import qualified Shelley.Spec.Ledger.STS.Chain as STS
import qualified Shelley.Spec.Ledger.UTxO as SL

import           Ouroboros.Consensus.Shelley.Ledger.Block
import           Ouroboros.Consensus.Shelley.Ledger.TPraos ()
import           Ouroboros.Consensus.Shelley.Protocol (MaxMajorProtVer (..),
                     TPraosCrypto, Ticked (TickedPraosLedgerView))

{-------------------------------------------------------------------------------
  Ledger errors
-------------------------------------------------------------------------------}

data ShelleyLedgerError era
  = TickError  !(SL.TickTransitionError  era)
  | BBodyError !(SL.BlockTransitionError era)
  deriving (Eq, Generic, Show)

instance Era era => NoUnexpectedThunks (ShelleyLedgerError era)

{-------------------------------------------------------------------------------
  Config
-------------------------------------------------------------------------------}

data ShelleyLedgerConfig era = ShelleyLedgerConfig {
      shelleyLedgerGenesis   :: !(SL.ShelleyGenesis era)
      -- | Derived from 'shelleyLedgerGenesis' but we store a cached version
      -- because it used very often.
    , shelleyLedgerGlobals   :: !SL.Globals
      -- | Derived from 'shelleyLedgerGenesis' but we store a cached version
      -- because it used very often.
    , shelleyLedgerEraParams :: !HardFork.EraParams
    }
  deriving (Generic, NoUnexpectedThunks)

shelleyEraParams :: SL.ShelleyGenesis era -> HardFork.EraParams
shelleyEraParams genesis = HardFork.EraParams {
      eraEpochSize  = SL.sgEpochLength genesis
    , eraSlotLength = mkSlotLength $ SL.sgSlotLength genesis
    , eraSafeZone   = HardFork.StandardSafeZone
                        stabilityWindow
                        HardFork.NoLowerBound
    }
  where
    stabilityWindow =
        SL.computeStabilityWindow
          (SL.sgSecurityParam genesis)
          (SL.sgActiveSlotCoeff genesis)

mkShelleyLedgerConfig
  :: SL.ShelleyGenesis era
  -> EpochInfo Identity
  -> MaxMajorProtVer
  -> ShelleyLedgerConfig era
mkShelleyLedgerConfig genesis epochInfo (MaxMajorProtVer maxMajorPV) =
    ShelleyLedgerConfig {
        shelleyLedgerGenesis   = genesis
      , shelleyLedgerGlobals   = SL.mkShelleyGlobals genesis epochInfo maxMajorPV
      , shelleyLedgerEraParams = shelleyEraParams genesis
      }

type instance LedgerCfg (LedgerState (ShelleyBlock era)) = ShelleyLedgerConfig era

{-------------------------------------------------------------------------------
  LedgerState
-------------------------------------------------------------------------------}

data instance LedgerState (ShelleyBlock era) = ShelleyLedgerState {
      ledgerTip    :: !(Point (ShelleyBlock era))
    , shelleyState :: !(SL.ShelleyState era)
    }
  deriving (Eq, Show, Generic, NoUnexpectedThunks)

instance TPraosCrypto era => UpdateLedger (ShelleyBlock era)

{-------------------------------------------------------------------------------
  GetTip
-------------------------------------------------------------------------------}

instance GetTip (LedgerState (ShelleyBlock era)) where
  getTip = castPoint . ledgerTip

instance GetTip (Ticked (LedgerState (ShelleyBlock era))) where
  getTip = castPoint . untickedLedgerTip

{-------------------------------------------------------------------------------
  Ticking
-------------------------------------------------------------------------------}

-- | Ticking only affects the state itself
data instance Ticked (LedgerState (ShelleyBlock era)) = TickedShelleyLedgerState {
      untickedLedgerTip  :: !(Point (ShelleyBlock era))
    , tickedShelleyState :: !(SL.ShelleyState era)
    }
  deriving (Generic, NoUnexpectedThunks)

instance Era era => IsLedger (LedgerState (ShelleyBlock era)) where
  type LedgerErr (LedgerState (ShelleyBlock era)) = ShelleyLedgerError era

  applyChainTick cfg slotNo (ShelleyLedgerState pt bhState) =
      TickedShelleyLedgerState {
          untickedLedgerTip  = pt
        , tickedShelleyState = SL.applyTickTransition
                                 (shelleyLedgerGlobals cfg)
                                 bhState
                                 slotNo
        }

instance TPraosCrypto era
      => ApplyBlock (LedgerState (ShelleyBlock era)) (ShelleyBlock era) where
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
     (TPraosCrypto era, Monad m)
  => (SL.Globals -> SL.ShelleyState era -> SL.Block era -> m (SL.ShelleyState era))
  -> LedgerConfig (ShelleyBlock era)
  -> ShelleyBlock era
  -> Ticked (LedgerState (ShelleyBlock era))
  -> m (LedgerState (ShelleyBlock era))
applyHelper f cfg blk (TickedShelleyLedgerState _ oldShelleyState) = do
    newShelleyState <- f globals oldShelleyState (shelleyBlockRaw blk)

    return ShelleyLedgerState {
        ledgerTip    = blockPoint blk
      , shelleyState = newShelleyState
      }
  where
    globals = shelleyLedgerGlobals cfg

instance TPraosCrypto era => LedgerSupportsProtocol (ShelleyBlock era) where
  protocolLedgerView _cfg = TickedPraosLedgerView
                          . SL.currentLedgerView
                          . tickedShelleyState

  ledgerViewForecastAt cfg ledgerState = Forecast at $ \for -> if
      | NotOrigin for == at ->
        return $ TickedPraosLedgerView $ SL.currentLedgerView shelleyState
      | for < maxFor ->
        return $ futureLedgerView for
      | otherwise ->
        throwError $ OutsideForecastRange {
            outsideForecastAt     = at
          , outsideForecastMaxFor = maxFor
          , outsideForecastFor    = for
          }
    where
      ShelleyLedgerState { shelleyState } = ledgerState
      globals = shelleyLedgerGlobals cfg
      swindow = SL.stabilityWindow globals
      at      = ledgerTipSlot ledgerState

      -- | 'SL.futureLedgerView' imposes its own bounds. Those bounds could
      -- /exceed/ the 'maxFor' we have computed, but should never be /less/.
      futureLedgerView :: SlotNo -> Ticked (SL.LedgerView era)
      futureLedgerView =
            either
              (\e -> error ("futureLedgerView failed: " <> show e))
              TickedPraosLedgerView
          . SL.futureLedgerView globals shelleyState

      -- Exclusive upper bound
      maxFor :: SlotNo
      maxFor = case at of
          Origin      -> SlotNo swindow
          NotOrigin s -> SlotNo $ unSlotNo s + 1 + swindow

instance HasHardForkHistory (ShelleyBlock era) where
  type HardForkIndices (ShelleyBlock era) = '[ShelleyBlock era]
  hardForkSummary = neverForksHardForkSummary shelleyLedgerEraParams

{-------------------------------------------------------------------------------
  QueryLedger
-------------------------------------------------------------------------------}

newtype NonMyopicMemberRewards era = NonMyopicMemberRewards {
      unNonMyopicMemberRewards ::
        Map (Either SL.Coin (SL.Credential 'SL.Staking era))
            (Map (SL.KeyHash 'SL.StakePool era) SL.Coin)
    }
  deriving stock   (Show)
  deriving newtype (Eq)

type Delegations era = Map (SL.Credential 'SL.Staking era) (SL.KeyHash 'SL.StakePool era)

instance Era era => Serialise (NonMyopicMemberRewards era) where
  encode = toCBOR . unNonMyopicMemberRewards
  decode = NonMyopicMemberRewards <$> fromCBOR

data instance Query (ShelleyBlock era) :: Type -> Type where
  GetLedgerTip :: Query (ShelleyBlock era) (Point (ShelleyBlock era))
  GetEpochNo :: Query (ShelleyBlock era) EpochNo
  -- | Calculate the Non-Myopic Pool Member Rewards for a set of
  -- credentials. See 'SL.getNonMyopicMemberRewards'
  GetNonMyopicMemberRewards
    :: Set (Either SL.Coin (SL.Credential 'SL.Staking era))
    -> Query (ShelleyBlock era) (NonMyopicMemberRewards era)
  GetCurrentPParams
    :: Query (ShelleyBlock era) SL.PParams
  GetProposedPParamsUpdates
    :: Query (ShelleyBlock era) (SL.ProposedPPUpdates era)
  GetStakeDistribution
    :: Query (ShelleyBlock era) (SL.PoolDistr era)
  GetFilteredUTxO
    :: Set (SL.Addr era)
    -> Query (ShelleyBlock era) (SL.UTxO era)
  GetUTxO
    :: Query (ShelleyBlock era) (SL.UTxO era)

  -- | Only for debugging purposes, we don't guarantee binary compatibility.
  -- Moreover, it is huge.
  GetCurrentEpochState
    :: Query (ShelleyBlock era) (SL.EpochState era)

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
    :: Query (ShelleyBlock era) result
    -> Query (ShelleyBlock era) (Serialised result)

  GetFilteredDelegationsAndRewardAccounts
    :: Set (SL.Credential 'SL.Staking era)
    -> Query (ShelleyBlock era) (Delegations era, SL.RewardAccounts era)

instance Typeable era => ShowProxy (Query (ShelleyBlock era)) where

instance TPraosCrypto era => QueryLedger (ShelleyBlock era) where
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
          (shelleyState st)
          creds
    where
      globals = shelleyLedgerGlobals cfg

instance SameDepIndex (Query (ShelleyBlock era)) where
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

deriving instance Eq   (Query (ShelleyBlock era) result)
deriving instance Show (Query (ShelleyBlock era) result)

instance Era era => ShowQuery (Query (ShelleyBlock era)) where
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

instance TPraosCrypto era => CommonProtocolParams (ShelleyBlock era) where
  maxHeaderSize = fromIntegral . SL._maxBHSize . getPParams . shelleyState
  maxTxSize     = fromIntegral . SL._maxTxSize . getPParams . shelleyState

{-------------------------------------------------------------------------------
  ValidateEnvelope
-------------------------------------------------------------------------------}

instance Era era => BasicEnvelopeValidation (ShelleyBlock era) where
  -- defaults all OK

instance Era era => ValidateEnvelope (ShelleyBlock era) where
  type OtherHeaderEnvelopeError (ShelleyBlock era) =
    STS.PredicateFailure (STS.CHAIN era)

  additionalEnvelopeChecks cfg (TickedPraosLedgerView ledgerView) hdr =
      SL.chainChecks globals pparams (shelleyHeaderRaw hdr)
    where
      pparams = SL.lvProtParams ledgerView
      globals = shelleyLedgerGlobals (configLedger cfg)

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

getPParams :: SL.ShelleyState era -> SL.PParams
getPParams = SL.esPp . SL.nesEs

getProposedPPUpdates :: SL.ShelleyState era -> SL.ProposedPPUpdates era
getProposedPPUpdates = SL.proposals . SL._ppups
                     . SL._utxoState . SL.esLState . SL.nesEs

-- Get the current EpochState. This is mainly for debugging.
getCurrentEpochState :: SL.ShelleyState era -> SL.EpochState era
getCurrentEpochState = SL.nesEs

getDState :: SL.ShelleyState era -> SL.DState era
getDState = SL._dstate . SL._delegationState . SL.esLState . SL.nesEs

getFilteredDelegationsAndRewardAccounts :: SL.ShelleyState era
                                        -> Set (SL.Credential 'SL.Staking era)
                                        -> (Delegations era, SL.RewardAccounts era)
getFilteredDelegationsAndRewardAccounts ss creds =
    (filteredDelegations, filteredRwdAcnts)
  where
    dstate = getDState ss
    filteredDelegations = Map.restrictKeys (SL._delegations dstate) creds
    filteredRwdAcnts = Map.restrictKeys (SL._rewards dstate) creds

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

serialisationFormatVersion0 :: VersionNumber
serialisationFormatVersion0 = 0

serialisationFormatVersion1 :: VersionNumber
serialisationFormatVersion1 = 1

encodeShelleyAnnTip :: Era era => AnnTip (ShelleyBlock era) -> Encoding
encodeShelleyAnnTip = defaultEncodeAnnTip toCBOR

decodeShelleyAnnTip :: Era era => Decoder s (AnnTip (ShelleyBlock era))
decodeShelleyAnnTip = defaultDecodeAnnTip fromCBOR

encodeShelleyHeaderState :: Era era
                         => HeaderState (ShelleyBlock era)
                         -> Encoding
encodeShelleyHeaderState = encodeHeaderState
    encode
    encodeShelleyAnnTip

encodeShelleyLedgerState :: Era era => LedgerState (ShelleyBlock era) -> Encoding
encodeShelleyLedgerState
    ShelleyLedgerState { ledgerTip, shelleyState } =
    encodeVersion serialisationFormatVersion1 $ mconcat [
        CBOR.encodeListLen 2
      , encode ledgerTip
      , toCBOR shelleyState
      ]

decodeShelleyLedgerState ::
     forall era s. Era era
  => Decoder s (LedgerState (ShelleyBlock era))
decodeShelleyLedgerState = decodeVersion [
      (serialisationFormatVersion0, Decode decodeShelleyLedgerState0)
    , (serialisationFormatVersion1, Decode decodeShelleyLedgerState1)
    ]
  where
    decodeShelleyLedgerState0 :: Decoder s' (LedgerState (ShelleyBlock era))
    decodeShelleyLedgerState0 = do
      enforceSize "LedgerState ShelleyBlock" 3
      ledgerTip    <- decode
      _ :: Proxy era <- decodeLedgerViewHistory
      shelleyState <- fromCBOR
      return ShelleyLedgerState { ledgerTip, shelleyState }

    decodeShelleyLedgerState1 :: Decoder s' (LedgerState (ShelleyBlock era))
    decodeShelleyLedgerState1 = do
      enforceSize "LedgerState ShelleyBlock" 2
      ledgerTip    <- decode
      shelleyState <- fromCBOR
      return ShelleyLedgerState { ledgerTip, shelleyState }

-- | Decode what used to be the @LedgerViewHistory@, removed as part of #1935.
--
-- Since we're no longer interested in it, ignore the resulting bytes.
--
-- But to remain binary compatible with old Shelley ledger state, we have to
-- consume the exact bytes corresponding to the ledger view history.
--
-- We return a 'Proxy' with the crypto type to avoid ambiguity.
decodeLedgerViewHistory :: forall era s. Era era => Decoder s (Proxy era)
decodeLedgerViewHistory = do
    enforceSize "LedgerViewHistory" 2
    _ :: WithOrigin SlotNo <- decode
    _ :: StrictSeq (History.Snapshot (SL.LedgerView era)) <- decodeSeq fromCBOR
    return (Proxy @era)

encodeShelleyQuery :: Era era => Query (ShelleyBlock era) result -> Encoding
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

decodeShelleyQuery :: Era era => Decoder s (SomeBlock Query (ShelleyBlock era))
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
  :: Era era
  => Query (ShelleyBlock era) result -> result -> Encoding
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
  :: Era era
  => Query (ShelleyBlock era) result
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
