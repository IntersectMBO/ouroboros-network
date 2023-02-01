{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DisambiguateRecordFields   #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Shelley.Ledger.Ledger (
    LedgerState (..)
  , LedgerTables (..)
  , ShelleyBasedEra
  , ShelleyLedgerError (..)
  , ShelleyTip (..)
  , ShelleyTransition (..)
  , Ticked1 (..)
  , castShelleyTip
  , shelleyLedgerTipPoint
  , shelleyTipToPoint
    -- * Ledger config
  , ShelleyLedgerConfig (..)
  , mkShelleyLedgerConfig
  , shelleyEraParams
  , shelleyEraParamsNeverHardForks
  , shelleyLedgerGenesis
    -- * Auxiliary
  , ShelleyLedgerEvent (..)
  , ShelleyReapplyException (..)
  , getPParams
    -- * Serialisation
  , decodeShelleyAnnTip
  , decodeShelleyLedgerState
  , encodeShelleyAnnTip
  , encodeShelleyHeaderState
  , encodeShelleyLedgerState
  ) where

import           Codec.CBOR.Decoding (Decoder)
import qualified Codec.CBOR.Decoding as CBOR
import           Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as CBOR
import           Codec.Serialise (decode, encode)
import           Control.Arrow (left)
import qualified Control.Exception as Exception
import           Control.Monad.Except
import           Data.Coerce (coerce)
import           Data.Functor ((<&>))
import           Data.Functor.Identity
import qualified Data.Text as Text
import           Data.Word
import           GHC.Generics (Generic)
import           GHC.Records
import           NoThunks.Class (NoThunks (..))

import           Cardano.Binary (FromCBOR (..), ToCBOR (..), enforceSize)
import           Cardano.Slotting.EpochInfo

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime.WallClock.Types
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HardFork.Abstract
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import           Ouroboros.Consensus.HardFork.History.Util
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.CommonProtocolParams
import           Ouroboros.Consensus.Ledger.Tables
import qualified Ouroboros.Consensus.Storage.LedgerDB.HD.DiffSeq as DS
import           Ouroboros.Consensus.Ticked
import           Ouroboros.Consensus.Util ((..:))
import           Ouroboros.Consensus.Util.CBOR (decodeWithOrigin,
                     encodeWithOrigin)
import           Ouroboros.Consensus.Util.Versioned

import qualified Cardano.Ledger.BaseTypes as SL (epochInfoPure)
import qualified Cardano.Ledger.BHeaderView as SL (BHeaderView)
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Control.State.Transition.Extended as STS

import           Ouroboros.Consensus.Protocol.Ledger.Util (isNewEpoch)
import           Ouroboros.Consensus.Protocol.TPraos (MaxMajorProtVer (..))
import           Ouroboros.Consensus.Shelley.Eras (EraCrypto)
import           Ouroboros.Consensus.Shelley.Ledger.Block
import           Ouroboros.Consensus.Shelley.Ledger.Config
import           Ouroboros.Consensus.Shelley.Ledger.Protocol ()
import           Ouroboros.Consensus.Shelley.Protocol.Abstract
                     (EnvelopeCheckError, envelopeChecks, mkHeaderView)

{-------------------------------------------------------------------------------
  Ledger errors
-------------------------------------------------------------------------------}

newtype ShelleyLedgerError era = BBodyError (SL.BlockTransitionError era)
  deriving (Generic)

deriving instance ShelleyBasedEra era => Eq   (ShelleyLedgerError era)
deriving instance ShelleyBasedEra era => Show (ShelleyLedgerError era)

instance ShelleyBasedEra era => NoThunks (ShelleyLedgerError era)

{-------------------------------------------------------------------------------
  Config
-------------------------------------------------------------------------------}

data ShelleyLedgerConfig era = ShelleyLedgerConfig {
      shelleyLedgerCompactGenesis     :: !(CompactGenesis era)
      -- | Derived from 'shelleyLedgerGenesis' but we store a cached version
      -- because it used very often.
    , shelleyLedgerGlobals            :: !SL.Globals
    , shelleyLedgerTranslationContext :: !(Core.TranslationContext era)
    }
  deriving (Generic, NoThunks)

shelleyLedgerGenesis :: ShelleyLedgerConfig era -> SL.ShelleyGenesis era
shelleyLedgerGenesis = getCompactGenesis . shelleyLedgerCompactGenesis

shelleyEraParams ::
     SL.ShelleyGenesis era
  -> HardFork.EraParams
shelleyEraParams genesis = HardFork.EraParams {
      eraEpochSize  = SL.sgEpochLength genesis
    , eraSlotLength = mkSlotLength $ SL.sgSlotLength genesis
    , eraSafeZone   = HardFork.StandardSafeZone stabilityWindow
    }
  where
    stabilityWindow =
        SL.computeStabilityWindow
          (SL.sgSecurityParam genesis)
          (SL.sgActiveSlotCoeff genesis)

-- | Separate variant of 'shelleyEraParams' to be used for a Shelley-only chain.
shelleyEraParamsNeverHardForks :: SL.ShelleyGenesis era -> HardFork.EraParams
shelleyEraParamsNeverHardForks genesis = HardFork.EraParams {
      eraEpochSize  = SL.sgEpochLength genesis
    , eraSlotLength = mkSlotLength $ SL.sgSlotLength genesis
    , eraSafeZone   = HardFork.UnsafeIndefiniteSafeZone
    }

mkShelleyLedgerConfig
  :: SL.ShelleyGenesis era
  -> Core.TranslationContext era
  -> EpochInfo (Except HardFork.PastHorizonException)
  -> MaxMajorProtVer
  -> ShelleyLedgerConfig era
mkShelleyLedgerConfig genesis transCtxt epochInfo mmpv =
    ShelleyLedgerConfig {
        shelleyLedgerCompactGenesis     = compactGenesis genesis
      , shelleyLedgerGlobals            =
          SL.mkShelleyGlobals
            genesis
            (hoistEpochInfo (left (Text.pack . show) . runExcept) epochInfo)
            maxMajorPV
      , shelleyLedgerTranslationContext = transCtxt
      }
  where
    MaxMajorProtVer maxMajorPV = mmpv

type instance LedgerCfg (LedgerState (ShelleyBlock proto era)) = ShelleyLedgerConfig era

{-------------------------------------------------------------------------------
  LedgerState
-------------------------------------------------------------------------------}

data ShelleyTip proto era = ShelleyTip {
      shelleyTipSlotNo  :: !SlotNo
    , shelleyTipBlockNo :: !BlockNo
    , shelleyTipHash    :: !(HeaderHash (ShelleyBlock proto era))
    }
  deriving (Eq, Show, Generic, NoThunks)

shelleyTipToPoint :: WithOrigin (ShelleyTip proto era) -> Point (ShelleyBlock proto era)
shelleyTipToPoint Origin          = GenesisPoint
shelleyTipToPoint (NotOrigin tip) = BlockPoint (shelleyTipSlotNo tip)
                                               (shelleyTipHash   tip)

castShelleyTip ::
     HeaderHash (ShelleyBlock proto era) ~ HeaderHash (ShelleyBlock proto' era')
  => ShelleyTip proto era -> ShelleyTip proto' era'
castShelleyTip (ShelleyTip sn bn hh) = ShelleyTip {
      shelleyTipSlotNo  = sn
    , shelleyTipBlockNo = bn
    , shelleyTipHash    = coerce hh
    }

data instance LedgerState (ShelleyBlock proto era) mk = ShelleyLedgerState {
      shelleyLedgerTip        :: !(WithOrigin (ShelleyTip proto era))
    , shelleyLedgerState      :: !(SL.NewEpochState era)
    , shelleyLedgerTransition :: !ShelleyTransition
    , shelleyLedgerTables     :: !(LedgerTables (LedgerState (ShelleyBlock proto era)) mk)
    }
  deriving (Generic)

deriving instance (IsMapKind mk, ShelleyBasedEra era) => Show     (LedgerState (ShelleyBlock proto era) mk)
deriving instance (IsMapKind mk, ShelleyBasedEra era) => Eq       (LedgerState (ShelleyBlock proto era) mk)
deriving instance (IsMapKind mk, ShelleyBasedEra era) => NoThunks (LedgerState (ShelleyBlock proto era) mk)

-- | Information required to determine the hard fork point from Shelley to the
-- next ledger
newtype ShelleyTransition = ShelleyTransitionInfo {
      -- | The number of blocks in this epoch past the voting deadline
      --
      -- We record this to make sure that we can tell the HFC about hard forks
      -- if and only if we are certain:
      --
      -- 1. Blocks that came in within an epoch after the 4k/f voting deadline
      --    are not relevant (10k/f - 2 * 3k/f).
      -- 2. Since there are slots between blocks, we are probably only sure that
      --    there will be no more relevant block when we have seen the first
      --    block after the deadline.
      -- 3. If we count how many blocks we have seen post deadline, and we have
      --    reached k of them, we know that that last pre-deadline block won't
      --    be rolled back anymore.
      -- 4. At this point we can look at the ledger state and see which
      --    proposals we accepted in the voting period, if any, and notify the
      --    HFC is one of them indicates a transition.
      shelleyAfterVoting :: Word32
    }
  deriving stock   (Eq, Show, Generic)
  deriving newtype (NoThunks)

shelleyLedgerTipPoint :: LedgerState (ShelleyBlock proto era) mk -> Point (ShelleyBlock proto era)
shelleyLedgerTipPoint = shelleyTipToPoint . shelleyLedgerTip

instance ShelleyCompatible proto era => UpdateLedger (ShelleyBlock proto era)

{-------------------------------------------------------------------------------
  GetTip
-------------------------------------------------------------------------------}

instance GetTip (LedgerState (ShelleyBlock proto era) mk) where
  getTip = castPoint . shelleyLedgerTipPoint

instance GetTip (Ticked1 (LedgerState (ShelleyBlock proto era)) mk) where
  getTip = castPoint . shelleyTipToPoint . untickedShelleyLedgerTip

{-------------------------------------------------------------------------------
  Ticking
-------------------------------------------------------------------------------}

-- | Ticking only affects the state itself
data instance Ticked1 (LedgerState (ShelleyBlock proto era)) mk = TickedShelleyLedgerState {
      untickedShelleyLedgerTip      :: !(WithOrigin (ShelleyTip proto era))
      -- | We are counting blocks within an epoch, this means:
      --
      -- 1. We are only incrementing this when /applying/ a block, not when ticking.
      -- 2. However, we count within an epoch, which is slot-based. So the count
      --    must be reset when /ticking/, not when applying a block.
    , tickedShelleyLedgerTransition :: !ShelleyTransition
    , tickedShelleyLedgerState      :: !(SL.NewEpochState era)
    , tickedShelleyLedgerTables     :: !(LedgerTables (LedgerState (ShelleyBlock proto era)) mk)
    }
  deriving (Generic)

deriving instance (IsMapKind mk, ShelleyBasedEra era)
               => NoThunks (Ticked1 (LedgerState (ShelleyBlock proto era)) mk)

instance ShelleyBasedEra era => IsLedger (LedgerState (ShelleyBlock proto era)) where
  type LedgerErr (LedgerState (ShelleyBlock proto era)) = ShelleyLedgerError era

  type AuxLedgerEvent (LedgerState (ShelleyBlock proto era)) = ShelleyLedgerEvent era

  applyChainTickLedgerResult cfg slotNo ShelleyLedgerState{
                                shelleyLedgerTip
                              , shelleyLedgerState
                              , shelleyLedgerTransition
                              , shelleyLedgerTables
                              } =
      swizzle appTick <&> \l' ->
      TickedShelleyLedgerState {
          untickedShelleyLedgerTip =
            shelleyLedgerTip
        , tickedShelleyLedgerTransition =
            -- The voting resets each epoch
            if isNewEpoch ei (shelleyTipSlotNo <$> shelleyLedgerTip) slotNo then
              ShelleyTransitionInfo { shelleyAfterVoting = 0 }
            else
              shelleyLedgerTransition
        , tickedShelleyLedgerState = l'
        , tickedShelleyLedgerTables = shelleyLedgerTables
        }
    where
      globals = shelleyLedgerGlobals cfg

      ei :: EpochInfo Identity
      ei = SL.epochInfoPure globals

      swizzle (l, events) =
          LedgerResult {
              lrEvents = map ShelleyLedgerEventTICK events
            , lrResult = l
            }

      appTick =
        SL.applyTickOpts
          STS.ApplySTSOpts {
              asoAssertions = STS.globalAssertionPolicy
            , asoValidation = STS.ValidateAll
            , asoEvents     = STS.EPReturn
            }
          globals
          shelleyLedgerState
          slotNo

-- | All events emitted by the Shelley ledger API
data ShelleyLedgerEvent era =
    -- | An event emitted when (re)applying a block
    ShelleyLedgerEventBBODY (STS.Event (Core.EraRule "BBODY" era))
    -- | An event emitted during the chain tick
  | ShelleyLedgerEventTICK  (STS.Event (Core.EraRule "TICK"  era))

instance ShelleyCompatible proto era
      => ApplyBlock (LedgerState (ShelleyBlock proto era)) (ShelleyBlock proto era) where
  -- Note: in the Shelley ledger, the @CHAIN@ rule is used to apply a whole
  -- block. In consensus, we split up the application of a block to the ledger
  -- into separate steps that are performed together by 'applyExtLedgerState':
  --
  -- + 'applyChainTickLedgerResult': executes the @TICK@ transition
  -- + 'validateHeader':
  --    - 'validateEnvelope': executes the @chainChecks@
  --    - 'updateChainDepState': executes the @PRTCL@ transition
  -- + 'applyBlockLedgerResult': executes the @BBODY@ transition
  --
  applyBlockLedgerResult =
      applyHelper (swizzle ..: appBlk)
    where
      swizzle m =
        withExcept BBodyError m <&> \(l, events) ->
          LedgerResult {
              lrEvents = map ShelleyLedgerEventBBODY events
            , lrResult = l
            }

      -- Apply the BBODY transition using the ticked state
      appBlk =
        SL.applyBlockOpts
          STS.ApplySTSOpts {
              asoAssertions = STS.globalAssertionPolicy
            , asoValidation = STS.ValidateAll
            , asoEvents     = STS.EPReturn
            }

  reapplyBlockLedgerResult =
      runIdentity ..: applyHelper (swizzle ..: reappBlk)
    where
      swizzle m = case runExcept m of
        Left err          ->
          Exception.throw $! ShelleyReapplyException @era err
        Right (l, events) ->
          pure LedgerResult {
              lrEvents = map ShelleyLedgerEventBBODY events
            , lrResult = l
            }

      -- Reapply the BBODY transition using the ticked state
      reappBlk =
        SL.applyBlockOpts
          STS.ApplySTSOpts {
                  asoAssertions = STS.AssertionsOff
                , asoValidation = STS.ValidateNone
                , asoEvents     = STS.EPReturn
                }

data ShelleyReapplyException =
  forall era. Show (SL.BlockTransitionError era)
  => ShelleyReapplyException (SL.BlockTransitionError era)

instance Show ShelleyReapplyException where
  show (ShelleyReapplyException err) = "(ShelleyReapplyException " <> show err <> ")"

instance Exception.Exception ShelleyReapplyException where

applyHelper ::
     (ShelleyCompatible proto era, Monad m)
  => (   SL.Globals
      -> SL.NewEpochState era
      -> SL.Block (SL.BHeaderView (EraCrypto era)) era
      -> m (LedgerResult
              (LedgerState (ShelleyBlock proto era))
              (SL.NewEpochState era)
           )
     )
  -> LedgerConfig (ShelleyBlock proto era)
  -> ShelleyBlock proto era
  -> Ticked1 (LedgerState (ShelleyBlock proto era)) mk
  -> m (LedgerResult
          (LedgerState (ShelleyBlock proto era))
          (LedgerState (ShelleyBlock proto era) mk))
applyHelper f cfg blk TickedShelleyLedgerState{
                          tickedShelleyLedgerTransition
                        , tickedShelleyLedgerState
                        , tickedShelleyLedgerTables
                        } = do
    ledgerResult <-
      f
        globals
        tickedShelleyLedgerState
        ( let b  = shelleyBlockRaw blk
              h' = mkHeaderView (SL.bheader b)
          -- Jared Corduan explains that the " Unsafe " here ultimately only
          -- means the value must not be serialized. We're only passing it to
          -- 'STS.applyBlockOpts', which does not serialize it. So this is a
          -- safe use.
          in SL.UnsafeUnserialisedBlock h' (SL.bbody b)
        )

    return $ ledgerResult <&> \newNewEpochState -> ShelleyLedgerState {
        shelleyLedgerTip = NotOrigin ShelleyTip {
            shelleyTipBlockNo = blockNo   blk
          , shelleyTipSlotNo  = blockSlot blk
          , shelleyTipHash    = blockHash blk
          }
      , shelleyLedgerState =
          newNewEpochState
      , shelleyLedgerTransition = ShelleyTransitionInfo {
            shelleyAfterVoting =
              -- We count the number of blocks that have been applied after the
              -- voting deadline has passed.
              (if blockSlot blk >= votingDeadline then succ else id) $
                shelleyAfterVoting tickedShelleyLedgerTransition
          }
      , shelleyLedgerTables = tickedShelleyLedgerTables
      }
  where
    globals = shelleyLedgerGlobals cfg
    swindow = SL.stabilityWindow globals

    ei :: EpochInfo Identity
    ei = SL.epochInfoPure globals

    -- The start of the next epoch is within the safe zone, always.
    startOfNextEpoch :: SlotNo
    startOfNextEpoch = runIdentity $ do
        blockEpoch <- epochInfoEpoch ei (blockSlot blk)
        let nextEpoch = succ blockEpoch
        epochInfoFirst ei nextEpoch

    -- The block must come in strictly before the voting deadline
    -- See Fig 13, "Protocol Parameter Update Inference Rules", of the
    -- Shelley specification.
    votingDeadline :: SlotNo
    votingDeadline = subSlots (2 * swindow) startOfNextEpoch

instance HasHardForkHistory (ShelleyBlock proto era) where
  type HardForkIndices (ShelleyBlock proto era) = '[ShelleyBlock proto era]
  hardForkSummary = neverForksHardForkSummary $
      shelleyEraParamsNeverHardForks . shelleyLedgerGenesis

instance ShelleyCompatible proto era
      => CommonProtocolParams (ShelleyBlock proto era) where
  maxHeaderSize = fromIntegral . getField @"_maxBHSize" . getPParams . shelleyLedgerState
  maxTxSize     = fromIntegral . getField @"_maxTxSize" . getPParams . shelleyLedgerState

{-------------------------------------------------------------------------------
  ValidateEnvelope
-------------------------------------------------------------------------------}

instance ShelleyCompatible proto era => BasicEnvelopeValidation (ShelleyBlock proto era) where
  -- defaults all OK

instance ShelleyCompatible proto era => ValidateEnvelope (ShelleyBlock proto era) where
  type OtherHeaderEnvelopeError (ShelleyBlock proto era) =
    EnvelopeCheckError proto

  additionalEnvelopeChecks cfg tlv hdr =
    envelopeChecks (configConsensus cfg) tlv (shelleyHeaderRaw hdr)

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

getPParams :: SL.NewEpochState era -> Core.PParams era
getPParams = SL.esPp . SL.nesEs

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

-- | Current version
--
-- o 'serialisationFormatVersion0' used to include the 'LedgerViewHistory', but
--   since we had to break binary backwards compatibility of the 'TPraosState',
--   we dropped backwards compatibility with 'serialisationFormatVersion0' too.
-- o 'serialisationFormatVersion1' did not include a 'BlockNo' at the tip of
--   the ledger, which was introduced in version 2. Again, since we broke
--   compat anyway, we dropped support for version 1.
serialisationFormatVersion2 :: VersionNumber
serialisationFormatVersion2 = 2

encodeShelleyAnnTip ::
     ShelleyCompatible proto era
  => AnnTip (ShelleyBlock proto era) -> Encoding
encodeShelleyAnnTip = defaultEncodeAnnTip toCBOR

decodeShelleyAnnTip ::
     ShelleyCompatible proto era
  => Decoder s (AnnTip (ShelleyBlock proto era))
decodeShelleyAnnTip = defaultDecodeAnnTip fromCBOR

encodeShelleyHeaderState ::
     ShelleyCompatible proto era
  => HeaderState (ShelleyBlock proto era)
  -> Encoding
encodeShelleyHeaderState = encodeHeaderState
    encode
    encodeShelleyAnnTip

encodeShelleyTip :: ShelleyCompatible proto era => ShelleyTip proto era -> Encoding
encodeShelleyTip ShelleyTip {
                     shelleyTipSlotNo
                   , shelleyTipBlockNo
                   , shelleyTipHash
                   } = mconcat [
      CBOR.encodeListLen 3
    , encode shelleyTipSlotNo
    , encode shelleyTipBlockNo
    , encode shelleyTipHash
    ]

decodeShelleyTip :: ShelleyCompatible proto era => Decoder s (ShelleyTip proto era)
decodeShelleyTip = do
    enforceSize "ShelleyTip" 3
    shelleyTipSlotNo  <- decode
    shelleyTipBlockNo <- decode
    shelleyTipHash    <- decode
    return ShelleyTip {
        shelleyTipSlotNo
      , shelleyTipBlockNo
      , shelleyTipHash
      }

encodeShelleyTransition :: ShelleyTransition -> Encoding
encodeShelleyTransition ShelleyTransitionInfo{shelleyAfterVoting} = mconcat [
      CBOR.encodeWord32 shelleyAfterVoting
    ]

decodeShelleyTransition :: Decoder s ShelleyTransition
decodeShelleyTransition = do
    shelleyAfterVoting <- CBOR.decodeWord32
    return ShelleyTransitionInfo{shelleyAfterVoting}

encodeShelleyLedgerState ::
     ShelleyCompatible proto era
  => LedgerState (ShelleyBlock proto era) mk
  -> Encoding
encodeShelleyLedgerState
    ShelleyLedgerState { shelleyLedgerTip
                       , shelleyLedgerState
                       , shelleyLedgerTransition
                       } =
    encodeVersion serialisationFormatVersion2 $ mconcat [
        CBOR.encodeListLen 3
      , encodeWithOrigin encodeShelleyTip shelleyLedgerTip
      , toCBOR shelleyLedgerState
      , encodeShelleyTransition shelleyLedgerTransition
      ]

decodeShelleyLedgerState ::
     forall era proto s mk. (IsMapKind mk, ShelleyCompatible proto era)
  => Decoder s (LedgerState (ShelleyBlock proto era) mk)
decodeShelleyLedgerState = decodeVersion [
      (serialisationFormatVersion2, Decode decodeShelleyLedgerState2)
    ]
  where
    decodeShelleyLedgerState2 :: Decoder s' (LedgerState (ShelleyBlock proto era) mk)
    decodeShelleyLedgerState2 = do
      enforceSize "LedgerState ShelleyBlock" 3
      shelleyLedgerTip        <- decodeWithOrigin decodeShelleyTip
      shelleyLedgerState      <- fromCBOR
      shelleyLedgerTransition <- decodeShelleyTransition
      return ShelleyLedgerState {
          shelleyLedgerTip
        , shelleyLedgerState
        , shelleyLedgerTransition
        , shelleyLedgerTables = pureLedgerTables emptyMK
        }

{-------------------------------------------------------------------------------
  Ledger Tables
-------------------------------------------------------------------------------}

instance ShelleyBasedEra era => HasLedgerTables (LedgerState (ShelleyBlock proto era)) where
  newtype instance LedgerTables (LedgerState (ShelleyBlock proto era)) mk = ShelleyLedgerTables {
        shelleyUTxOTable :: mk (SL.TxIn (EraCrypto era)) (Core.TxOut era)
      }
    deriving (Generic)

  projectLedgerTables        = shelleyLedgerTables
  withLedgerTables st tables =
      ShelleyLedgerState {
          shelleyLedgerTip
        , shelleyLedgerState
        , shelleyLedgerTransition
        , shelleyLedgerTables     = tables
        }
    where
      ShelleyLedgerState {
          shelleyLedgerTip
        , shelleyLedgerState
        , shelleyLedgerTransition
        } = st

  pureLedgerTables f = ShelleyLedgerTables f

  mapLedgerTables f (ShelleyLedgerTables utxo) = ShelleyLedgerTables (f utxo)

  traverseLedgerTables f (ShelleyLedgerTables utxo) = ShelleyLedgerTables <$> f utxo

  zipLedgerTables f (ShelleyLedgerTables utxoL) (ShelleyLedgerTables utxoR) =
      ShelleyLedgerTables (f utxoL utxoR)

  zipLedgerTables2
    f
    (ShelleyLedgerTables utxoL)
    (ShelleyLedgerTables utxoC)
    (ShelleyLedgerTables utxoR) =
      ShelleyLedgerTables (f utxoL utxoC utxoR)

  zipLedgerTablesA f (ShelleyLedgerTables utxoL) (ShelleyLedgerTables utxoR) =
      ShelleyLedgerTables <$> f utxoL utxoR

  zipLedgerTables2A
    f
    (ShelleyLedgerTables utxoL)
    (ShelleyLedgerTables utxoC)
    (ShelleyLedgerTables utxoR) =
      ShelleyLedgerTables <$> f utxoL utxoC utxoR

  foldLedgerTables f (ShelleyLedgerTables utxo) = f utxo

  foldLedgerTables2 f (ShelleyLedgerTables utxoL) (ShelleyLedgerTables utxoR) = f utxoL utxoR

  namesLedgerTables = ShelleyLedgerTables (NameMK "utxo")

instance ShelleyBasedEra era => HasTickedLedgerTables (LedgerState (ShelleyBlock proto era)) where
  projectLedgerTablesTicked        = tickedShelleyLedgerTables
  withLedgerTablesTicked st tables =
      TickedShelleyLedgerState {
          untickedShelleyLedgerTip
        , tickedShelleyLedgerTransition
        , tickedShelleyLedgerState
        , tickedShelleyLedgerTables     = tables
        }
    where
      TickedShelleyLedgerState {
          untickedShelleyLedgerTip
        , tickedShelleyLedgerTransition
        , tickedShelleyLedgerState
        } = st

deriving newtype  instance (ShelleyBasedEra era, Eq       (mk (SL.TxIn (EraCrypto era)) (Core.TxOut era))) => Eq       (LedgerTables (LedgerState (ShelleyBlock proto era)) mk)
deriving newtype  instance (ShelleyBasedEra era, Show     (mk (SL.TxIn (EraCrypto era)) (Core.TxOut era))) => Show     (LedgerTables (LedgerState (ShelleyBlock proto era)) mk)
deriving anyclass instance (ShelleyBasedEra era, NoThunks (mk (SL.TxIn (EraCrypto era)) (Core.TxOut era))) => NoThunks (LedgerTables (LedgerState (ShelleyBlock proto era)) mk)

instance ShelleyBasedEra era => CanSerializeLedgerTables (LedgerState (ShelleyBlock proto era)) where
    codecLedgerTables = ShelleyLedgerTables (CodecMK toCBOR toCBOR fromCBOR fromCBOR)

instance
     ShelleyBasedEra era
  => CanStowLedgerTables (LedgerState (ShelleyBlock proto era)) where
  stowLedgerTables st =
      ShelleyLedgerState {
          shelleyLedgerTip        = shelleyLedgerTip
        , shelleyLedgerState      =
            shelleyLedgerState `withUtxoSL` shelleyUTxOTable shelleyLedgerTables
        , shelleyLedgerTransition = shelleyLedgerTransition
        , shelleyLedgerTables     = pureLedgerTables emptyMK
        }
    where
      ShelleyLedgerState {
          shelleyLedgerTip
        , shelleyLedgerState
        , shelleyLedgerTransition
        , shelleyLedgerTables
        } = st
  unstowLedgerTables st =
      ShelleyLedgerState {
          shelleyLedgerTip        = shelleyLedgerTip
        , shelleyLedgerState      =
            shelleyLedgerState `withUtxoSL` emptyMK
        , shelleyLedgerTransition = shelleyLedgerTransition
        , shelleyLedgerTables     =
            ShelleyLedgerTables $ projectUtxoSL shelleyLedgerState
        }
    where
      ShelleyLedgerState {
          shelleyLedgerTip
        , shelleyLedgerState
        , shelleyLedgerTransition
        } = st

projectUtxoSL ::
     SL.NewEpochState era
  -> ValuesMK (SL.TxIn (EraCrypto era)) (Core.TxOut era)
projectUtxoSL =
      ValuesMK
    . DS.Values
    . SL.unUTxO
    . SL._utxo
    . SL.lsUTxOState
    . SL.esLState
    . SL.nesEs

withUtxoSL ::
     SL.NewEpochState era
  -> ValuesMK (SL.TxIn (EraCrypto era)) (Core.TxOut era)
  -> SL.NewEpochState era
withUtxoSL nes (ValuesMK (DS.Values m)) =
    nes {
        SL.nesEs = es {
            SL.esLState = us {
                SL.lsUTxOState = utxo {
                    SL._utxo = SL.UTxO m
                  }
              }
          }
      }
  where
    es   = SL.nesEs nes
    us   = SL.esLState es
    utxo = SL.lsUTxOState us
