{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DisambiguateRecordFields   #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE MultiWayIf                 #-}
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
  , ShelleyBasedEra
  , ShelleyLedgerError (..)
  , ShelleyTip (..)
  , ShelleyTransition (..)
  , Ticked (..)
  , shelleyLedgerTipPoint
  , shelleyTipToPoint
    -- * Ledger config
  , ShelleyLedgerConfig (..)
  , mkShelleyLedgerConfig
  , shelleyEraParams
  , shelleyEraParamsNeverHardForks
  , shelleyLedgerGenesis
    -- * Auxiliary
  , getPParams
    -- * Serialisation
  , decodeShelleyAnnTip
  , decodeShelleyLedgerState
  , encodeShelleyAnnTip
  , encodeShelleyHeaderState
  , encodeShelleyLedgerState
  ) where

import           GHC.Prim
import           GHC.Types (Int(I#))

import           Codec.CBOR.Decoding (Decoder)
import qualified Codec.CBOR.Decoding as CBOR
import           Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as CBOR
import           Codec.Serialise (decode, encode)
import           Control.Monad.Except
import           Data.Functor.Identity
import           Data.Word
import           GHC.Generics (Generic)
import           GHC.Records
import           NoThunks.Class (NoThunks (..))

import           Cardano.Binary (FromCBOR (..), ToCBOR (..), enforceSize)
import           Cardano.Slotting.EpochInfo

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime.WallClock.Types
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Forecast
import           Ouroboros.Consensus.HardFork.Abstract
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import           Ouroboros.Consensus.HardFork.History.Util
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.CommonProtocolParams
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Util ((...:), (..:))
import           Ouroboros.Consensus.Util.CBOR (decodeWithOrigin,
                     encodeWithOrigin)
import           Ouroboros.Consensus.Util.Versioned

import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Era as Core
import qualified Shelley.Spec.Ledger.API as SL
import qualified Shelley.Spec.Ledger.STS.Chain as SL (PredicateFailure)

import           Ouroboros.Consensus.Shelley.Eras (EraCrypto)
import           Ouroboros.Consensus.Shelley.Ledger.Block
import           Ouroboros.Consensus.Shelley.Ledger.Config
import           Ouroboros.Consensus.Shelley.Ledger.Stub
import           Ouroboros.Consensus.Shelley.Ledger.TPraos ()
import           Ouroboros.Consensus.Shelley.Protocol (MaxMajorProtVer (..),
                     Ticked (TickedPraosLedgerView))
import           Ouroboros.Consensus.Shelley.Protocol.Util (isNewEpoch)

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
            (HardFork.toPureEpochInfo epochInfo)
            maxMajorPV
      , shelleyLedgerTranslationContext = transCtxt
      }
  where
    MaxMajorProtVer maxMajorPV = mmpv

type instance LedgerCfg (LedgerState (ShelleyBlock era)) = ShelleyLedgerConfig era

{-------------------------------------------------------------------------------
  LedgerState
-------------------------------------------------------------------------------}

data ShelleyTip era = ShelleyTip {
      shelleyTipSlotNo  :: !SlotNo
    , shelleyTipBlockNo :: !BlockNo
    , shelleyTipHash    :: !(HeaderHash (ShelleyBlock era))
    }
  deriving (Eq, Show, Generic, NoThunks)

shelleyTipToPoint :: WithOrigin (ShelleyTip era) -> Point (ShelleyBlock era)
shelleyTipToPoint Origin          = GenesisPoint
shelleyTipToPoint (NotOrigin tip) = BlockPoint (shelleyTipSlotNo tip)
                                               (shelleyTipHash   tip)

data instance LedgerState (ShelleyBlock era) = ShelleyLedgerState {
      shelleyLedgerTip        :: !(WithOrigin (ShelleyTip era))
    , shelleyLedgerState      :: !(SL.NewEpochState era)
    , shelleyLedgerTransition :: !ShelleyTransition
    }
  deriving (Generic)

deriving instance ShelleyBasedEra era => Show     (LedgerState (ShelleyBlock era))
deriving instance ShelleyBasedEra era => Eq       (LedgerState (ShelleyBlock era))
deriving instance ShelleyBasedEra era => NoThunks (LedgerState (ShelleyBlock era))

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

shelleyLedgerTipPoint :: LedgerState (ShelleyBlock era) -> Point (ShelleyBlock era)
shelleyLedgerTipPoint = shelleyTipToPoint . shelleyLedgerTip

instance ShelleyBasedEra era => UpdateLedger (ShelleyBlock era)

{-------------------------------------------------------------------------------
  GetTip
-------------------------------------------------------------------------------}

instance GetTip (LedgerState (ShelleyBlock era)) where
  getTip = castPoint . shelleyLedgerTipPoint

instance GetTip (Ticked (LedgerState (ShelleyBlock era))) where
  getTip = castPoint . untickedShelleyLedgerTipPoint

{-------------------------------------------------------------------------------
  Ticking
-------------------------------------------------------------------------------}

-- | Ticking only affects the state itself
data instance Ticked (LedgerState (ShelleyBlock era)) = TickedShelleyLedgerState {
      untickedShelleyLedgerTip      :: !(WithOrigin (ShelleyTip era))
      -- | We are counting blocks within an epoch, this means:
      --
      -- 1. We are only incrementing this when /applying/ a block, not when ticking.
      -- 2. However, we count within an epoch, which is slot-based. So the count
      --    must be reset when /ticking/, not when applying a block.
    , tickedShelleyLedgerTransition :: !ShelleyTransition
    , tickedShelleyLedgerState      :: !(SL.NewEpochState era)
    }
  deriving (Generic)

deriving instance ShelleyBasedEra era
               => NoThunks (Ticked (LedgerState (ShelleyBlock era)))

untickedShelleyLedgerTipPoint ::
     Ticked (LedgerState (ShelleyBlock era))
  -> Point (ShelleyBlock era)
untickedShelleyLedgerTipPoint = shelleyTipToPoint . untickedShelleyLedgerTip

instance ShelleyBasedEra era => IsLedger (LedgerState (ShelleyBlock era)) where
  type LedgerErr (LedgerState (ShelleyBlock era)) = ShelleyLedgerError era

  applyChainTick cfg slotNo ShelleyLedgerState{
                                shelleyLedgerTip
                              , shelleyLedgerState
                              , shelleyLedgerTransition
                              } =
      TickedShelleyLedgerState {
          untickedShelleyLedgerTip =
            shelleyLedgerTip
        , tickedShelleyLedgerTransition =
            -- The voting resets each epoch
            if isNewEpoch ei (shelleyTipSlotNo <$> shelleyLedgerTip) slotNo then
              ShelleyTransitionInfo { shelleyAfterVoting = 0 }
            else
              shelleyLedgerTransition
        , tickedShelleyLedgerState =
            SL.applyTick
              globals
              shelleyLedgerState
              slotNo
        }
    where
      globals = shelleyLedgerGlobals cfg

      ei :: EpochInfo Identity
      ei = SL.epochInfo globals

instance ShelleyBasedEra era
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
  applyLedgerBlock x y z =
      seq (stubComputation (stubComputationArg
                             + I# (reallyUnsafePtrEquality# x x)
                             + I# (reallyUnsafePtrEquality# y y)
                             + I# (reallyUnsafePtrEquality# z z))) $
      applyHelper
        -- Apply the BBODY transition using the ticked state
        (withExcept BBodyError ..: SL.applyBlock)
        x y z

  reapplyLedgerBlock x y z =
    ((seq (stubComputation (stubComputationArg
                             + I# (reallyUnsafePtrEquality# x x)
                             + I# (reallyUnsafePtrEquality# y y)
                             + I# (reallyUnsafePtrEquality# z z)))
      .
      runIdentity) ...:
      applyHelper)
         -- Reapply the BBODY transition using the ticked state
         (Identity ..: SL.reapplyBlock)
         x y z

applyHelper ::
     (ShelleyBasedEra era, Monad m)
  => (SL.Globals -> SL.NewEpochState era -> SL.Block era -> m (SL.NewEpochState era))
  -> LedgerConfig (ShelleyBlock era)
  -> ShelleyBlock era
  -> Ticked (LedgerState (ShelleyBlock era))
  -> m (LedgerState (ShelleyBlock era))
applyHelper f cfg blk TickedShelleyLedgerState{
                          tickedShelleyLedgerTransition
                        , tickedShelleyLedgerState
                        } = do
    newNewEpochState <- f globals tickedShelleyLedgerState (shelleyBlockRaw blk)

    return ShelleyLedgerState {
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
      }
  where
    globals = shelleyLedgerGlobals cfg
    swindow = SL.stabilityWindow globals

    ei :: EpochInfo Identity
    ei = SL.epochInfo globals

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

instance ShelleyBasedEra era
      => LedgerSupportsProtocol (ShelleyBlock era) where
  protocolLedgerView _cfg = TickedPraosLedgerView
                          . SL.currentLedgerView
                          . tickedShelleyLedgerState

  ledgerViewForecastAt cfg ledgerState = Forecast at $ \for -> if
      | NotOrigin for == at ->
        return $ TickedPraosLedgerView $ SL.currentLedgerView shelleyLedgerState
      | for < maxFor ->
        return $ futureLedgerView for
      | otherwise ->
        throwError $ OutsideForecastRange {
            outsideForecastAt     = at
          , outsideForecastMaxFor = maxFor
          , outsideForecastFor    = for
          }
    where
      ShelleyLedgerState { shelleyLedgerState } = ledgerState
      globals = shelleyLedgerGlobals cfg
      swindow = SL.stabilityWindow globals
      at      = ledgerTipSlot ledgerState

      -- | 'SL.futureLedgerView' imposes its own bounds. Those bounds could
      -- /exceed/ the 'maxFor' we have computed, but should never be /less/.
      futureLedgerView :: SlotNo -> Ticked (SL.LedgerView (EraCrypto era))
      futureLedgerView =
            either
              (\e -> error ("futureLedgerView failed: " <> show e))
              TickedPraosLedgerView
          . SL.futureLedgerView globals shelleyLedgerState

      -- Exclusive upper bound
      maxFor :: SlotNo
      maxFor = addSlots swindow $ succWithOrigin at

instance HasHardForkHistory (ShelleyBlock era) where
  type HardForkIndices (ShelleyBlock era) = '[ShelleyBlock era]
  hardForkSummary = neverForksHardForkSummary $
      shelleyEraParamsNeverHardForks . shelleyLedgerGenesis

instance ShelleyBasedEra era
      => CommonProtocolParams (ShelleyBlock era) where
  maxHeaderSize = fromIntegral . getField @"_maxBHSize" . getPParams . shelleyLedgerState
  maxTxSize     = fromIntegral . getField @"_maxTxSize" . getPParams . shelleyLedgerState

{-------------------------------------------------------------------------------
  ValidateEnvelope
-------------------------------------------------------------------------------}

instance ShelleyBasedEra era => BasicEnvelopeValidation (ShelleyBlock era) where
  -- defaults all OK

instance ShelleyBasedEra era => ValidateEnvelope (ShelleyBlock era) where
  type OtherHeaderEnvelopeError (ShelleyBlock era) =
    SL.PredicateFailure (SL.CHAIN era)

  additionalEnvelopeChecks cfg (TickedPraosLedgerView ledgerView) hdr =
      SL.chainChecks globals (SL.lvChainChecks ledgerView) (shelleyHeaderRaw hdr)
    where
      globals = shelleyLedgerGlobals (configLedger cfg)

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
     ShelleyBasedEra era
  => AnnTip (ShelleyBlock era) -> Encoding
encodeShelleyAnnTip = defaultEncodeAnnTip toCBOR

decodeShelleyAnnTip ::
     ShelleyBasedEra era
  => Decoder s (AnnTip (ShelleyBlock era))
decodeShelleyAnnTip = defaultDecodeAnnTip fromCBOR

encodeShelleyHeaderState ::
     ShelleyBasedEra era
  => HeaderState (ShelleyBlock era)
  -> Encoding
encodeShelleyHeaderState = encodeHeaderState
    encode
    encodeShelleyAnnTip

encodeShelleyTip :: ShelleyBasedEra era => ShelleyTip era -> Encoding
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

decodeShelleyTip :: ShelleyBasedEra era => Decoder s (ShelleyTip era)
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
     ShelleyBasedEra era
  => LedgerState (ShelleyBlock era)
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
     forall era s. ShelleyBasedEra era
  => Decoder s (LedgerState (ShelleyBlock era))
decodeShelleyLedgerState = decodeVersion [
      (serialisationFormatVersion2, Decode decodeShelleyLedgerState2)
    ]
  where
    decodeShelleyLedgerState2 :: Decoder s' (LedgerState (ShelleyBlock era))
    decodeShelleyLedgerState2 = do
      enforceSize "LedgerState ShelleyBlock" 3
      shelleyLedgerTip        <- decodeWithOrigin decodeShelleyTip
      shelleyLedgerState      <- fromCBOR
      shelleyLedgerTransition <- decodeShelleyTransition
      return ShelleyLedgerState {
          shelleyLedgerTip
        , shelleyLedgerState
        , shelleyLedgerTransition
        }
