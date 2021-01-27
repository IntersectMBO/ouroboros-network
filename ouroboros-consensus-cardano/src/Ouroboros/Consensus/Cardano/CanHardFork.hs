{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveAnyClass           #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TupleSections            #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE UndecidableInstances     #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Cardano.CanHardFork (
    TriggerHardFork (..)
  , ByronPartialLedgerConfig (..)
  , ShelleyPartialLedgerConfig (..)
  , CardanoHardForkConstraints
  , forecastAcrossShelley
  , translateChainDepStateAcrossShelley
  ) where

import           Control.Monad
import           Control.Monad.Except (Except, runExcept, throwError)
import qualified Data.Map.Strict as Map
import           Data.Maybe (listToMaybe, mapMaybe)
import           Data.Proxy
import           Data.SOP.Strict ((:.:) (..), NP (..), unComp)
import           Data.Void (Void)
import           Data.Word
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

import           Cardano.Crypto.DSIGN (Ed25519DSIGN)
import           Cardano.Crypto.Hash.Blake2b (Blake2b_224, Blake2b_256)

import qualified Cardano.Chain.Common as CC
import qualified Cardano.Chain.Genesis as CC.Genesis
import qualified Cardano.Chain.Update as CC.Update

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Forecast
import           Ouroboros.Consensus.HardFork.History (Bound (boundSlot),
                     addSlots)
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util (eitherToMaybe)
import           Ouroboros.Consensus.Util.RedundantConstraints

import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.State.Types
import           Ouroboros.Consensus.HardFork.Combinator.Util.InPairs
                     (RequiringBoth (..), ignoringBoth)
import           Ouroboros.Consensus.HardFork.Combinator.Util.Tails (Tails (..))

import           Ouroboros.Consensus.Byron.Ledger
import qualified Ouroboros.Consensus.Byron.Ledger.Inspect as Byron.Inspect
import           Ouroboros.Consensus.Byron.Node ()
import           Ouroboros.Consensus.Protocol.PBFT (PBft, PBftCrypto)
import           Ouroboros.Consensus.Protocol.PBFT.State (PBftState)
import qualified Ouroboros.Consensus.Protocol.PBFT.State as PBftState

import           Ouroboros.Consensus.Shelley.Ledger
import qualified Ouroboros.Consensus.Shelley.Ledger.Inspect as Shelley.Inspect
import           Ouroboros.Consensus.Shelley.Node ()
import           Ouroboros.Consensus.Shelley.Protocol

import           Cardano.Ledger.Allegra.Translation ()
import           Cardano.Ledger.Crypto (ADDRHASH, DSIGN, HASH)
import qualified Cardano.Ledger.Era as SL
import           Cardano.Ledger.Mary.Translation ()
import qualified Shelley.Spec.Ledger.API as SL

import           Ouroboros.Consensus.Cardano.Block


{-------------------------------------------------------------------------------
  Figure out the transition point for Byron

  The Byron ledger defines the update 'State' in
  "Cardano.Chain.Update.Validation.Interface". The critical piece of state we
  need is

  > candidateProtocolUpdates :: ![CandidateProtocolUpdate]

  which are the update proposals that have been voted on, accepted, and
  endorsed, and now need to become stable. In `tryBumpVersion`
  ("Cardano.Chain.Update.Validation.Interface.ProtocolVersionBump") we
  find the candidates that are at least 'kUpdateStabilityParam' (@== 4k@) deep,
  and then construct

  > State
  > { nextProtocolVersion    = cpuProtocolVersion
  > , nextProtocolParameters = cpuProtocolParameters
  > }

  (with 'State' from "Cardano.Chain.Update.Validation.Interface.ProtocolVersionBump")
  where 'cpuProtocolVersion'/'cpuProtocolParameters' are the version and
  parameters from the update. This then ends up in the following callstack

  > applyChainTick
  > |
  > \-- epochTransition
  >     |
  >     \-- registerEpoch
  >         |
  >         \-- tryBumpVersion

  Now, if this is changing the major version of the protocol, then this actually
  indicates the transition to Shelley, and the Byron 'applyChainTick' won't
  actually happen. Instead, in 'singleEraTransition' we will report the
  'EpochNo' of the transition as soon as it's @2k@ (not @4k@!) deep: in other
  words, as soon as it is stable; at this point, the HFC will do the rest.

  A slightly subtle point is that the Byron ledger does not record any
  information about /past/ updates to the protocol parameters, and so if we
  /were/ to ask the Byron ledger /after/ the update when the transition is
  going to take place (did take place), it will say 'Nothing': transition not
  yet known. In practice this won't matter, as it will have been translated to
  a Shelley ledger at that point.
-------------------------------------------------------------------------------}

byronTransition :: PartialLedgerConfig ByronBlock
                -> Word16   -- ^ Shelley major protocol version
                -> LedgerState ByronBlock
                -> Maybe EpochNo
byronTransition ByronPartialLedgerConfig{..} shelleyMajorVersion state =
      takeAny
    . mapMaybe isTransitionToShelley
    . Byron.Inspect.protocolUpdates byronLedgerConfig
    $ state
  where
    ByronTransitionInfo transitionInfo = byronLedgerTransition state

    genesis = byronLedgerConfig
    k       = CC.Genesis.gdK $ CC.Genesis.configGenesisData genesis

    isTransitionToShelley :: Byron.Inspect.ProtocolUpdate -> Maybe EpochNo
    isTransitionToShelley update = do
        guard $ CC.Update.pvMajor version == shelleyMajorVersion
        case Byron.Inspect.protocolUpdateState update of
          Byron.Inspect.UpdateCandidate _becameCandidateSlotNo adoptedIn -> do
            becameCandidateBlockNo <- Map.lookup version transitionInfo
            guard $ isReallyStable becameCandidateBlockNo
            return adoptedIn
          Byron.Inspect.UpdateStableCandidate adoptedIn ->
            -- If the Byron ledger thinks it's stable, it's _definitely_ stable
            return adoptedIn
          _otherwise ->
            -- The proposal isn't yet a candidate, never mind a stable one
            mzero
      where
        version :: CC.Update.ProtocolVersion
        version = Byron.Inspect.protocolUpdateVersion update

    -- Normally, stability in the ledger is defined in terms of slots, not
    -- blocks. Byron considers the proposal to be stable after the slot is more
    -- than @2k@ old. That is not wrong: after @2k@, the block indeed is stable.
    --
    -- Unfortunately, this means that the /conclusion about stability itself/
    -- is /not/ stable: if we were to switch to a denser fork, we might change
    -- our mind (on the sparse chain we thought the block was already stable,
    -- but on the dense chain we conclude it is it not yet stable).
    --
    -- It is unclear at the moment if this presents a problem; the HFC assumes
    -- monotonicity of timing info, in the sense that that any slot/time
    -- conversions are either unknown or else not subject to rollback.
    -- The problem sketched above might mean that we can go from "conversion
    -- known" to "conversion unknown", but then when we go back again to
    -- "conversion known", we /are/ guaranteed that we'd get the same answer.
    --
    -- Rather than trying to analyse this subtle problem, we instead base
    -- stability on block numbers; after the block is `k` deep, we know for sure
    -- that it is stable, and moreover, no matter which chain we switch to, that
    -- will remain to be the case.
    --
    -- The Byron 'UpdateState' records the 'SlotNo' of the block in which the
    -- proposal became a candidate (i.e., when the last required endorsement
    -- came in). That doesn't tell us very much, we need to know the block
    -- number; that's precisely what the 'ByronTransition' part of the Byron
    -- state tells us.
    isReallyStable :: BlockNo -> Bool
    isReallyStable (BlockNo bno) = distance >= CC.unBlockCount k
      where
        distance :: Word64
        distance = case byronLedgerTipBlockNo state of
                     Origin                  -> bno + 1
                     NotOrigin (BlockNo tip) -> tip - bno

    -- We only expect a single proposal that updates to Shelley, but in case
    -- there are multiple, any one will do
    takeAny :: [a] -> Maybe a
    takeAny = listToMaybe

{-------------------------------------------------------------------------------
  Figure out the transition point for Shelley
-------------------------------------------------------------------------------}

shelleyTransition ::
     forall era. ShelleyBasedEra era
  => PartialLedgerConfig (ShelleyBlock era)
  -> Word16   -- ^ Next era's major protocol version
  -> LedgerState (ShelleyBlock era)
  -> Maybe EpochNo
shelleyTransition ShelleyPartialLedgerConfig{..}
                  transitionMajorVersion
                  state =
      takeAny
    . mapMaybe isTransition
    . Shelley.Inspect.protocolUpdates genesis
    $ state
  where
    ShelleyTransitionInfo{..} = shelleyLedgerTransition state

    -- 'shelleyLedgerConfig' contains a dummy 'EpochInfo' but this does not
    -- matter for extracting the genesis config
    genesis :: SL.ShelleyGenesis era
    genesis = shelleyLedgerGenesis shelleyLedgerConfig

    k :: Word64
    k = SL.sgSecurityParam genesis

    isTransition :: Shelley.Inspect.ProtocolUpdate era -> Maybe EpochNo
    isTransition Shelley.Inspect.ProtocolUpdate{..} = do
         SL.ProtVer major _minor <- proposalVersion
         guard $ fromIntegral major == transitionMajorVersion
         guard $ proposalReachedQuorum
         guard $ shelleyAfterVoting >= fromIntegral k
         return proposalEpoch
       where
         Shelley.Inspect.UpdateProposal{..} = protocolUpdateProposal
         Shelley.Inspect.UpdateState{..}    = protocolUpdateState

    -- In principle there could be multiple proposals that all change the
    -- major protocol version. In practice this can't happen because each
    -- delegate can only vote for one proposal, but the types don't guarantee
    -- this. We don't need to worry about this, and just pick any of them.
    takeAny :: [a] -> Maybe a
    takeAny = listToMaybe

{-------------------------------------------------------------------------------
  SingleEraBlock Byron
-------------------------------------------------------------------------------}

instance SingleEraBlock ByronBlock where
  singleEraTransition pcfg _eraParams _eraStart ledgerState =
      case byronTriggerHardFork pcfg of
        TriggerHardForkNever                         -> Nothing
        TriggerHardForkAtEpoch   epoch               -> Just epoch
        TriggerHardForkAtVersion shelleyMajorVersion ->
            byronTransition
              pcfg
              shelleyMajorVersion
              ledgerState

  singleEraInfo _ = SingleEraInfo {
      singleEraName = "Byron"
    }

instance PBftCrypto bc => HasPartialConsensusConfig (PBft bc)
  -- Use defaults

-- | The trigger condition that will cause the hard fork transition.
data TriggerHardFork =
    -- | Trigger the transition when the on-chain protocol major version (from
    -- the ledger state) reaches this number.
    TriggerHardForkAtVersion !Word16
    -- | For testing only, trigger the transition at a specific hard-coded
    -- epoch, irrespective of the ledger state.
  | TriggerHardForkAtEpoch !EpochNo
    -- | Never trigger a hard fork
  | TriggerHardForkNever
  deriving (Show, Generic, NoThunks)

-- | When Byron is part of the hard-fork combinator, we use the partial ledger
-- config. Standalone Byron uses the regular ledger config. This means that
-- the partial ledger config is the perfect place to store the trigger
-- condition for the hard fork to Shelley, as we don't have to modify the
-- ledger config for standalone Byron.
data ByronPartialLedgerConfig = ByronPartialLedgerConfig {
      byronLedgerConfig    :: !(LedgerConfig ByronBlock)
    , byronTriggerHardFork :: !TriggerHardFork
    }
  deriving (Generic, NoThunks)

instance HasPartialLedgerConfig ByronBlock where

  type PartialLedgerConfig ByronBlock = ByronPartialLedgerConfig

  completeLedgerConfig _ _ = byronLedgerConfig

{-------------------------------------------------------------------------------
  SingleEraBlock Shelley
-------------------------------------------------------------------------------}

instance ShelleyBasedEra era => SingleEraBlock (ShelleyBlock era) where
  singleEraTransition pcfg _eraParams _eraStart ledgerState =
      case shelleyTriggerHardFork pcfg of
        TriggerHardForkNever                         -> Nothing
        TriggerHardForkAtEpoch   epoch               -> Just epoch
        TriggerHardForkAtVersion shelleyMajorVersion ->
            shelleyTransition
              pcfg
              shelleyMajorVersion
              ledgerState

  singleEraInfo _ = SingleEraInfo {
      singleEraName = shelleyBasedEraName (Proxy @era)
    }

instance PraosCrypto c => HasPartialConsensusConfig (TPraos c) where
  type PartialConsensusConfig (TPraos c) = TPraosParams

  completeConsensusConfig _ tpraosEpochInfo tpraosParams = TPraosConfig {..}

data ShelleyPartialLedgerConfig era = ShelleyPartialLedgerConfig {
      -- | We cache the non-partial ledger config containing a dummy
      -- 'EpochInfo' that needs to be replaced with the correct one.
      --
      -- We do this to avoid recomputing the ledger config each time
      -- 'completeLedgerConfig' is called, as 'mkShelleyLedgerConfig' does
      -- some rather expensive computations that shouldn't be repeated too
      -- often (e.g., 'sgActiveSlotCoeff').
      shelleyLedgerConfig    :: !(ShelleyLedgerConfig era)
    , shelleyTriggerHardFork :: !TriggerHardFork
    }
  deriving (Generic, NoThunks)

instance ShelleyBasedEra era => HasPartialLedgerConfig (ShelleyBlock era) where
  type PartialLedgerConfig (ShelleyBlock era) = ShelleyPartialLedgerConfig era

  -- Replace the dummy 'EpochInfo' with the real one
  completeLedgerConfig _ epochInfo (ShelleyPartialLedgerConfig cfg _) =
      cfg {
          shelleyLedgerGlobals = (shelleyLedgerGlobals cfg) {
              SL.epochInfo = epochInfo
            }
        }

{-------------------------------------------------------------------------------
  CanHardFork
-------------------------------------------------------------------------------}

type CardanoHardForkConstraints c =
  ( PraosCrypto c
  , ShelleyBasedEra (ShelleyEra c)
  , ShelleyBasedEra (AllegraEra c)
  , ShelleyBasedEra (MaryEra    c)
    -- These equalities allow the transition from Byron to Shelley, since
    -- @shelley-spec-ledger@ requires Ed25519 for Byron bootstrap addresses and
    -- the current Byron-to-Shelley translation requires a 224-bit hash for
    -- address and a 256-bit hash for header hashes.
  , HASH     c ~ Blake2b_256
  , ADDRHASH c ~ Blake2b_224
  , DSIGN    c ~ Ed25519DSIGN
  )

instance CardanoHardForkConstraints c => CanHardFork (CardanoEras c) where
  hardForkEraTranslation = EraTranslation {
      translateLedgerState   =
          PCons translateLedgerStateByronToShelleyWrapper
        $ PCons translateLedgerStateShelleyToAllegraWrapper
        $ PCons translateLedgerStateAllegraToMaryWrapper
        $ PCons translateLedgerStateMaryToAlonzoWrapper
        $ PNil
    , translateChainDepState =
          PCons translateChainDepStateByronToShelleyWrapper
        $ PCons translateChainDepStateAcrossShelley
        $ PCons translateChainDepStateAcrossShelley
        $ PCons translateChainDepStateAcrossShelley
        $ PNil
    , translateLedgerView    =
          PCons translateLedgerViewByronToShelleyWrapper
        $ PCons translateLedgerViewAcrossShelley
        $ PCons translateLedgerViewAcrossShelley
        $ PCons translateLedgerViewAcrossShelley
        $ PNil
    }
  hardForkChainSel =
        -- Byron <-> Shelley, ...
        TCons (   CompareBlockNo
               :* CompareBlockNo
               :* CompareBlockNo
               :* CompareBlockNo
               :* Nil)
        -- Shelley <-> Allegra, ...
      $ TCons (SelectSameProtocol :* SelectSameProtocol :* SelectSameProtocol :* Nil)
        -- Allegra <-> Mary, ...
      $ TCons (SelectSameProtocol :* SelectSameProtocol :* Nil)
        -- Mary <-> ...
      $ TCons (SelectSameProtocol :* Nil)
        -- Alonzo <-> ...
      $ TCons Nil
      $ TNil
  hardForkInjectTxs =
        PCons (ignoringBoth cannotInjectTx)
      $ PCons (ignoringBoth translateTxShelleyToAllegraWrapper)
      $ PCons (ignoringBoth translateTxAllegraToMaryWrapper)
      $ PCons (ignoringBoth translateTxMaryToAlonzoWrapper)
      $ PNil

{-------------------------------------------------------------------------------
  Translation from Byron to Shelley
-------------------------------------------------------------------------------}

translateHeaderHashByronToShelley ::
     forall c.
     ( ShelleyBasedEra (ShelleyEra c)
     , HASH c ~ Blake2b_256
     )
  => HeaderHash ByronBlock
  -> HeaderHash (ShelleyBlock (ShelleyEra c))
translateHeaderHashByronToShelley =
      fromShortRawHash (Proxy @(ShelleyBlock (ShelleyEra c)))
    . toShortRawHash   (Proxy @ByronBlock)
  where
    -- Byron uses 'Blake2b_256' for header hashes
    _ = keepRedundantConstraint (Proxy @(HASH c ~ Blake2b_256))

translatePointByronToShelley ::
     ( ShelleyBasedEra (ShelleyEra c)
     , HASH c ~ Blake2b_256
     )
  => Point ByronBlock
  -> WithOrigin BlockNo
  -> WithOrigin (ShelleyTip (ShelleyEra c))
translatePointByronToShelley point bNo =
    case (point, bNo) of
      (GenesisPoint, Origin) ->
        Origin
      (BlockPoint s h, NotOrigin n) -> NotOrigin ShelleyTip {
          shelleyTipSlotNo  = s
        , shelleyTipBlockNo = n
        , shelleyTipHash    = translateHeaderHashByronToShelley h
        }
      _otherwise ->
        error "translatePointByronToShelley: invalid Byron state"

translateLedgerStateByronToShelleyWrapper ::
     ( ShelleyBasedEra (ShelleyEra c)
     , HASH     c ~ Blake2b_256
     , ADDRHASH c ~ Blake2b_224
     )
  => RequiringBoth
       WrapLedgerConfig
       (Translate LedgerState)
       ByronBlock
       (ShelleyBlock (ShelleyEra c))
translateLedgerStateByronToShelleyWrapper =
    RequireBoth $ \_ (WrapLedgerConfig cfgShelley) ->
    Translate   $ \epochNo ledgerByron ->
      ShelleyLedgerState {
        shelleyLedgerTip =
          translatePointByronToShelley
            (ledgerTipPoint (Proxy @ByronBlock) ledgerByron)
            (byronLedgerTipBlockNo ledgerByron)
      , shelleyLedgerState =
          SL.translateToShelleyLedgerState
            (shelleyLedgerGenesis cfgShelley)
            epochNo
            (byronLedgerState ledgerByron)
      , shelleyLedgerTransition =
          ShelleyTransitionInfo{shelleyAfterVoting = 0}
      }

translateChainDepStateByronToShelleyWrapper ::
     RequiringBoth
       WrapConsensusConfig
       (Translate WrapChainDepState)
       ByronBlock
       (ShelleyBlock (ShelleyEra c))
translateChainDepStateByronToShelleyWrapper =
    RequireBoth $ \_ (WrapConsensusConfig cfgShelley) ->
      Translate $ \_ (WrapChainDepState pbftState) ->
        WrapChainDepState $
          translateChainDepStateByronToShelley cfgShelley pbftState

translateChainDepStateByronToShelley ::
     forall bc c.
     ConsensusConfig (TPraos c)
  -> PBftState bc
  -> TPraosState c
translateChainDepStateByronToShelley TPraosConfig { tpraosParams } pbftState =
    -- Note that the 'PBftState' doesn't know about EBBs. So if the last slot of
    -- the Byron era were occupied by an EBB (and no regular block in that same
    -- slot), we would pick the wrong slot here, i.e., the slot of the regular
    -- block before the EBB.
    --
    -- Fortunately, this is impossible for two reasons:
    --
    -- 1. On mainnet we stopped producing EBBs a while before the transition.
    -- 2. The transition happens at the start of an epoch, so if the last slot
    --    were occupied by an EBB, it must have been the EBB at the start of the
    --    previous epoch. This means the previous epoch must have been empty,
    --    which is a violation of the "@k@ blocks per @2k@ slots" property.
    TPraosState (PBftState.lastSignedSlot pbftState) $
      SL.ChainDepState
        { SL.csProtocol = SL.PrtclState Map.empty nonce nonce
        , SL.csTickn    = SL.TicknState {
              ticknStateEpochNonce    = nonce
            , ticknStatePrevHashNonce = SL.NeutralNonce
            }
          -- Overridden before used
        , SL.csLabNonce = SL.NeutralNonce
        }
  where
    nonce = tpraosInitialNonce tpraosParams

translateLedgerViewByronToShelleyWrapper ::
     forall c.
     RequiringBoth
       WrapLedgerConfig
       (TranslateForecast LedgerState WrapLedgerView)
       ByronBlock
       (ShelleyBlock (ShelleyEra c))
translateLedgerViewByronToShelleyWrapper =
    RequireBoth $ \_ (WrapLedgerConfig cfgShelley) ->
      TranslateForecast (forecast cfgShelley)
  where
    -- We ignore the Byron ledger view and create a new Shelley.
    --
    -- The full Shelley forecast range (stability window) starts from the first
    -- slot of the Shelley era, no matter how many slots there are between the
    -- Byron ledger and the first Shelley slot. Note that this number of slots
    -- is still guaranteed to be less than the forecast range of the HFC in the
    -- Byron era.
    forecast ::
         ShelleyLedgerConfig (ShelleyEra c)
      -> Bound
      -> SlotNo
      -> LedgerState ByronBlock
      -> Except
           OutsideForecastRange
           (Ticked (WrapLedgerView (ShelleyBlock (ShelleyEra c))))
    forecast cfgShelley bound forecastFor currentByronState
        | forecastFor < maxFor
        = return $
            WrapTickedLedgerView $ TickedPraosLedgerView $
              SL.mkInitialShelleyLedgerView
                (shelleyLedgerGenesis cfgShelley)
        | otherwise
        = throwError $ OutsideForecastRange {
              outsideForecastAt     = ledgerTipSlot currentByronState
            , outsideForecastMaxFor = maxFor
            , outsideForecastFor    = forecastFor
            }
      where
        globals = shelleyLedgerGlobals cfgShelley
        swindow = SL.stabilityWindow globals

        -- This is the exclusive upper bound of the forecast range
        --
        -- If Shelley's stability window is 0, it means we can't forecast /at
        -- all/ in the Shelley era. Not even to the first slot in the Shelley
        -- era! Remember that forecasting to slot @S@ means forecasting the
        -- ledger view obtained from the ledger state /after/ applying the block
        -- with slot @S@. If the stability window is 0, we can't even forecast
        -- after the very first "virtual" Shelley block, meaning we can't
        -- forecast into the Shelley era when still in the Byron era.
        maxFor :: SlotNo
        maxFor = addSlots swindow (boundSlot bound)

{-------------------------------------------------------------------------------
  Translation from one Shelley-based era to another Shelley-based era
-------------------------------------------------------------------------------}

instance ( ShelleyBasedEra era
         , ShelleyBasedEra (SL.PreviousEra era)
         , EraCrypto (SL.PreviousEra era) ~ EraCrypto era
         ) => SL.TranslateEra era ShelleyTip where
  translateEra _ (ShelleyTip sno bno (ShelleyHash hash)) =
      return $ ShelleyTip sno bno (ShelleyHash hash)

instance ( ShelleyBasedEra era
         , SL.TranslateEra era ShelleyTip
         , SL.TranslateEra era SL.NewEpochState
         , SL.TranslationError era SL.NewEpochState ~ Void
         ) => SL.TranslateEra era (LedgerState :.: ShelleyBlock) where
  translateEra ctxt (Comp (ShelleyLedgerState tip state _transition)) = do
      tip'   <- mapM (SL.translateEra ctxt) tip
      state' <- SL.translateEra ctxt state
      return $ Comp $ ShelleyLedgerState {
          shelleyLedgerTip        = tip'
        , shelleyLedgerState      = state'
        , shelleyLedgerTransition = ShelleyTransitionInfo 0
        }

instance ( ShelleyBasedEra era
         , SL.TranslateEra era SL.Tx
         ) => SL.TranslateEra era (GenTx :.: ShelleyBlock) where
  type TranslationError era (GenTx :.: ShelleyBlock) = SL.TranslationError era SL.Tx
  translateEra ctxt (Comp (ShelleyTx _txId tx)) =
    -- TODO will the txId stay the same? If so, we could avoid recomputing it
    Comp . mkShelleyTx <$> SL.translateEra ctxt tx

-- | Forecast from a Shelley-based era to the next Shelley-based era.
forecastAcrossShelley ::
     forall eraFrom eraTo.
     ( EraCrypto eraFrom ~ EraCrypto eraTo
     , ShelleyBasedEra eraFrom
     )
  => ShelleyLedgerConfig eraFrom
  -> ShelleyLedgerConfig eraTo
  -> Bound  -- ^ Transition between the two eras
  -> SlotNo -- ^ Forecast for this slot
  -> LedgerState (ShelleyBlock eraFrom)
  -> Except OutsideForecastRange (Ticked (WrapLedgerView (ShelleyBlock eraTo)))
forecastAcrossShelley cfgFrom cfgTo transition forecastFor ledgerStateFrom
    | forecastFor < maxFor
    = return $ futureLedgerView forecastFor
    | otherwise
    = throwError $ OutsideForecastRange {
          outsideForecastAt     = ledgerTipSlot ledgerStateFrom
        , outsideForecastMaxFor = maxFor
        , outsideForecastFor    = forecastFor
        }
  where
    -- | 'SL.futureLedgerView' imposes its own bounds. Those bounds could
    -- /exceed/ the 'maxFor' we have computed, but should never be /less/.
    futureLedgerView :: SlotNo -> Ticked (WrapLedgerView (ShelleyBlock eraTo))
    futureLedgerView =
          WrapTickedLedgerView
        . TickedPraosLedgerView
        . either
            (\e -> error ("futureLedgerView failed: " <> show e))
            id
        . SL.futureLedgerView
            (shelleyLedgerGlobals cfgFrom)
            (shelleyLedgerState ledgerStateFrom)

    -- Exclusive upper bound
    maxFor :: SlotNo
    maxFor = crossEraForecastBound
               (ledgerTipSlot ledgerStateFrom)
               (boundSlot transition)
               (SL.stabilityWindow (shelleyLedgerGlobals cfgFrom))
               (SL.stabilityWindow (shelleyLedgerGlobals cfgTo))

translateChainDepStateAcrossShelley ::
     forall eraFrom eraTo.
     EraCrypto eraFrom ~ EraCrypto eraTo
  => RequiringBoth
       WrapConsensusConfig
       (Translate WrapChainDepState)
       (ShelleyBlock eraFrom)
       (ShelleyBlock eraTo)
translateChainDepStateAcrossShelley =
    ignoringBoth $
      Translate $ \_epochNo (WrapChainDepState chainDepState) ->
        -- Same protocol, same 'ChainDepState'. Note that we don't have to apply
        -- any changes related to an epoch transition, this is already done when
        -- ticking the state.
        WrapChainDepState chainDepState

translateLedgerViewAcrossShelley ::
     forall eraFrom eraTo.
     ( EraCrypto eraFrom ~ EraCrypto eraTo
     , ShelleyBasedEra eraFrom
     )
  => RequiringBoth
       WrapLedgerConfig
       (TranslateForecast LedgerState WrapLedgerView)
       (ShelleyBlock eraFrom)
       (ShelleyBlock eraTo)
translateLedgerViewAcrossShelley =
    RequireBoth $ \(WrapLedgerConfig cfgFrom)
                   (WrapLedgerConfig cfgTo) ->
      TranslateForecast $ forecastAcrossShelley cfgFrom cfgTo

{-------------------------------------------------------------------------------
  Translation from Shelley to Allegra
-------------------------------------------------------------------------------}

translateLedgerStateShelleyToAllegraWrapper ::
     PraosCrypto c
  => RequiringBoth
       WrapLedgerConfig
       (Translate LedgerState)
       (ShelleyBlock (ShelleyEra c))
       (ShelleyBlock (AllegraEra c))
translateLedgerStateShelleyToAllegraWrapper =
    ignoringBoth $
      Translate $ \_epochNo ->
        unComp . SL.translateEra' () . Comp

translateTxShelleyToAllegraWrapper ::
     PraosCrypto c
  => InjectTx
       (ShelleyBlock (ShelleyEra c))
       (ShelleyBlock (AllegraEra c))
translateTxShelleyToAllegraWrapper = InjectTx $
    fmap unComp . eitherToMaybe . runExcept . SL.translateEra () . Comp

{-------------------------------------------------------------------------------
  Translation from Shelley to Allegra
-------------------------------------------------------------------------------}

translateLedgerStateAllegraToMaryWrapper ::
     PraosCrypto c
  => RequiringBoth
       WrapLedgerConfig
       (Translate LedgerState)
       (ShelleyBlock (AllegraEra c))
       (ShelleyBlock (MaryEra c))
translateLedgerStateAllegraToMaryWrapper =
    ignoringBoth $
      Translate $ \_epochNo ->
        unComp . SL.translateEra' () . Comp

translateLedgerStateMaryToAlonzoWrapper ::
    RequiringBoth
       WrapLedgerConfig
       (Translate LedgerState)
       (ShelleyBlock (MaryEra c))
       (ShelleyBlock (AlonzoEra c))
translateLedgerStateMaryToAlonzoWrapper = undefined

translateTxAllegraToMaryWrapper ::
     PraosCrypto c
  => InjectTx
       (ShelleyBlock (AllegraEra c))
       (ShelleyBlock (MaryEra c))
translateTxAllegraToMaryWrapper = InjectTx $
    fmap unComp . eitherToMaybe . runExcept . SL.translateEra () . Comp

translateTxMaryToAlonzoWrapper ::
     InjectTx
       (ShelleyBlock (MaryEra c))
       (ShelleyBlock (AlonzoEra c))
translateTxMaryToAlonzoWrapper = undefined
