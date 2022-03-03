{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DisambiguateRecordFields   #-}
{-# LANGUAGE EmptyCase                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE UndecidableSuperClasses    #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- TODO my recent changes (eg TableStuff instance) cause this module to take
-- about ~20 minutes to typecheck, so I've disabled the pattern-checker (as
-- we've already done in a couple other modules)
{-# OPTIONS_GHC -Wno-orphans
                -Wno-incomplete-patterns
                -Wno-incomplete-uni-patterns
                -Wno-incomplete-record-updates
                -Wno-overlapping-patterns #-}

module Ouroboros.Consensus.Cardano.CanHardFork (
    ByronPartialLedgerConfig (..)
  , CardanoHardForkConstraints
  , TriggerHardFork (..)
    -- * Re-exports of Shelley code
  , ShelleyPartialLedgerConfig (..)
  , forecastAcrossShelley
  , translateChainDepStateAcrossShelley
    -- * Data families
  , LedgerTables (..)
    -- * For re-use by other hardfork tests
  , IsShelleyTele (..)
  , consolidateShelleyNS
  ) where

import           Control.Monad
import           Control.Monad.Except (runExcept, throwError)
import           Data.Kind (Type)
import qualified Data.Map.Strict as Map
import           Data.Maybe (listToMaybe, mapMaybe)
import           Data.Proxy
import           Data.SOP.Strict (NP (..), NS (..), type (-.->), unComp,
                     (:.:) (..))
import qualified Data.SOP.Strict as SOP
import           Data.Word
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)
import           NoThunks.Class (NoThunks)

import           Cardano.Binary (fromCBOR, toCBOR)
import           Cardano.Crypto.DSIGN (Ed25519DSIGN)
import           Cardano.Crypto.Hash.Blake2b (Blake2b_224, Blake2b_256)

import qualified Cardano.Chain.Common as CC
import qualified Cardano.Chain.Genesis as CC.Genesis
import qualified Cardano.Chain.Update as CC.Update

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Forecast
import           Ouroboros.Consensus.HardFork.History (Bound (boundSlot),
                     addSlots)
import           Ouroboros.Consensus.HardFork.Simple
import           Ouroboros.Consensus.Ledger.Abstract
import qualified Ouroboros.Consensus.Storage.LedgerDB.HD as HD
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util (eitherToMaybe)
import           Ouroboros.Consensus.Util.RedundantConstraints
import           Ouroboros.Consensus.Util.SOP (Index (IS, IZ))
import qualified Ouroboros.Consensus.Util.SOP as SOP

import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.State.Types
import           Ouroboros.Consensus.HardFork.Combinator.Util.Functors
                     (Flip (..))
import           Ouroboros.Consensus.HardFork.Combinator.Util.InPairs
                     (RequiringBoth (..), ignoringBoth)
import           Ouroboros.Consensus.HardFork.Combinator.Util.Tails (Tails (..))
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Telescope as Telescope

import           Ouroboros.Consensus.Byron.Ledger
import qualified Ouroboros.Consensus.Byron.Ledger.Inspect as Byron.Inspect
import           Ouroboros.Consensus.Byron.Node ()
import           Ouroboros.Consensus.Protocol.PBFT (PBft, PBftCrypto)
import           Ouroboros.Consensus.Protocol.PBFT.State (PBftState)
import qualified Ouroboros.Consensus.Protocol.PBFT.State as PBftState
import           Ouroboros.Consensus.Protocol.TPraos

import           Ouroboros.Consensus.Shelley.Ledger
import           Ouroboros.Consensus.Shelley.Node ()
import           Ouroboros.Consensus.Shelley.ShelleyHFC

import           Cardano.Ledger.Allegra.Translation ()
import qualified Cardano.Ledger.Alonzo.Genesis as Alonzo
import qualified Cardano.Ledger.Alonzo.Translation as Alonzo
import qualified Cardano.Ledger.Core as Core
import           Cardano.Ledger.Crypto (ADDRHASH, DSIGN, HASH)
import qualified Cardano.Ledger.Era as SL
import           Cardano.Ledger.Mary.Translation ()
import qualified Cardano.Ledger.Shelley.API as SL

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
                -> LedgerState ByronBlock mk
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
  CanHardFork
-------------------------------------------------------------------------------}

type CardanoHardForkConstraints c =
  ( PraosCrypto c
  , ShelleyBasedEra (ShelleyEra c)
  , ShelleyBasedEra (AllegraEra c)
  , ShelleyBasedEra (MaryEra    c)
  , ShelleyBasedEra (AlonzoEra  c)
    -- These equalities allow the transition from Byron to Shelley, since
    -- @cardano-ledger-shelley@ requires Ed25519 for Byron bootstrap addresses and
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
        -- Mary <-> Alonzo, ...
      $ TCons (SelectSameProtocol :* Nil)
        -- Alonzo <-> ...
      $ TCons Nil
      $ TNil
  hardForkInjectTxs =
        PCons (ignoringBoth $ Pair2 cannotInjectTx cannotInjectValidatedTx)
      $ PCons (   ignoringBoth
                $ Pair2
                    translateTxShelleyToAllegraWrapper
                    translateValidatedTxShelleyToAllegraWrapper
              )
      $ PCons (   ignoringBoth
                $ Pair2
                    translateTxAllegraToMaryWrapper
                    translateValidatedTxAllegraToMaryWrapper
              )
      $ PCons (RequireBoth $ \_cfgMary cfgAlonzo ->
                 let ctxt = getAlonzoTranslationContext cfgAlonzo
                 in
                 Pair2
                   (translateTxMaryToAlonzoWrapper          ctxt)
                   (translateValidatedTxMaryToAlonzoWrapper ctxt)
              )
      $ PNil

{-------------------------------------------------------------------------------
  TableStuff
-------------------------------------------------------------------------------}

type CardanoTxOut c = ShelleyTxOut (ShelleyBasedEras c)

-- We reuse this for both TableStuff and TickedTableStuff instances, so the
-- @TickedTableStuff x@ constraint here is excessive in the TableStuff case.
-- However, since x is always a Cardano era, we do know we have
-- TickedTableStuff for every x, so hardcoding the stronger constraint is
-- easier than parameterizing this helper over the constraint.
projectLedgerTablesHelper :: forall c fmk mk.
     (CardanoHardForkConstraints c, IsApplyMapKind mk)
  => (forall x.
         TickedTableStuff (LedgerState x)
      => fmk x -> LedgerTables (LedgerState x) mk
     )
  -> HardForkState fmk (CardanoEras c)
  -> LedgerTables (LedgerState (CardanoBlock c)) mk
projectLedgerTablesHelper prjLT (HardForkState st) =
    case st of
      -- the first era is Byron
      TZ Current {
          currentState = prjLT -> NoByronLedgerTables
        } ->
        polyEmptyLedgerTables

      -- all the remaining eras are Shelley
      TS _past tele ->
          SOP.hcollapse
        $ SOP.hcimap
            (Proxy @(CardanoShelleyBasedEra c))
            projectOne
            (Telescope.tip (consolidateShelleyTele tele))
  where
    projectOne :: forall era.
         CardanoShelleyBasedEra c era
      => SOP.Index (ShelleyBasedEras c)                         era
         -- ^ the current era of the ledger state we're projecting from
      -> (Current fmk :.: ShelleyBlock)                           era
         -- ^ the ledger state we're projecting from
      -> SOP.K (LedgerTables (LedgerState (CardanoBlock c)) mk) era
    projectOne idx (Comp current) =
        SOP.K $ CardanoLedgerTables $ inj appliedMK
      where
        ShelleyLedgerTables appliedMK = prjLT $ currentState current

        inj ::
             ApplyMapKind mk (SL.TxIn c) (Core.TxOut era)
          -> ApplyMapKind mk (SL.TxIn c) (CardanoTxOut c)
        inj = mapValuesAppliedMK (ShelleyTxOut . SOP.injectNS idx . TxOutWrapper)

-- Same note regarding the @TickedTableStuff x@ constraint as
-- 'projectLedgerTablesHelper'
withLedgerTablesHelper ::
  forall c mk fany fmk.
     (CardanoHardForkConstraints c, IsApplyMapKind mk)
  => (forall x.
         TickedTableStuff (LedgerState x)
      => fany x -> LedgerTables (LedgerState x) mk -> fmk x
     )
  -> HardForkState fany (CardanoEras c)
  -> LedgerTables (LedgerState (CardanoBlock c)) mk
  -> HardForkState fmk (CardanoEras c)
withLedgerTablesHelper withLT (HardForkState st) (CardanoLedgerTables appliedMK) =
      HardForkState
    $ case st of
        -- the first era is Byron
        TZ Current {
            currentStart
          , currentState = byronSt
          } ->
          TZ Current {
                 currentStart
               , currentState = withLT byronSt NoByronLedgerTables
               }

        -- all the remaining eras are Shelley
        TS past tele ->
            TS past
          $ unconsolidateShelleyTele
          $ SOP.hap
              updateOne
              (consolidateShelleyTele tele)
  where
    -- how to update the ledger table of each possible individual era
    updateOne ::
      NP
        (     Current fany :.: ShelleyBlock
         -.-> Current fmk  :.: ShelleyBlock
        )
        (ShelleyBasedEras c)
    updateOne =
        ($ translations)
      $ SOP.hcmap (Proxy @(CardanoShelleyBasedEra c))
      $ \translate -> SOP.fn $ \(Comp current) ->
        let Current{currentState = innerSt} = current
            newInnerSt =
                withLT innerSt
              $ ShelleyLedgerTables
              $ mapValuesAppliedMK
                  (unTxOutWrapper . SOP.apFn translate . SOP.K)
                  appliedMK
        in Comp $ current{currentState = newInnerSt}

    -- the composed translations for each possible era; see
    -- 'composeTxOutTranslationPairs' to understand why this is partial but
    -- is safe in the absence of Consensus bugs
    translations ::
      NP
        (SOP.K (CardanoTxOut c) -.-> TxOutWrapper)
        (ShelleyBasedEras c)
    translations =
        SOP.hmap
          (\f -> SOP.fn $ \(SOP.K (ShelleyTxOut x)) -> f `SOP.apFn` SOP.K x)
      $ composeTxOutTranslationPairs translateTxOut

-- Note that this is a HardForkBlock instance, but it's not compositional. This
-- is because the LedgerTables relies on knowledge specific to Cardano and we
-- have so far not found a pleasant way to express that compositionally.
instance CardanoHardForkConstraints c => TableStuff (LedgerState (CardanoBlock c)) where
  -- TODO Right now, this definition corresponds to what is on the disk. It
  -- might be possible to instead have this type be the Shelley table for the
  -- latest era, ie a sum of maps instead of a map of sums. I'm not sure the
  -- trade-offs.
  newtype LedgerTables (LedgerState (CardanoBlock c)) mk = CardanoLedgerTables {
        cardanoUTxOTable :: ApplyMapKind mk (SL.TxIn c) (CardanoTxOut c)
      }
    deriving (Generic)

  projectLedgerTables (HardForkLedgerState hfstate) =
      projectLedgerTablesHelper
        (projectLedgerTables . unFlip)
        hfstate

  withLedgerTables (HardForkLedgerState hfstate) tables =
        HardForkLedgerState
      $ withLedgerTablesHelper
          (\(Flip st) tables' -> Flip $ withLedgerTables st tables')
          hfstate
          tables

  pureLedgerTables     f                                                 = CardanoLedgerTables f
  mapLedgerTables      f                         (CardanoLedgerTables x) = CardanoLedgerTables (f x)
  traverseLedgerTables f                         (CardanoLedgerTables x) = CardanoLedgerTables <$> f x
  zipLedgerTables      f (CardanoLedgerTables l) (CardanoLedgerTables r) = CardanoLedgerTables (f l r)
  foldLedgerTables     f                         (CardanoLedgerTables x) = f x
  foldLedgerTables2    f (CardanoLedgerTables l) (CardanoLedgerTables r) = f l r

instance CardanoHardForkConstraints c
      => SufficientSerializationForAnyBackingStore (LedgerState (CardanoBlock c)) where
    codecLedgerTables = CardanoLedgerTables (CodecMK toCBOR toCBOR fromCBOR fromCBOR)

deriving newtype instance PraosCrypto c => Eq (LedgerTables (LedgerState (CardanoBlock c)) EmptyMK)
deriving newtype instance PraosCrypto c => Eq (LedgerTables (LedgerState (CardanoBlock c)) ValuesMK)
deriving newtype instance PraosCrypto c => Eq (LedgerTables (LedgerState (CardanoBlock c)) DiffMK)

deriving newtype instance PraosCrypto c => NoThunks (LedgerTables (LedgerState (CardanoBlock c)) EmptyMK)
deriving newtype instance PraosCrypto c => NoThunks (LedgerTables (LedgerState (CardanoBlock c)) ValuesMK)
deriving newtype instance PraosCrypto c => NoThunks (LedgerTables (LedgerState (CardanoBlock c)) SeqDiffMK)

instance PraosCrypto c => ShowLedgerState (LedgerTables (LedgerState (CardanoBlock c))) where
  showsLedgerState _mk (CardanoLedgerTables utxo) =
        showParen True
      $ showString "CardanoLedgerTables " . showsApplyMapKind utxo

-- | Auxiliary for convenience
--
-- We can't reuse 'Translate' because we don't have the 'EpochNo' it requires.
newtype TranslateTxOutWrapper era1 era2 =
    TranslateTxOutWrapper (Core.TxOut era1 -> Core.TxOut era2)

-- | The 'Core.TxOut' translations between the adjacent Shelley-based eras in
-- Cardano
translateTxOut ::
     CardanoHardForkConstraints c
  => InPairs
       TranslateTxOutWrapper
       (ShelleyBasedEras c)
translateTxOut =
      PCons (TranslateTxOutWrapper $ SL.translateEra' ())
    $ PCons (TranslateTxOutWrapper $ SL.translateEra' ())
    $ PCons (TranslateTxOutWrapper $ Alonzo.translateTxOut)
    $ PNil

-- | Auxiliary @SOP@ combinator
--
-- WARNING: The functions in the result fail if the 'NS' argument's tag
-- precedes the 'NP' index.
--
-- TODO use an accumulator instead of this quadratic traversal
composeTxOutTranslationPairs ::
     (SOP.SListI eras, HasCallStack)
  => InPairs
       TranslateTxOutWrapper
       eras
  -> NP
       (SOP.K (NS TxOutWrapper eras) -.-> TxOutWrapper)
       eras
composeTxOutTranslationPairs = \case
    PNil                                  ->
      (SOP.fn $ SOP.unZ . SOP.unK) :* Nil
    PCons (TranslateTxOutWrapper f) inner ->
         (SOP.fn $
            eitherNS
              id
              (error "composeTxOutTranslationPairs: anachrony")
          . SOP.unK
         )
      :* SOP.hmap
          (\innerf -> SOP.fn $
              SOP.apFn innerf
            . SOP.K
            . eitherNS
                (Z . TxOutWrapper . f . unTxOutWrapper)
                id
            . SOP.unK)
          (composeTxOutTranslationPairs inner)
  where
    eitherNS :: (f x -> c) -> (NS f xs -> c) -> NS f (x ': xs) -> c
    eitherNS l r = \case
      Z x -> l x
      S x -> r x

-- | Auxiliary alias for convenenience
class    (ShelleyBasedEra era, SL.Crypto era ~ c) => CardanoShelleyBasedEra c era
instance (ShelleyBasedEra era, SL.Crypto era ~ c) => CardanoShelleyBasedEra c era

-- | Auxiliary; see 'ShelleyTele'
type family MapShelleyBlock (eras :: [Type]) = (blks :: [Type]) | blks -> eras where
  MapShelleyBlock '[]           = '[]
  MapShelleyBlock (era ': eras) = ShelleyBlock era ': MapShelleyBlock eras

-- | Auxiliary; relates a telescope with a list of indices that are all
-- 'ShelleyBlock' applications to a telescope in which the common structure is
-- captured in the telescope's non-index arguments
--
-- This is not /necessarily/ required, but it lets us use 'ShelleyBasedEras'
-- elsewhere in this module, which is appealing.
class IsShelleyTele eras where
  consolidateShelleyTele ::
       Telescope  g                    f                   (MapShelleyBlock eras)
    -> Telescope (g :.: ShelleyBlock) (f :.: ShelleyBlock)                  eras
  unconsolidateShelleyTele ::
       Telescope (g :.: ShelleyBlock) (f :.: ShelleyBlock)                  eras
    -> Telescope  g                    f                   (MapShelleyBlock eras)

consolidateShelleyNS ::
     IsShelleyTele eras
  => NS f (MapShelleyBlock eras) -> NS (f :.: ShelleyBlock) eras
consolidateShelleyNS =
    Telescope.tip . consolidateShelleyTele . Telescope.fromTip

instance IsShelleyTele '[] where
  consolidateShelleyTele   = \case {}
  unconsolidateShelleyTele = \case {}

instance IsShelleyTele xs => IsShelleyTele (x ': xs) where
  consolidateShelleyTele = \case
    TZ x       -> TZ (Comp x)
    TS p inner -> TS (Comp p) (consolidateShelleyTele inner)
  unconsolidateShelleyTele   = \case
    TZ (Comp x)       -> TZ x
    TS (Comp p) inner -> TS p (unconsolidateShelleyTele inner)

instance CardanoHardForkConstraints c => TickedTableStuff (LedgerState (CardanoBlock c)) where
  projectLedgerTablesTicked st =
      projectLedgerTablesHelper
        (\(FlipTickedLedgerState st') -> projectLedgerTablesTicked st')
        (tickedHardForkLedgerStatePerEra st)
  withLedgerTablesTicked TickedHardForkLedgerState{..} tables =
      TickedHardForkLedgerState {
          tickedHardForkLedgerStateTransition = tickedHardForkLedgerStateTransition
        , tickedHardForkLedgerStatePerEra     =
            withLedgerTablesHelper
              (\(FlipTickedLedgerState st) tables' ->
                 FlipTickedLedgerState $ withLedgerTablesTicked st tables')
              tickedHardForkLedgerStatePerEra
              tables
        }

instance CardanoHardForkConstraints c => LedgerTablesCanHardFork (CardanoEras c) where
  hardForkInjectLedgerTablesKeysMK =
         byron
      :* shelley IZ
      :* shelley (IS IZ)
      :* shelley (IS (IS IZ))
      :* shelley (IS (IS (IS IZ)))
      :* Nil
    where
      byron :: InjectLedgerTables (CardanoEras c) ByronBlock
      byron = InjectLedgerTables $ \NoByronLedgerTables -> polyEmptyLedgerTables

      shelley ::
           forall era. CardanoShelleyBasedEra c era
        => SOP.Index (ShelleyBasedEras c) era
        -> InjectLedgerTables (CardanoEras c) (ShelleyBlock era)
      shelley idx =
          InjectLedgerTables
        $ \(ShelleyLedgerTables lt) -> CardanoLedgerTables $ mapValuesAppliedMK f lt
        where
          f :: Core.TxOut era -> CardanoTxOut c
          f = ShelleyTxOut . SOP.injectNS idx . TxOutWrapper

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

translateLedgerStateByronToShelleyWrapper :: forall c.
     ( ShelleyBasedEra (ShelleyEra c)
     , HASH     c ~ Blake2b_256
     , ADDRHASH c ~ Blake2b_224
     )
  => RequiringBoth
       WrapLedgerConfig
       TranslateLedgerState
       ByronBlock
       (ShelleyBlock (ShelleyEra c))
translateLedgerStateByronToShelleyWrapper =
    RequireBoth $ \_ (WrapLedgerConfig cfgShelley) ->
    TranslateLedgerState $ \epochNo ledgerByron ->
      -- TODO yuck! this fixup step is too clever
      (\x -> case sMapKind' ledgerByron of
          SValuesMK -> unstowLedgerTables x
          _         -> unstowLedgerTables x `withLedgerTables` polyEmptyLedgerTables) $
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
      , shelleyLedgerTables = translateNoTables (projectLedgerTables ledgerByron)
      }
  where
    translateNoTables ::
         LedgerTables (LedgerState ByronBlock)                    mk
      -> LedgerTables (LedgerState (ShelleyBlock (ShelleyEra c))) EmptyMK
    translateNoTables NoByronLedgerTables = emptyLedgerTables

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
      -> LedgerState ByronBlock mk
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
  Translation from Shelley to Allegra
-------------------------------------------------------------------------------}

translateLedgerStateShelleyToAllegraWrapper ::
     PraosCrypto c
  => RequiringBoth
       WrapLedgerConfig
       TranslateLedgerState
       (ShelleyBlock (ShelleyEra c))
       (ShelleyBlock (AllegraEra c))
translateLedgerStateShelleyToAllegraWrapper =
    ignoringBoth $
      TranslateLedgerState $ \_epochNo ->
        unFlip . unComp . SL.translateEra' () . Comp . Flip

translateTxShelleyToAllegraWrapper ::
     PraosCrypto c
  => InjectTx
       (ShelleyBlock (ShelleyEra c))
       (ShelleyBlock (AllegraEra c))
translateTxShelleyToAllegraWrapper = InjectTx $
    fmap unComp . eitherToMaybe . runExcept . SL.translateEra () . Comp

translateValidatedTxShelleyToAllegraWrapper ::
     PraosCrypto c
  => InjectValidatedTx
       (ShelleyBlock (ShelleyEra c))
       (ShelleyBlock (AllegraEra c))
translateValidatedTxShelleyToAllegraWrapper = InjectValidatedTx $
    fmap unComp . eitherToMaybe . runExcept . SL.translateEra () . Comp

{-------------------------------------------------------------------------------
  Translation from Shelley to Allegra
-------------------------------------------------------------------------------}

translateLedgerStateAllegraToMaryWrapper ::
     PraosCrypto c
  => RequiringBoth
       WrapLedgerConfig
       TranslateLedgerState
       (ShelleyBlock (AllegraEra c))
       (ShelleyBlock (MaryEra c))
translateLedgerStateAllegraToMaryWrapper =
    ignoringBoth $
      TranslateLedgerState $ \_epochNo ->
        unFlip . unComp . SL.translateEra' () . Comp . Flip

{-------------------------------------------------------------------------------
  Translation from Allegra to Mary
-------------------------------------------------------------------------------}

translateTxAllegraToMaryWrapper ::
     PraosCrypto c
  => InjectTx
       (ShelleyBlock (AllegraEra c))
       (ShelleyBlock (MaryEra c))
translateTxAllegraToMaryWrapper = InjectTx $
    fmap unComp . eitherToMaybe . runExcept . SL.translateEra () . Comp

translateValidatedTxAllegraToMaryWrapper ::
     PraosCrypto c
  => InjectValidatedTx
       (ShelleyBlock (AllegraEra c))
       (ShelleyBlock (MaryEra c))
translateValidatedTxAllegraToMaryWrapper = InjectValidatedTx $
    fmap unComp . eitherToMaybe . runExcept . SL.translateEra () . Comp

{-------------------------------------------------------------------------------
  Translation from Mary to Alonzo
-------------------------------------------------------------------------------}

translateLedgerStateMaryToAlonzoWrapper ::
     PraosCrypto c
  => RequiringBoth
       WrapLedgerConfig
       TranslateLedgerState
       (ShelleyBlock (MaryEra c))
       (ShelleyBlock (AlonzoEra c))
translateLedgerStateMaryToAlonzoWrapper =
    RequireBoth $ \_cfgMary cfgAlonzo ->
      TranslateLedgerState $ \_epochNo ->
        unFlip . unComp . SL.translateEra' (getAlonzoTranslationContext cfgAlonzo) . Comp . Flip

getAlonzoTranslationContext ::
     WrapLedgerConfig (ShelleyBlock (AlonzoEra c))
  -> Alonzo.AlonzoGenesis
getAlonzoTranslationContext =
    shelleyLedgerTranslationContext . unwrapLedgerConfig

translateTxMaryToAlonzoWrapper ::
     PraosCrypto c
  => Alonzo.AlonzoGenesis
  -> InjectTx
       (ShelleyBlock (MaryEra c))
       (ShelleyBlock (AlonzoEra c))
translateTxMaryToAlonzoWrapper ctxt = InjectTx $
    fmap unComp . eitherToMaybe . runExcept . SL.translateEra ctxt . Comp

translateValidatedTxMaryToAlonzoWrapper ::
     forall c.
     PraosCrypto c
  => Alonzo.AlonzoGenesis
  -> InjectValidatedTx
       (ShelleyBlock (MaryEra c))
       (ShelleyBlock (AlonzoEra c))
translateValidatedTxMaryToAlonzoWrapper ctxt = InjectValidatedTx $
    fmap unComp . eitherToMaybe . runExcept . SL.translateEra ctxt . Comp
