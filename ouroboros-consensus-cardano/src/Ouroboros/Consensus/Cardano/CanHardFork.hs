{- HLINT ignore -}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE EmptyCase                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE StandaloneKindSignatures   #-}
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
  ) where

import           Control.Monad
import           Control.Monad.Except (runExcept, throwError)
import           Data.Coerce (coerce)
import           Data.Kind (Type)
import qualified Data.Map.Strict as Map
import           Data.Maybe (listToMaybe, mapMaybe)
import           Data.Proxy
import           Data.SOP.Strict
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
import           Ouroboros.Consensus.Ledger.Tables
import           Ouroboros.Consensus.Ledger.Tables.Utils
import qualified Ouroboros.Consensus.Storage.LedgerDB.HD.DiffSeq as DS
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util (eitherToMaybe)
import           Ouroboros.Consensus.Util.RedundantConstraints
import           Ouroboros.Consensus.Util.SOP

import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.State.Types
import           Ouroboros.Consensus.HardFork.Combinator.Util.Functors
                     (Flip (..))
import           Ouroboros.Consensus.HardFork.Combinator.Util.InPairs
                     (RequiringBoth (..), ignoringBoth)
import           Ouroboros.Consensus.HardFork.Combinator.Util.Tails (Tails (..))
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Tails as Tails
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Telescope as Telescope

import           Ouroboros.Consensus.Byron.Ledger
import qualified Ouroboros.Consensus.Byron.Ledger.Inspect as Byron.Inspect
import           Ouroboros.Consensus.Byron.Node ()
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.PBFT (PBft, PBftCrypto)
import           Ouroboros.Consensus.Protocol.PBFT.State (PBftState)
import qualified Ouroboros.Consensus.Protocol.PBFT.State as PBftState
import           Ouroboros.Consensus.Protocol.TPraos

import           Ouroboros.Consensus.Shelley.Ledger
import           Ouroboros.Consensus.Shelley.Node ()
import           Ouroboros.Consensus.Shelley.ShelleyHFC

import           Cardano.Ledger.Allegra.Translation
                     (shelleyToAllegraAVVMsToDelete)
import qualified Cardano.Ledger.Alonzo.Genesis as Alonzo
import qualified Cardano.Ledger.Alonzo.Translation as Alonzo
import qualified Cardano.Ledger.Babbage.Translation as Babbage
import qualified Cardano.Ledger.Core as Core
import           Cardano.Ledger.Crypto (ADDRHASH, DSIGN, HASH)
import qualified Cardano.Ledger.Era as SL
import           Cardano.Ledger.Hashes (EraIndependentTxBody)
import           Cardano.Ledger.Keys (DSignable, Hash)
import           Cardano.Ledger.Mary.Translation ()
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Protocol.TPraos.API as SL
import qualified Cardano.Protocol.TPraos.Rules.Prtcl as SL
import qualified Cardano.Protocol.TPraos.Rules.Tickn as SL

import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Ledger.SupportsProtocol
                     (LedgerSupportsProtocol)
import           Ouroboros.Consensus.Protocol.Praos (Praos)
import qualified Ouroboros.Consensus.Protocol.Praos as Praos
import qualified Ouroboros.Consensus.Protocol.TPraos as TPraos
import           Ouroboros.Consensus.Protocol.Translate (TranslateProto)
import           Ouroboros.Consensus.Shelley.Protocol.Praos ()

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
  ( TPraos.PraosCrypto c
  , Praos.PraosCrypto c
  , TranslateProto (TPraos c) (Praos c)
  , ShelleyCompatible (TPraos c) (ShelleyEra c)
  , LedgerSupportsProtocol (ShelleyBlock (TPraos c) (ShelleyEra c))
  , ShelleyCompatible (TPraos c) (AllegraEra c)
  , LedgerSupportsProtocol (ShelleyBlock (TPraos c) (AllegraEra c))
  , ShelleyCompatible (TPraos c) (MaryEra    c)
  , LedgerSupportsProtocol (ShelleyBlock (TPraos c) (MaryEra c))
  , ShelleyCompatible (TPraos c) (AlonzoEra  c)
  , LedgerSupportsProtocol (ShelleyBlock (TPraos c) (AlonzoEra c))
  , ShelleyCompatible (Praos c) (BabbageEra  c)
  , LedgerSupportsProtocol (ShelleyBlock (Praos c) (BabbageEra c))
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
        $ PCons translateLedgerStateAlonzoToBabbageWrapper
        $ PNil
    , translateChainDepState =
          PCons translateChainDepStateByronToShelleyWrapper
        $ PCons translateChainDepStateAcrossShelley
        $ PCons translateChainDepStateAcrossShelley
        $ PCons translateChainDepStateAcrossShelley
        $ PCons translateChainDepStateAcrossShelley
        $ PNil
    , translateLedgerView    =
          PCons translateLedgerViewByronToShelleyWrapper
        $ PCons translateLedgerViewAcrossShelley
        $ PCons translateLedgerViewAcrossShelley
        $ PCons translateLedgerViewAcrossShelley
        $ PCons translateLedgerViewAcrossShelley
        $ PNil
    }
  hardForkChainSel =
        -- Byron <-> Shelley, ...
        TCons (hpure CompareBlockNo)
        -- Inter-Shelley-based
      $ Tails.hcpure (Proxy @(HasPraosSelectView c)) CompareSameSelectView
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
      $ PCons (RequireBoth $ \_cfgAlonzo cfgBabbage ->
                let ctxt = getBabbageTranslationContext cfgBabbage
                in
                Pair2
                  (translateTxAlonzoToBabbageWrapper          ctxt)
                  (translateValidatedTxAlonzoToBabbageWrapper ctxt)
              )
      $ PNil

class    (SelectView (BlockProtocol blk) ~ PraosChainSelectView c) => HasPraosSelectView c blk
instance (SelectView (BlockProtocol blk) ~ PraosChainSelectView c) => HasPraosSelectView c blk

{-------------------------------------------------------------------------------
  TableStuff
-------------------------------------------------------------------------------}

type CardanoTxOut c = ShelleyTxOut (ShelleyBasedEras c)

type MapShelleyBlock :: [(Type, Type)] -> [Type]
type family MapShelleyBlock protosAndEras = blks | blks -> protosAndEras where
  MapShelleyBlock '[]                              = '[]
  MapShelleyBlock ('(proto, era) ': protosAndEras') = ShelleyBlock proto era ': MapShelleyBlock protosAndEras'

class IsShelleyTele protosAndEras where
  consolidateShelleyTele ::
       Telescope              g                            f               (MapShelleyBlock protosAndEras)
    -> Telescope (UncurryComp g ShelleyBlock) (UncurryComp f ShelleyBlock) protosAndEras
  unconsolidateShelleyTele ::
       Telescope (UncurryComp g ShelleyBlock) (UncurryComp f ShelleyBlock) protosAndEras
    -> Telescope              g                            f               (MapShelleyBlock protosAndEras)

instance IsShelleyTele '[] where
  consolidateShelleyTele   = \case {}
  unconsolidateShelleyTele = \case {}

instance IsShelleyTele xs => IsShelleyTele ('(x, y) ': xs) where
  consolidateShelleyTele = \case
    TZ x       -> TZ (UncurryComp x)
    TS p inner -> TS (UncurryComp p) (consolidateShelleyTele inner)
  unconsolidateShelleyTele   = \case
    TZ (UncurryComp x)       -> TZ x
    TS (UncurryComp p) inner -> TS p (unconsolidateShelleyTele inner)

-- We reuse this for both TableStuff and TickedTableStuff instances, so the
-- @TickedTableStuff x@ constraint here is excessive in the TableStuff case.
-- However, since x is always a Cardano era, we do know we have
-- TickedTableStuff for every x, so hardcoding the stronger constraint is
-- easier than parameterizing this helper over the constraint.
projectLedgerTablesHelper :: forall c mk fmk.
     (CardanoHardForkConstraints c, IsMapKind mk)
  => (forall blk.
         TickedTableStuff (LedgerState blk)
      => fmk blk -> LedgerTables (LedgerState blk) mk
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
          hcollapse
        $ hcimap
            (Proxy @(SndShelleyBasedEra c))
            projectOne
            (Telescope.tip $ consolidateShelleyTele tele)
  where
    projectOne :: forall (a :: (Type, Type)).
         (SL.Crypto (Snd a) ~ c, ShelleyBasedEra (Snd a))
      => Index (ShelleyBasedProtosAndEras c)             a
         -- ^ the current era of the ledger state we're projecting from
      -> UncurryComp (Current fmk) ShelleyBlock a
         -- ^ the ledger state we're projecting from
      -> K (LedgerTables (LedgerState (CardanoBlock c)) mk) a
    projectOne idx (UncurryComp current) =
        K $ CardanoLedgerTables $ inj appliedMK
      where
        ShelleyLedgerTables appliedMK = prjLT $ currentState current

        inj ::
             mk (SL.TxIn c) (Core.TxOut (Snd a))
          -> mk (SL.TxIn c) (CardanoTxOut c)
        inj = mapMK (ShelleyTxOut . injectNS (castSndIdx idx) . TxOutWrapper)

class (SL.Crypto (Snd a) ~ c, ShelleyBasedEra (Snd a)) => SndShelleyBasedEra c a
instance (SL.Crypto (Snd a) ~ c, ShelleyBasedEra (Snd a)) => SndShelleyBasedEra c a

-- Same note regarding the @TickedTableStuff x@ constraint as
-- 'projectLedgerTablesHelper'
withLedgerTablesHelper ::
  forall c mk fany fmk.
     (CardanoHardForkConstraints c, IsMapKind mk)
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
          $ hap
              updateOne
              (consolidateShelleyTele tele)
  where
    -- how to update the ledger table of each possible individual era
    updateOne ::
      NP
        (     UncurryComp (Current fany) ShelleyBlock
         -.-> UncurryComp (Current fmk ) ShelleyBlock
        )
        (ShelleyBasedProtosAndEras c)
    updateOne =
      hcmap
        (Proxy @(SndShelleyBasedEra c))
        (\(ApOnlySnd translate) -> fn $ \(UncurryComp current) ->
            let Current{currentState = innerSt} = current
                newInnerSt =
                  withLT innerSt
                  $ ShelleyLedgerTables
                  $ mapMK
                      (unTxOutWrapper . apFn translate . K)
                      appliedMK
            in UncurryComp $ current{currentState = newInnerSt})
        translations

    -- the composed translations for each possible era; see
    -- 'composeTxOutTranslationPairs' to understand why this is partial but
    -- is safe in the absence of Consensus bugs
    translations ::
      NP
        (ApOnlySnd (K (CardanoTxOut c) -.-> TxOutWrapper))
        (ShelleyBasedProtosAndEras c)
    translations =
        hmap
          (\(ApOnlySnd f)
             -> ApOnlySnd $ fn $ \(K (ShelleyTxOut x)) -> f `apFn` K (nsMapSnd x))
          (composeTxOutTranslationPairs translateTxOut)

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

  pureLedgerTables     f                                                                         = CardanoLedgerTables f
  mapLedgerTables      f                                                 (CardanoLedgerTables x) = CardanoLedgerTables (f x)
  traverseLedgerTables f                                                 (CardanoLedgerTables x) = CardanoLedgerTables <$> f x
  zipLedgerTables      f                         (CardanoLedgerTables l) (CardanoLedgerTables r) = CardanoLedgerTables (f l r)
  zipLedgerTables2     f (CardanoLedgerTables l) (CardanoLedgerTables c) (CardanoLedgerTables r) = CardanoLedgerTables (f l c r)
  zipLedgerTablesA     f                         (CardanoLedgerTables l) (CardanoLedgerTables r) = CardanoLedgerTables <$> f l r
  zipLedgerTables2A    f (CardanoLedgerTables l) (CardanoLedgerTables c) (CardanoLedgerTables r) = CardanoLedgerTables <$> f l c r
  foldLedgerTables     f                                                 (CardanoLedgerTables x) = f x
  foldLedgerTables2    f                         (CardanoLedgerTables l) (CardanoLedgerTables r) = f l r

  namesLedgerTables = CardanoLedgerTables { cardanoUTxOTable = NameMK "cardanoUTxOTable" }

instance CardanoHardForkConstraints c
      => SufficientSerializationForAnyBackingStore (LedgerState (CardanoBlock c)) where
    codecLedgerTables = CardanoLedgerTables (CodecMK toCBOR toCBOR fromCBOR fromCBOR)

deriving newtype instance (Praos.PraosCrypto c, TPraos.PraosCrypto c, DSignable c (Hash c EraIndependentTxBody)) => Eq (LedgerTables (LedgerState (CardanoBlock c)) EmptyMK)
deriving newtype instance (Praos.PraosCrypto c, TPraos.PraosCrypto c, DSignable c (Hash c EraIndependentTxBody)) => Eq (LedgerTables (LedgerState (CardanoBlock c)) ValuesMK)
deriving newtype instance (Praos.PraosCrypto c, TPraos.PraosCrypto c, DSignable c (Hash c EraIndependentTxBody)) => Eq (LedgerTables (LedgerState (CardanoBlock c)) DiffMK)

deriving newtype instance (Praos.PraosCrypto c, TPraos.PraosCrypto c, DSignable c (Hash c EraIndependentTxBody)) => NoThunks (LedgerTables (LedgerState (CardanoBlock c)) EmptyMK)
deriving newtype instance (Praos.PraosCrypto c, TPraos.PraosCrypto c, DSignable c (Hash c EraIndependentTxBody)) => NoThunks (LedgerTables (LedgerState (CardanoBlock c)) ValuesMK)
deriving newtype instance (Praos.PraosCrypto c, TPraos.PraosCrypto c, DSignable c (Hash c EraIndependentTxBody)) => NoThunks (LedgerTables (LedgerState (CardanoBlock c)) SeqDiffMK)

instance (TPraos.PraosCrypto c, Praos.PraosCrypto c, DSignable c (Hash c EraIndependentTxBody)) => ShowLedgerState (LedgerTables (LedgerState (CardanoBlock c))) where
  showsLedgerState (CardanoLedgerTables utxo) =
        showParen True
      $ showString "CardanoLedgerTables " . showsApplyMapKind utxo

-- | Auxiliary for convenience
--
-- We can't reuse 'Translate' because we don't have the 'EpochNo' it requires.
newtype TranslateTxOutWrapper era1 era2 =
    TranslateTxOutWrapper (Core.TxOut era1 -> Core.TxOut era2)

-- newtype CurryComp (t1 :: (Type, Type)) (t2 :: (Type, Type)) =
--   CurryComp (TranslateTxOutWrapper ())

-- | The 'Core.TxOut' translations between the adjacent Shelley-based eras in
-- Cardano
translateTxOut ::
     CardanoHardForkConstraints c
  => InPairs
       (ApOnlySnd2 TranslateTxOutWrapper)
       (ShelleyBasedProtosAndEras c)
translateTxOut =
      PCons (ApOnlySnd2 $ TranslateTxOutWrapper $ SL.translateEra' ())
    $ PCons (ApOnlySnd2 $ TranslateTxOutWrapper $ SL.translateEra' ())
    $ PCons (ApOnlySnd2 $ TranslateTxOutWrapper $ Alonzo.translateTxOut)
    $ PCons (ApOnlySnd2 $ TranslateTxOutWrapper $ Babbage.translateTxOut)
    $ PNil

-- | Auxiliary @SOP@ combinator
--
-- WARNING: The functions in the result fail if the 'NS' argument's tag
-- precedes the 'NP' index.
--
-- TODO use an accumulator instead of this quadratic traversal
composeTxOutTranslationPairs ::
     (SListI protosAndEras, HasCallStack)
  => InPairs
       (ApOnlySnd2 TranslateTxOutWrapper)
       protosAndEras
  -> NP
       (ApOnlySnd (K (NS (ApOnlySnd TxOutWrapper) protosAndEras) -.-> TxOutWrapper))
       protosAndEras
composeTxOutTranslationPairs = \case
    PNil                                  ->
      (ApOnlySnd $ fn $ unApOnlySnd . unZ . unK) :* Nil
    PCons (ApOnlySnd2 (TranslateTxOutWrapper f)) inner ->
         (ApOnlySnd $ fn $
            unApOnlySnd
          . eitherNS
              id
              (error "composeTxOutTranslationPairs: anachrony")
          . unK
         )
      :* hmap
          (\(ApOnlySnd innerf) -> ApOnlySnd $ fn $
              apFn innerf
            . K
            . eitherNS
                (Z . ApOnlySnd . TxOutWrapper . f . unTxOutWrapper . unApOnlySnd)
                id
            . unK)
          (composeTxOutTranslationPairs inner)
  where
    eitherNS :: (f x -> c) -> (NS f xs -> c) -> NS f (x ': xs) -> c
    eitherNS l r = \case
      Z x -> l x
      S x -> r x

instance CardanoHardForkConstraints c => TickedTableStuff (LedgerState (CardanoBlock c)) where
  projectLedgerTablesTicked st = projectLedgerTablesHelper
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
      :* shelley (IS (IS (IS (IS IZ))))
      :* Nil
    where
      byron :: InjectLedgerTables (CardanoEras c) ByronBlock
      byron = InjectLedgerTables $ \NoByronLedgerTables -> polyEmptyLedgerTables

      shelley ::
           forall era proto. (SL.Crypto era ~ c, Eq (Core.TxOut era))
        => Index (ShelleyBasedEras c) era
        -> InjectLedgerTables (CardanoEras c) (ShelleyBlock proto era)
      shelley idx =
          InjectLedgerTables
        $ \(ShelleyLedgerTables lt) -> CardanoLedgerTables $ mapMK f lt
        where
          f :: Core.TxOut era -> CardanoTxOut c
          f = ShelleyTxOut . injectNS idx . TxOutWrapper

{-------------------------------------------------------------------------------
  Translation from Byron to Shelley
-------------------------------------------------------------------------------}

translateHeaderHashByronToShelley ::
     forall c.
     ( ShelleyCompatible (TPraos c) (ShelleyEra c)
     , HASH c ~ Blake2b_256
     )
  => HeaderHash ByronBlock
  -> HeaderHash (ShelleyBlock (TPraos c) (ShelleyEra c))
translateHeaderHashByronToShelley =
      fromShortRawHash (Proxy @(ShelleyBlock (TPraos c) (ShelleyEra c)))
    . toShortRawHash   (Proxy @ByronBlock)
  where
    -- Byron uses 'Blake2b_256' for header hashes
    _ = keepRedundantConstraint (Proxy @(HASH c ~ Blake2b_256))

translatePointByronToShelley ::
     ( ShelleyCompatible (TPraos c) (ShelleyEra c)
     , HASH c ~ Blake2b_256
     )
  => Point ByronBlock
  -> WithOrigin BlockNo
  -> WithOrigin (ShelleyTip (TPraos c) (ShelleyEra c))
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
     ( ShelleyCompatible (TPraos c) (ShelleyEra c)
     , HASH     c ~ Blake2b_256
     , ADDRHASH c ~ Blake2b_224
     )
  => RequiringBoth
       WrapLedgerConfig
       TranslateLedgerState
       ByronBlock
       (ShelleyBlock (TPraos c) (ShelleyEra c))
translateLedgerStateByronToShelleyWrapper =
      RequireBoth
    $ \_ (WrapLedgerConfig cfgShelley) ->
        TranslateLedgerState {
            translateLedgerStateWith = \epochNo ledgerByron ->
                forgetLedgerTablesValues
              . calculateAdditions
              . unstowLedgerTables
              $ ShelleyLedgerState {
                    shelleyLedgerTip =
                      translatePointByronToShelley
                      (ledgerTipPoint ledgerByron)
                      (byronLedgerTipBlockNo ledgerByron)
                  , shelleyLedgerState =
                      SL.translateToShelleyLedgerState
                        (shelleyLedgerGenesis cfgShelley)
                        epochNo
                        (byronLedgerState ledgerByron)
                  , shelleyLedgerTransition =
                      ShelleyTransitionInfo{shelleyAfterVoting = 0}
                  , shelleyLedgerTables = polyEmptyLedgerTables
                }
          , translateLedgerTablesWith = \NoByronLedgerTables -> polyEmptyLedgerTables
          }

translateChainDepStateByronToShelleyWrapper ::
     RequiringBoth
       WrapConsensusConfig
       (Translate WrapChainDepState)
       ByronBlock
       (ShelleyBlock (TPraos c) (ShelleyEra c))
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
       (ShelleyBlock (TPraos c) (ShelleyEra c))
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
           (Ticked (WrapLedgerView (ShelleyBlock (TPraos c) (ShelleyEra c))))
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
     (PraosCrypto c, DSignable c (Hash c EraIndependentTxBody))
  => RequiringBoth
       WrapLedgerConfig
       TranslateLedgerState
       (ShelleyBlock (TPraos c) (ShelleyEra c))
       (ShelleyBlock (TPraos c) (AllegraEra c))
translateLedgerStateShelleyToAllegraWrapper =
    ignoringBoth $
      TranslateLedgerState {
          translateLedgerStateWith = \_epochNo ls ->
              -- In the Shelley to Allegra transition, the AVVM addresses have
              -- to be deleted, and their balance has to be moved to the
              -- reserves. For this matter, the Ledger keeps track of these
              -- small set of entries since the Byron to Shelley transition and
              -- provides them to us through 'shelleyToAllegraAVVMsToDelete'.
              --
              -- In the long run, the ledger will already use ledger states
              -- parametrized by the map kind and therefore will already provide
              -- the differences in this translation.
              let avvms           = SL.unUTxO (shelleyToAllegraAVVMsToDelete $ shelleyLedgerState ls)

                  -- While techically we can diff the LedgerTables, it becomes
                  -- complex doing so, as we cannot perform operations with
                  -- 'LedgerTables l1 mk' and 'LedgerTables l2 mk'. Because of
                  -- this, for now we choose to generate the differences out of
                  -- thin air and when the time comes in which ticking produces
                  -- differences, we will have to revisit this.
                  avvmsAsDeletions = ShelleyLedgerTables
                                   . ApplyDiffMK
                                   . DS.Diff
                                   . Map.map (  DS.singletonDelete
                                              . unTxOutWrapper
                                              . SL.translateEra' ()
                                              . TxOutWrapper
                                             )
                                   $ avvms

                  -- This 'stowLedgerTables' + 'withLedgerTables' injects the
                  -- values provided by the Ledger so that the translation
                  -- operation finds those entries in the UTxO and destroys
                  -- them, modifying the reserves accordingly.
                  stowedState = stowLedgerTables
                              . withLedgerTables ls
                              . ShelleyLedgerTables
                              . ApplyValuesMK
                              . DS.Values
                              $ avvms

                  resultingState = unFlip . unComp
                                 . SL.translateEra' ()
                                 . Comp   . Flip
                                 $ stowedState

              in resultingState `withLedgerTables` avvmsAsDeletions

        , translateLedgerTablesWith =
            \ShelleyLedgerTables { shelleyUTxOTable = diffMK } ->
             ShelleyLedgerTables { shelleyUTxOTable = fmap (SL.translateEra' ()) diffMK
                                 }
        }

translateTxShelleyToAllegraWrapper ::
     (PraosCrypto c, DSignable c (Hash c EraIndependentTxBody))
  => InjectTx
       (ShelleyBlock (TPraos c) (ShelleyEra c))
       (ShelleyBlock (TPraos c) (AllegraEra c))
translateTxShelleyToAllegraWrapper = InjectTx $
    fmap unComp . eitherToMaybe . runExcept . SL.translateEra () . Comp

translateValidatedTxShelleyToAllegraWrapper ::
     (PraosCrypto c, DSignable c (Hash c EraIndependentTxBody))
  => InjectValidatedTx
       (ShelleyBlock (TPraos c) (ShelleyEra c))
       (ShelleyBlock (TPraos c) (AllegraEra c))
translateValidatedTxShelleyToAllegraWrapper = InjectValidatedTx $
    fmap unComp . eitherToMaybe . runExcept . SL.translateEra () . Comp

{-------------------------------------------------------------------------------
  Translation from Shelley to Allegra
-------------------------------------------------------------------------------}

translateLedgerStateAllegraToMaryWrapper ::
     (PraosCrypto c, DSignable c (Hash c EraIndependentTxBody))
  => RequiringBoth
       WrapLedgerConfig
       TranslateLedgerState
       (ShelleyBlock (TPraos c) (AllegraEra c))
       (ShelleyBlock (TPraos c) (MaryEra c))
translateLedgerStateAllegraToMaryWrapper =
    ignoringBoth $
      TranslateLedgerState {
          translateLedgerStateWith = \_epochNo ->
                noNewTickingDiffs
              . unFlip
              . unComp
              . SL.translateEra' ()
              . Comp
              . Flip
        , translateLedgerTablesWith =
            \ShelleyLedgerTables { shelleyUTxOTable = diffMK } ->
             ShelleyLedgerTables { shelleyUTxOTable = fmap (SL.translateEra' ()) diffMK
                                 }
        }

{-------------------------------------------------------------------------------
  Translation from Allegra to Mary
-------------------------------------------------------------------------------}

translateTxAllegraToMaryWrapper ::
     (PraosCrypto c, DSignable c (Hash c EraIndependentTxBody))
  => InjectTx
       (ShelleyBlock (TPraos c) (AllegraEra c))
       (ShelleyBlock (TPraos c) (MaryEra c))
translateTxAllegraToMaryWrapper = InjectTx $
    fmap unComp . eitherToMaybe . runExcept . SL.translateEra () . Comp

translateValidatedTxAllegraToMaryWrapper ::
     (PraosCrypto c, DSignable c (Hash c EraIndependentTxBody))
  => InjectValidatedTx
       (ShelleyBlock (TPraos c) (AllegraEra c))
       (ShelleyBlock (TPraos c) (MaryEra c))
translateValidatedTxAllegraToMaryWrapper = InjectValidatedTx $
    fmap unComp . eitherToMaybe . runExcept . SL.translateEra () . Comp

{-------------------------------------------------------------------------------
  Translation from Mary to Alonzo
-------------------------------------------------------------------------------}

translateLedgerStateMaryToAlonzoWrapper ::
     (PraosCrypto c, DSignable c (Hash c EraIndependentTxBody))
  => RequiringBoth
       WrapLedgerConfig
       TranslateLedgerState
       (ShelleyBlock (TPraos c) (MaryEra c))
       (ShelleyBlock (TPraos c) (AlonzoEra c))
translateLedgerStateMaryToAlonzoWrapper =
    RequireBoth $ \_cfgMary cfgAlonzo ->
      TranslateLedgerState {
          translateLedgerStateWith = \_epochNo ->
                noNewTickingDiffs
              . unFlip
              . unComp
              . SL.translateEra' (getAlonzoTranslationContext cfgAlonzo)
              . Comp
              . Flip
        , translateLedgerTablesWith =
            \ShelleyLedgerTables { shelleyUTxOTable = diffMK } ->
             ShelleyLedgerTables { shelleyUTxOTable = fmap Alonzo.translateTxOut diffMK
                                 }
        }

getAlonzoTranslationContext ::
     WrapLedgerConfig (ShelleyBlock (TPraos c) (AlonzoEra c))
  -> Alonzo.AlonzoGenesis
getAlonzoTranslationContext =
    shelleyLedgerTranslationContext . unwrapLedgerConfig

translateTxMaryToAlonzoWrapper ::
     (PraosCrypto c, DSignable c (Hash c EraIndependentTxBody))
  => Alonzo.AlonzoGenesis
  -> InjectTx
       (ShelleyBlock (TPraos c) (MaryEra c))
       (ShelleyBlock (TPraos c) (AlonzoEra c))
translateTxMaryToAlonzoWrapper ctxt = InjectTx $
    fmap unComp . eitherToMaybe . runExcept . SL.translateEra ctxt . Comp

translateValidatedTxMaryToAlonzoWrapper ::
     forall c.
     (PraosCrypto c, DSignable c (Hash c EraIndependentTxBody))
  => Alonzo.AlonzoGenesis
  -> InjectValidatedTx
       (ShelleyBlock (TPraos c) (MaryEra c))
       (ShelleyBlock (TPraos c) (AlonzoEra c))
translateValidatedTxMaryToAlonzoWrapper ctxt = InjectValidatedTx $
    fmap unComp . eitherToMaybe . runExcept . SL.translateEra ctxt . Comp

{-------------------------------------------------------------------------------
  Translation from Alonzo to Babbage
-------------------------------------------------------------------------------}

translateLedgerStateAlonzoToBabbageWrapper ::
     (Praos.PraosCrypto c, TPraos.PraosCrypto c)
  => RequiringBoth
       WrapLedgerConfig
       TranslateLedgerState
       (ShelleyBlock (TPraos c) (AlonzoEra c))
       (ShelleyBlock (Praos c) (BabbageEra c))
translateLedgerStateAlonzoToBabbageWrapper =
  RequireBoth $ \_cfgMary cfgBabbage ->
      TranslateLedgerState {
          translateLedgerStateWith = \_epochNo ->
                noNewTickingDiffs
              . unFlip
              . unComp
              . SL.translateEra' (getBabbageTranslationContext cfgBabbage)
              . Comp
              . Flip
              . transPraosLS
        , translateLedgerTablesWith =
            \ShelleyLedgerTables { shelleyUTxOTable = diffMK } ->
             ShelleyLedgerTables { shelleyUTxOTable = fmap Babbage.translateTxOut diffMK
                                 }
        }
  where
    transPraosLS ::
      LedgerState (ShelleyBlock (TPraos c) (AlonzoEra c)) mk ->
      LedgerState (ShelleyBlock (Praos c)  (AlonzoEra c)) mk
    transPraosLS (ShelleyLedgerState wo nes st tb) =
      ShelleyLedgerState
        { shelleyLedgerTip        = fmap castShelleyTip wo
        , shelleyLedgerState      = nes
        , shelleyLedgerTransition = st
        , shelleyLedgerTables     = coerce tb
        }

getBabbageTranslationContext ::
     WrapLedgerConfig (ShelleyBlock (Praos c) (BabbageEra c))
  -> Alonzo.AlonzoGenesis
getBabbageTranslationContext =
    shelleyLedgerTranslationContext . unwrapLedgerConfig

translateTxAlonzoToBabbageWrapper ::
     (Praos.PraosCrypto c)
  => Alonzo.AlonzoGenesis
  -> InjectTx
       (ShelleyBlock (TPraos c) (AlonzoEra c))
       (ShelleyBlock (Praos c) (BabbageEra c))
translateTxAlonzoToBabbageWrapper ctxt = InjectTx $
    fmap unComp . eitherToMaybe . runExcept . SL.translateEra ctxt . Comp . transPraosTx
  where
    transPraosTx
      :: GenTx (ShelleyBlock (TPraos c) (AlonzoEra c))
      -> GenTx (ShelleyBlock (Praos c) (AlonzoEra c))
    transPraosTx (ShelleyTx ti tx) = ShelleyTx ti (coerce tx)

translateValidatedTxAlonzoToBabbageWrapper ::
     forall c.
     (Praos.PraosCrypto c)
  => Alonzo.AlonzoGenesis
  -> InjectValidatedTx
       (ShelleyBlock (TPraos c) (AlonzoEra c))
       (ShelleyBlock (Praos c) (BabbageEra c))
translateValidatedTxAlonzoToBabbageWrapper ctxt = InjectValidatedTx $
  fmap unComp
    . eitherToMaybe
    . runExcept
    . SL.translateEra ctxt
    . Comp
    . transPraosValidatedTx
 where
  transPraosValidatedTx
    :: WrapValidatedGenTx (ShelleyBlock (TPraos c) (AlonzoEra c))
    -> WrapValidatedGenTx (ShelleyBlock (Praos c) (AlonzoEra c))
  transPraosValidatedTx (WrapValidatedGenTx x) = case x of
    ShelleyValidatedTx txid vtx -> WrapValidatedGenTx $
      ShelleyValidatedTx txid (SL.coerceValidated vtx)
