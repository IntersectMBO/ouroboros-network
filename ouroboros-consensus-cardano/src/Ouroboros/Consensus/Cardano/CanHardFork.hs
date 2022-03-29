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
import           Data.SOP.Strict (NP (..), NS (..), unComp,
                     (:.:) (..))
import qualified Data.SOP.Strict as SOP
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
import           Cardano.Ledger.Crypto (ADDRHASH, DSIGN, HASH)
import qualified Cardano.Ledger.Era as SL
import           Cardano.Ledger.Mary.Translation ()
import qualified Cardano.Ledger.Shelley.API as SL

import           Ouroboros.Consensus.Cardano.Block

import qualified Ouroboros.Consensus.HardFork.Combinator.Util.InPairs as InPairs
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
  , SOP.All (SOP.Compose (SOP.Compose TableStuff LedgerTables) LedgerState) (CardanoEras c)
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

-- Note that this is a HardForkBlock instance, but it's not compositional. This
-- is because the LedgerTables relies on knowledge specific to Cardano and we
-- have so far not found a pleasant way to express that compositionally.
instance CardanoHardForkConstraints c => TableStuff (LedgerState (CardanoBlock c)) where
  newtype LedgerTables (LedgerState (CardanoBlock c)) mk = CardanoLedgerTables {
        cardanoUTxOTable :: NS (Flip LedgerTables mk :.: LedgerState) (CardanoEras c)
      }
    deriving (Generic)

  projectLedgerTables =
        CardanoLedgerTables
      . SOP.hcmap proxySingle ( Comp
                              . Flip
                              . projectLedgerTables
                              . unFlip
                              . currentState
                              )
      . Telescope.tip
      . getHardForkState
      . hardForkLedgerStatePerEra

  withLedgerTables (HardForkLedgerState hfstate) (CardanoLedgerTables tables) =
        HardForkLedgerState
      . HardForkState
      . f tables
      . getHardForkState
      $ hfstate
    where
      f :: (IsApplyMapKind mk, SOP.All (SOP.Compose TableStuff LedgerState) xs)
        => NS (Flip LedgerTables mk :.: LedgerState)               xs
        -> Telescope (SOP.K Past) (Current (Flip LedgerState any)) xs
        -> Telescope (SOP.K Past) (Current (Flip LedgerState mk))  xs
      f (Z (Comp tbs)) (TZ ttip) =
          TZ
        . Current (currentStart ttip)
        . Flip
        . withLedgerTables ( unFlip
                           . currentState
                           $ ttip
                           )
        . unFlip
        $ tbs
      f (S s) (TS g ts) = TS g (f s ts)
      f (S _) (TZ _)   = error "unaligned withLedgerTables"
      f (Z _) (TS _ _)   = error "unaligned withLedgerTables"

  pureLedgerTables f =
        CardanoLedgerTables
      . SOP.injectNS IZ
      . Comp
      . Flip
      $ pureLedgerTables f

  mapLedgerTables f =
      CardanoLedgerTables
    . SOP.hcmap proxySingle ( Comp
                            . Flip
                            . mapLedgerTables f
                            . unFlip
                            . unComp
                            )
    . cardanoUTxOTable

  traverseLedgerTables f =
      fmap CardanoLedgerTables
    . SOP.sequence_NS'
    . SOP.hcmap proxySingle (Comp
                            . fmap (Comp . Flip)
                            . traverseLedgerTables f
                            . unFlip
                            . unComp
                            )
    . cardanoUTxOTable

  zipLedgerTables :: forall mk1 mk2 mk3.
                     (forall k v. Ord k
                               => mk1 k v
                               -> mk2 k v
                               -> mk3 k v
                     )
                  -> LedgerTables (LedgerState (CardanoBlock c)) mk1
                  -> LedgerTables (LedgerState (CardanoBlock c)) mk2
                  -> LedgerTables (LedgerState (CardanoBlock c)) mk3
  zipLedgerTables f (CardanoLedgerTables left) (CardanoLedgerTables right) =
    case compare (SOP.index_NS left) (SOP.index_NS right) of
      LT -> CardanoLedgerTables
        . Telescope.tip
        . SOP.unI
        $ Telescope.alignExtendNS
            (InPairs.hcmap (Proxy @(SOP.Compose (SOP.Compose TableStuff LedgerTables) LedgerState)) (\ip -> Telescope.Extend $ \cur ->
                                 SOP.I $ (SOP.K (), Comp
                                           . Flip
                                           . (undefined :: LedgerTables (LedgerState x) ValuesMK
                                                        -> LedgerTables (LedgerState x) mk1)
                                           . translateLedgerTablesWith (provideBoth ip undefined undefined)
                                           . (`withLedgerTables` polyEmptyLedgerTables)
                                           . unFlip
                                           . unComp $ cur))
              translate
            )
            (SOP.hcpure proxySingle (SOP.Fn $ \(Comp (Flip r)) -> SOP.Fn $ \(Comp (Flip l)) -> Comp . Flip $ zipLedgerTables f l r))
            right
            (Telescope.fromTip left)

      EQ -> CardanoLedgerTables $ recurseSameIdx left right
      GT -> CardanoLedgerTables
        . Telescope.tip
        . SOP.unI
        $ Telescope.alignExtendNS
            (InPairs.hcmap (Proxy @(SOP.Compose (SOP.Compose TableStuff LedgerTables) LedgerState)) (\ip -> Telescope.Extend $ \cur ->
                                 SOP.I $ (SOP.K (), Comp
                                           . Flip
                                           . (undefined :: LedgerTables (LedgerState x) ValuesMK
                                                        -> LedgerTables (LedgerState x) mk2)
                                           . translateLedgerTablesWith (provideBoth ip undefined undefined)
                                           . (`withLedgerTables` polyEmptyLedgerTables)
                                           . unFlip
                                           . unComp $ cur))
              translate
            )
            (SOP.hcpure proxySingle (SOP.Fn $ \(Comp (Flip l)) -> SOP.Fn $ \(Comp (Flip r)) -> Comp . Flip $ zipLedgerTables f l r))
            left
            (Telescope.fromTip right)
    where
      recurseSameIdx :: SOP.All (SOP.Compose TableStuff LedgerState) xs
                     => NS (Flip LedgerTables mk1 :.: LedgerState) xs
                     -> NS (Flip LedgerTables mk2 :.: LedgerState) xs
                     -> NS (Flip LedgerTables mk3 :.: LedgerState) xs
      recurseSameIdx (Z l) (Z r) = Z $ Comp . Flip $ zipLedgerTables f (unFlip $ unComp l) (unFlip $ unComp r)
      recurseSameIdx (S l) (S r) = S $ recurseSameIdx l r
      recurseSameIdx _     _     = case () of

      translate :: InPairs (RequiringBoth WrapLedgerConfig TranslateLedgerState) (CardanoEras c)
      translate = translateLedgerState hardForkEraTranslation

  foldLedgerTables f =
      SOP.hcollapse
    . SOP.hcmap proxySingle ( SOP.K
                            . foldLedgerTables f
                            . unFlip
                            . unComp)
    . cardanoUTxOTable

  foldLedgerTables2 :: forall m mk1 mk2. Monoid m
    => (forall k v . Ord k
        => mk1 k v
        -> mk2 k v
        -> m
       )
    -> LedgerTables (LedgerState (CardanoBlock c)) mk1
    -> LedgerTables (LedgerState (CardanoBlock c)) mk2
    -> m
  foldLedgerTables2 f (CardanoLedgerTables left) (CardanoLedgerTables right) =
    SOP.hcollapse $
    case compare (SOP.index_NS left) (SOP.index_NS right) of
      LT ->  Telescope.tip
        . SOP.unI
        $ Telescope.alignExtendNS
            (InPairs.hcmap (Proxy @(SOP.Compose (SOP.Compose TableStuff LedgerTables) LedgerState)) (\ip -> Telescope.Extend $ \cur ->
                                 SOP.I $ (SOP.K (), Comp
                                           . Flip
                                           . (undefined :: LedgerTables (LedgerState x) ValuesMK
                                                        -> LedgerTables (LedgerState x) mk1)
                                           . translateLedgerTablesWith (provideBoth ip undefined undefined)
                                           . (`withLedgerTables` polyEmptyLedgerTables)
                                           . unFlip
                                           . unComp $ cur))
              translate
            )
            (SOP.hcpure proxySingle (SOP.Fn $ \(Comp (Flip r)) -> SOP.Fn $ \(Comp (Flip l)) -> SOP.K $ foldLedgerTables2 f l r))
            right
            (Telescope.fromTip left)

      EQ -> recurseSameIdx left right
      GT -> Telescope.tip
        . SOP.unI
        $ Telescope.alignExtendNS
            (InPairs.hcmap (Proxy @(SOP.Compose (SOP.Compose TableStuff LedgerTables) LedgerState)) (\ip -> Telescope.Extend $ \cur ->
                                 SOP.I $ (SOP.K (), Comp
                                           . Flip
                                           . (undefined :: LedgerTables (LedgerState x) ValuesMK
                                                        -> LedgerTables (LedgerState x) mk2)
                                           . translateLedgerTablesWith (provideBoth ip undefined undefined)
                                           . (`withLedgerTables` polyEmptyLedgerTables)
                                           . unFlip
                                           . unComp $ cur))
              translate
            )
            (SOP.hcpure proxySingle (SOP.Fn $ \(Comp (Flip l)) -> SOP.Fn $ \(Comp (Flip r)) -> SOP.K $ foldLedgerTables2 f l r))
            left
            (Telescope.fromTip right)
    where
      recurseSameIdx :: SOP.All (SOP.Compose TableStuff LedgerState) xs
                     => NS (Flip LedgerTables mk1 :.: LedgerState) xs
                     -> NS (Flip LedgerTables mk2 :.: LedgerState) xs
                     -> NS (SOP.K m) xs
      recurseSameIdx (Z l) (Z r) = Z $ SOP.K $ foldLedgerTables2 f (unFlip $ unComp l) (unFlip $ unComp r)
      recurseSameIdx (S l) (S r) = S $ recurseSameIdx l r
      recurseSameIdx _     _     = case () of

      translate :: InPairs (RequiringBoth WrapLedgerConfig TranslateLedgerState) (CardanoEras c)
      translate = translateLedgerState hardForkEraTranslation
    -- SOP.hcollapse
   -- $ go l r
   -- where
   --   go :: ( SOP.All (SOP.And (SOP.Compose TableStuff LedgerState) SingleEraBlock) xs
   --          )
   --       => NS (Flip LedgerTables mk1 :.: LedgerState) xs
   --       -> NS (Flip LedgerTables mk2 :.: LedgerState) xs
   --       -> NS (SOP.K m) xs
   --   go (Z (Comp l')) (Z (Comp r')) = Z (SOP.K $ foldLedgerTables2 f (unFlip l') (unFlip r'))
   --   go (S l')        (S r')        = S $ go l' r'
   --   go (Z _)         (S _)        = error "unaligned foldLedgerTables2"
   --   go (S _)         (Z _)        = error "unaligned foldLedgerTables2"



instance CardanoHardForkConstraints c
      => SufficientSerializationForAnyBackingStore (LedgerState (CardanoBlock c)) where
    codecLedgerTables = CardanoLedgerTables (Z . Comp . Flip $ codecLedgerTables) -- TODO @js: these are byron codecs??

deriving newtype instance (PraosCrypto c, SOP.All (SOP.Compose Eq (Flip LedgerTables EmptyMK :.: LedgerState)) (CardanoEras c)) => Eq (LedgerTables (LedgerState (CardanoBlock c)) EmptyMK)
deriving newtype instance (PraosCrypto c, SOP.All (SOP.Compose Eq (Flip LedgerTables ValuesMK :.: LedgerState)) (CardanoEras c)) => Eq (LedgerTables (LedgerState (CardanoBlock c)) ValuesMK)
deriving newtype instance (PraosCrypto c, SOP.All (SOP.Compose Eq (Flip LedgerTables DiffMK :.: LedgerState)) (CardanoEras c)) => Eq (LedgerTables (LedgerState (CardanoBlock c)) DiffMK)

deriving instance Eq (LedgerTables (LedgerState blk) EmptyMK) => Eq ((Flip LedgerTables EmptyMK :.: LedgerState) blk)
deriving instance Eq (LedgerTables (LedgerState blk) DiffMK) => Eq ((Flip LedgerTables DiffMK :.: LedgerState) blk)
deriving instance Eq (LedgerTables (LedgerState blk) ValuesMK) => Eq ((Flip LedgerTables ValuesMK :.: LedgerState) blk)

deriving newtype instance (PraosCrypto c, SOP.All (SOP.Compose NoThunks (Flip LedgerTables EmptyMK :.: LedgerState)) (CardanoEras c)) => NoThunks (LedgerTables (LedgerState (CardanoBlock c)) EmptyMK)
deriving newtype instance (PraosCrypto c, SOP.All (SOP.Compose NoThunks (Flip LedgerTables ValuesMK :.: LedgerState)) (CardanoEras c)) => NoThunks (LedgerTables (LedgerState (CardanoBlock c)) ValuesMK)
deriving newtype instance (PraosCrypto c, SOP.All (SOP.Compose NoThunks (Flip LedgerTables SeqDiffMK :.: LedgerState)) (CardanoEras c)) => NoThunks (LedgerTables (LedgerState (CardanoBlock c)) SeqDiffMK)

deriving anyclass instance NoThunks (LedgerTables (LedgerState blk) EmptyMK) => NoThunks ((Flip LedgerTables EmptyMK :.: LedgerState) blk)
deriving anyclass instance NoThunks (LedgerTables (LedgerState blk) ValuesMK) => NoThunks ((Flip LedgerTables ValuesMK :.: LedgerState) blk)
deriving anyclass instance NoThunks (LedgerTables (LedgerState blk) SeqDiffMK) => NoThunks ((Flip LedgerTables SeqDiffMK :.: LedgerState) blk)

instance CardanoHardForkConstraints c => ShowLedgerState (LedgerTables (LedgerState (CardanoBlock c))) where
  showsLedgerState mk =
      SOP.hcollapse
    . SOP.hcmap proxySingle ( SOP.K
                            . showsLedgerState mk
                            . unFlip
                            . unComp)
    . cardanoUTxOTable

-- | Auxiliary for convenience
--
-- We can't reuse 'Translate' because we don't have the 'EpochNo' it requires.
-- newtype TranslateTxOutWrapper era1 era2 =
--     TranslateTxOutWrapper (Core.TxOut era1 -> Core.TxOut era2)

-- | The 'Core.TxOut' translations between the adjacent Shelley-based eras in
-- Cardano
-- translateTxOut ::
--      CardanoHardForkConstraints c
--   => InPairs
--        TranslateTxOutWrapper
--        (ShelleyBasedEras c)
-- translateTxOut =
--       PCons (TranslateTxOutWrapper $ SL.translateEra' ())
--     $ PCons (TranslateTxOutWrapper $ SL.translateEra' ())
--     $ PCons (TranslateTxOutWrapper $ Alonzo.translateTxOut)
--     $ PNil

-- | Auxiliary @SOP@ combinator
--
-- WARNING: The functions in the result fail if the 'NS' argument's tag
-- precedes the 'NP' index.
--
-- TODO use an accumulator instead of this quadratic traversal
-- composeTxOutTranslationPairs ::
--      (SOP.SListI eras, HasCallStack)
--   => InPairs
--        TranslateTxOutWrapper
--        eras
--   -> NP
--        (SOP.K (NS TxOutWrapper eras) -.-> TxOutWrapper)
--        eras
-- composeTxOutTranslationPairs = \case
--     PNil                                  ->
--       (SOP.fn $ SOP.unZ . SOP.unK) :* Nil
--     PCons (TranslateTxOutWrapper f) inner ->
--          (SOP.fn $
--             eitherNS
--               id
--               (error "composeTxOutTranslationPairs: anachrony")
--           . SOP.unK
--          )
--       :* SOP.hmap
--           (\innerf -> SOP.fn $
--               SOP.apFn innerf
--             . SOP.K
--             . eitherNS
--                 (Z . TxOutWrapper . f . unTxOutWrapper)
--                 id
--             . SOP.unK)
--           (composeTxOutTranslationPairs inner)
--   where
--     eitherNS :: (f x -> c) -> (NS f xs -> c) -> NS f (x ': xs) -> c
--     eitherNS l r = \case
--       Z x -> l x
--       S x -> r x

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
  projectLedgerTablesTicked =
        CardanoLedgerTables
      . SOP.hcmap proxySingle ( Comp
                              . Flip
                              . projectLedgerTablesTicked
                              . getFlipTickedLedgerState
                              . currentState
                              )
      . Telescope.tip
      . getHardForkState
      . tickedHardForkLedgerStatePerEra

  withLedgerTablesTicked TickedHardForkLedgerState{..} (CardanoLedgerTables tables) =
      TickedHardForkLedgerState tickedHardForkLedgerStateTransition
      . HardForkState
      . f tables
      . getHardForkState
      $ tickedHardForkLedgerStatePerEra
    where
      f :: (IsApplyMapKind mk, SOP.All (SOP.Compose TickedTableStuff LedgerState) xs)
        => NS (Flip LedgerTables mk :.: LedgerState)               xs
        -> Telescope (SOP.K Past) (Current (FlipTickedLedgerState any)) xs
        -> Telescope (SOP.K Past) (Current (FlipTickedLedgerState mk))  xs
      f (Z (Comp tbs)) (TZ ttip) = TZ
                                 . Current (currentStart ttip)
                                 . FlipTickedLedgerState
                                 . withLedgerTablesTicked ( getFlipTickedLedgerState
                                                          . currentState
                                                          $ ttip
                                                          )
                                 . unFlip
                                 $ tbs
      f (S s)          (TS g ts) = TS g (f s ts)
      f (S _)          (TZ _)    = error "unaligned withLedgerTables"
      f (Z _)          (TS _ _)  = error "unaligned withLedgerTables"

instance CardanoHardForkConstraints c => LedgerTablesCanHardFork (CardanoEras c) where
  hardForkInjectLedgerTables =
         byron
      :* shelley (IS IZ)
      :* shelley (IS $ IS IZ)
      :* shelley (IS $ IS (IS IZ))
      :* shelley (IS $ IS (IS (IS IZ)))
      :* Nil
    where
      byron :: InjectLedgerTables (CardanoEras c) ByronBlock
      byron = InjectLedgerTables $ \NoByronLedgerTables -> polyEmptyLedgerTables

      shelley :: SOP.Index (CardanoEras c) (ShelleyBlock era)
              -> InjectLedgerTables (CardanoEras c) (ShelleyBlock era)
      shelley idx =
          InjectLedgerTables
        $ CardanoLedgerTables . SOP.injectNS idx . Comp . Flip


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
      RequireBoth
    $ \_ (WrapLedgerConfig cfgShelley) ->
        TranslateLedgerState {
            translateLedgerStateWith = \epochNo ledgerByron -> unstowLedgerTables
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
          , translateLedgerTablesWith = translateNoTables
          }
  where
    translateNoTables ::
          IsApplyMapKind mk
       => LedgerTables (LedgerState ByronBlock)                    mk
       -> LedgerTables (LedgerState (ShelleyBlock (ShelleyEra c))) mk
    translateNoTables NoByronLedgerTables = polyEmptyLedgerTables

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
      TranslateLedgerState {
          translateLedgerStateWith = \_epochNo ->
                (flip withLedgerTables  polyEmptyLedgerTables)
              . unFlip
              . unComp
              . SL.translateEra' ()
              . Comp
              . Flip
        , translateLedgerTablesWith =
              \ShelleyLedgerTables { shelleyUTxOTable = ApplyValuesMK (HD.UtxoValues vals) } ->
               ShelleyLedgerTables { shelleyUTxOTable = ApplyValuesMK
                                                      . HD.UtxoValues
                                                      . fmap (SL.translateEra' ())
                                                      $ vals
                                   }
        }

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
      TranslateLedgerState {
          translateLedgerStateWith = \_epochNo ->
                (flip withLedgerTables polyEmptyLedgerTables)
              . unFlip
              . unComp
              . SL.translateEra' ()
              . Comp
              . Flip
        , translateLedgerTablesWith =
            \ShelleyLedgerTables { shelleyUTxOTable = ApplyValuesMK (HD.UtxoValues vals) } ->
             ShelleyLedgerTables { shelleyUTxOTable = ApplyValuesMK
                                                    . HD.UtxoValues
                                                    . fmap (SL.translateEra' ())
                                                    $ vals
                                 }
        }

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
      TranslateLedgerState {
          translateLedgerStateWith = \_epochNo ->
                (flip withLedgerTables polyEmptyLedgerTables)
              . unFlip
              . unComp
              . SL.translateEra' (getAlonzoTranslationContext cfgAlonzo)
              . Comp
              . Flip
        , translateLedgerTablesWith =
            \ShelleyLedgerTables { shelleyUTxOTable = ApplyValuesMK (HD.UtxoValues vals) } ->
             ShelleyLedgerTables { shelleyUTxOTable = ApplyValuesMK
                                                    . HD.UtxoValues
                                                    . fmap Alonzo.translateTxOut
                                                    $ vals
                                 }
        }

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
