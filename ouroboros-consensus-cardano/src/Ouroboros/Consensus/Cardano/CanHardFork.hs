{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveAnyClass           #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Cardano.CanHardFork (
    TriggerHardFork (..)
  , ByronPartialLedgerConfig (..)
  , ShelleyPartialLedgerConfig (..)
  , CardanoHardForkConstraints
  ) where

import qualified Data.Map.Strict as Map
import           Data.Maybe (mapMaybe)
import           Data.Proxy
import           Data.Word
import           GHC.Generics (Generic)

import           Cardano.Crypto.DSIGN (Ed25519DSIGN)
import           Cardano.Crypto.Hash.Blake2b (Blake2b_224, Blake2b_256)
import           Cardano.Prelude (NoUnexpectedThunks)

import qualified Cardano.Chain.Update as CC.Update

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util.RedundantConstraints

import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.State.Types
import           Ouroboros.Consensus.HardFork.Combinator.Util.InPairs
                     (RequiringBoth (..))
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.InPairs as InPairs
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Tails as Tails

import           Ouroboros.Consensus.Byron.Ledger
import qualified Ouroboros.Consensus.Byron.Ledger.Inspect as Byron.Inspect
import           Ouroboros.Consensus.Byron.Node ()
import           Ouroboros.Consensus.Protocol.PBFT (PBft, PBftCrypto)
import           Ouroboros.Consensus.Protocol.PBFT.State (PBftState)
import qualified Ouroboros.Consensus.Protocol.PBFT.State as PBftState

import           Ouroboros.Consensus.Shelley.Ledger
import           Ouroboros.Consensus.Shelley.Node ()
import           Ouroboros.Consensus.Shelley.Protocol
import           Ouroboros.Consensus.Shelley.Protocol.State (TPraosState)
import qualified Ouroboros.Consensus.Shelley.Protocol.State as TPraosState


import           Cardano.Ledger.Crypto (ADDRHASH, DSIGN, HASH)
import qualified Cardano.Ledger.Era as Era
import qualified Shelley.Spec.Ledger.API as SL
import qualified Shelley.Spec.Ledger.BaseTypes as SL
import qualified Shelley.Spec.Ledger.ByronTranslation as SL
import qualified Shelley.Spec.Ledger.STS.Prtcl as SL
import qualified Shelley.Spec.Ledger.STS.Tickn as SL

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
byronTransition ByronPartialLedgerConfig{..} shelleyMajorVersion =
      latest
    . mapMaybe Byron.Inspect.isStableCandidate
    . filter bumpsMajorProtocolVersion
    . Byron.Inspect.protocolUpdates byronLedgerConfig
  where
    bumpsMajorProtocolVersion :: Byron.Inspect.ProtocolUpdate -> Bool
    bumpsMajorProtocolVersion Byron.Inspect.ProtocolUpdate{..} =
           shelleyMajorVersion
        == CC.Update.pvMajor protocolUpdateVersion

    -- 'tryBumpVersion' assumes head of the list of stable proposals is the
    -- newest, so we do too
    latest :: [a] -> Maybe a
    latest (newest:_) = Just newest
    latest []         = Nothing

{-------------------------------------------------------------------------------
  SingleEraBlock Byron
-------------------------------------------------------------------------------}

instance SingleEraBlock ByronBlock where
  singleEraTransition pcfg _eraParams _eraStart ledgerState =
      case triggerHardFork pcfg of
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
  deriving (Generic, NoUnexpectedThunks)

-- | When Byron is part of the hard-fork combinator, we use the partial ledger
-- config. Standalone Byron uses the regular ledger config. This means that
-- the partial ledger config is the perfect place to store the trigger
-- condition for the hard fork to Shelley, as we don't have to modify the
-- ledger config for standalone Byron.
data ByronPartialLedgerConfig = ByronPartialLedgerConfig {
      byronLedgerConfig :: !(LedgerConfig ByronBlock)
    , triggerHardFork   :: !TriggerHardFork
    }
  deriving (Generic, NoUnexpectedThunks)

instance HasPartialLedgerConfig ByronBlock where

  type PartialLedgerConfig ByronBlock = ByronPartialLedgerConfig

  completeLedgerConfig _ _ = byronLedgerConfig

{-------------------------------------------------------------------------------
  SingleEraBlock Shelley
-------------------------------------------------------------------------------}

instance TPraosCrypto era => SingleEraBlock (ShelleyBlock era) where
  -- No transition from Shelley to Goguen yet
  singleEraTransition _cfg _eraParams _eraStart _st = Nothing

  singleEraInfo _ = SingleEraInfo {
      singleEraName = "Shelley"
    }

instance TPraosCrypto era => HasPartialConsensusConfig (TPraos era) where
  type PartialConsensusConfig (TPraos era) = TPraosParams

  completeConsensusConfig _ tpraosEpochInfo tpraosParams = TPraosConfig {..}

  -- 'ChainSelConfig' is ()
  partialChainSelConfig _ _ = ()

newtype ShelleyPartialLedgerConfig era = ShelleyPartialLedgerConfig {
      -- | We cache the non-partial ledger config containing a dummy
      -- 'EpochInfo' that needs to be replaced with the correct one.
      --
      -- We do this to avoid recomputing the ledger config each time
      -- 'completeLedgerConfig' is called, as 'mkShelleyLedgerConfig' does
      -- some rather expensive computations that shouldn't be repeated too
      -- often (e.g., 'sgActiveSlotCoeff').
      getShelleyPartialLedgerConfig :: ShelleyLedgerConfig era
    }
  deriving (Generic, NoUnexpectedThunks)

instance TPraosCrypto era => HasPartialLedgerConfig (ShelleyBlock era) where
  type PartialLedgerConfig (ShelleyBlock era) = ShelleyPartialLedgerConfig era

  -- Replace the dummy 'EpochInfo' with the real one
  completeLedgerConfig _ epochInfo (ShelleyPartialLedgerConfig cfg) =
      cfg {
          shelleyLedgerGlobals = (shelleyLedgerGlobals cfg) {
              SL.epochInfo = epochInfo
            }
        }

{-------------------------------------------------------------------------------
  CanHardFork
-------------------------------------------------------------------------------}

type CardanoHardForkConstraints c =
  ( TPraosCrypto (ShelleyEra c)
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
      translateLedgerState   = PCons translateLedgerStateByronToShelleyWrapper   PNil
    , translateChainDepState = PCons translateChainDepStateByronToShelleyWrapper PNil
    , translateLedgerView    = PCons translateLedgerViewByronToShelleyWrapper    PNil
    }
  hardForkChainSel  = Tails.mk2 CompareBlockNo
  hardForkInjectTxs = InPairs.mk2 $ InPairs.ignoringBoth cannotInjectTx

{-------------------------------------------------------------------------------
  Translation from Byron to Shelley
-------------------------------------------------------------------------------}

translateHeaderHashByronToShelley ::
     forall era. (Era era, HASH (Era.Crypto era) ~ Blake2b_256)
  => HeaderHash ByronBlock
  -> HeaderHash (ShelleyBlock era)
translateHeaderHashByronToShelley =
      fromShortRawHash (Proxy @(ShelleyBlock era))
    . toShortRawHash   (Proxy @ByronBlock)
  where
    -- Byron uses 'Blake2b_256' for header hashes
    _ = keepRedundantConstraint (Proxy @(HASH (Era.Crypto era) ~ Blake2b_256))

translatePointByronToShelley ::
     forall era.  (Era era, HASH (Era.Crypto era) ~ Blake2b_256)
  => Point ByronBlock
  -> Point (ShelleyBlock era)
translatePointByronToShelley = \case
    GenesisPoint   -> GenesisPoint
    BlockPoint s h -> BlockPoint s (translateHeaderHashByronToShelley h)

translateLedgerStateByronToShelleyWrapper ::
     ( Era era
     , HASH     (Era.Crypto era) ~ Blake2b_256
     , ADDRHASH (Era.Crypto era) ~ Blake2b_224
     )
  => RequiringBoth
       WrapLedgerConfig
       (Translate LedgerState)
       ByronBlock
       (ShelleyBlock era)
translateLedgerStateByronToShelleyWrapper =
    RequireBoth $ \_ (WrapLedgerConfig cfgShelley) ->
    Translate   $ \epochNo ledgerByron ->
      ShelleyLedgerState {
        ledgerTip =
          translatePointByronToShelley $
            ledgerTipPoint (Proxy @ByronBlock) ledgerByron
      , shelleyState =
          SL.translateToShelleyLedgerState
            (shelleyLedgerGenesis cfgShelley)
            (shelleyLedgerGlobals cfgShelley)
            epochNo
            (byronLedgerState ledgerByron)
      }

translateChainDepStateByronToShelleyWrapper
  :: forall era.
     RequiringBoth
       WrapConsensusConfig
       (Translate WrapChainDepState)
       ByronBlock
       (ShelleyBlock era)
translateChainDepStateByronToShelleyWrapper =
    RequireBoth $ \_ (WrapConsensusConfig cfgShelley) ->
      Translate $ \_ (WrapChainDepState pbftState) ->
        WrapChainDepState $
          translateChainDepStateByronToShelley cfgShelley pbftState

translateChainDepStateByronToShelley
  :: forall bc era.
     ConsensusConfig (TPraos era)
  -> PBftState bc
  -> TPraosState era
translateChainDepStateByronToShelley TPraosConfig { tpraosParams } pbftState =
    TPraosState.empty (PBftState.tipSlot pbftState) $
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
     forall era.
     RequiringBoth
       WrapLedgerConfig
       (TranslateForecast WrapLedgerView)
       ByronBlock
       (ShelleyBlock era)
translateLedgerViewByronToShelleyWrapper =
    RequireBoth $ \_ (WrapLedgerConfig cfgShelley) ->
      TranslateForecast $ \epochNo _forecastFor _finalByronView ->
        WrapTickedLedgerView $
          TickedPraosLedgerView $
            SL.mkInitialShelleyLedgerView
              (shelleyLedgerGenesis cfgShelley)
              (shelleyLedgerGlobals cfgShelley)
              epochNo
