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
{-# LANGUAGE TupleSections            #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Cardano.CanHardFork (
    TriggerHardFork (..)
  , ByronPartialLedgerConfig (..)
  , ShelleyPartialLedgerConfig (..)
  , CardanoHardForkConstraints
  ) where

import           Control.Monad
import qualified Data.Map.Strict as Map
import           Data.Maybe (listToMaybe, mapMaybe)
import           Data.Proxy
import           Data.Word
import           GHC.Generics (Generic)

import           Cardano.Crypto.DSIGN (Ed25519DSIGN)
import           Cardano.Crypto.Hash.Blake2b (Blake2b_224, Blake2b_256)
import           Cardano.Prelude (NoUnexpectedThunks)

import qualified Cardano.Chain.Common as CC
import qualified Cardano.Chain.Genesis as CC.Genesis
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
import qualified Ouroboros.Consensus.Shelley.Ledger.Inspect as Shelley.Inspect
import           Ouroboros.Consensus.Shelley.Node ()
import           Ouroboros.Consensus.Shelley.Protocol

import           Cardano.Ledger.Crypto (ADDRHASH, DSIGN, HASH)
import qualified Cardano.Ledger.Era as Era

import qualified Shelley.Spec.Ledger.API as SL
import qualified Shelley.Spec.Ledger.BaseTypes as SL
import qualified Shelley.Spec.Ledger.ByronTranslation as SL
import qualified Shelley.Spec.Ledger.Genesis as SL
import qualified Shelley.Spec.Ledger.PParams as SL
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
     forall era.
     PartialLedgerConfig (ShelleyBlock era)
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
  deriving (Generic, NoUnexpectedThunks)

-- | When Byron is part of the hard-fork combinator, we use the partial ledger
-- config. Standalone Byron uses the regular ledger config. This means that
-- the partial ledger config is the perfect place to store the trigger
-- condition for the hard fork to Shelley, as we don't have to modify the
-- ledger config for standalone Byron.
data ByronPartialLedgerConfig = ByronPartialLedgerConfig {
      byronLedgerConfig    :: !(LedgerConfig ByronBlock)
    , byronTriggerHardFork :: !TriggerHardFork
    }
  deriving (Generic, NoUnexpectedThunks)

instance HasPartialLedgerConfig ByronBlock where

  type PartialLedgerConfig ByronBlock = ByronPartialLedgerConfig

  completeLedgerConfig _ _ = byronLedgerConfig

{-------------------------------------------------------------------------------
  SingleEraBlock Shelley
-------------------------------------------------------------------------------}

instance TPraosCrypto era => SingleEraBlock (ShelleyBlock era) where
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
      singleEraName = "Shelley"
    }

instance TPraosCrypto era => HasPartialConsensusConfig (TPraos era) where
  type PartialConsensusConfig (TPraos era) = TPraosParams

  completeConsensusConfig _ tpraosEpochInfo tpraosParams = TPraosConfig {..}

  -- 'ChainSelConfig' is ()
  partialChainSelConfig _ _ = ()

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
  deriving (Generic, NoUnexpectedThunks)

instance TPraosCrypto era => HasPartialLedgerConfig (ShelleyBlock era) where
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
  -> WithOrigin BlockNo
  -> WithOrigin (ShelleyTip era)
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
     forall era.
     RequiringBoth
       WrapLedgerConfig
       (TranslateForecast WrapLedgerView)
       ByronBlock
       (ShelleyBlock era)
translateLedgerViewByronToShelleyWrapper =
    RequireBoth $ \_ (WrapLedgerConfig cfgShelley) ->
      TranslateForecast $ \_epochNo _forecastFor _finalByronView ->
        WrapTickedLedgerView $
          TickedPraosLedgerView $
            SL.mkInitialShelleyLedgerView
              (shelleyLedgerGenesis cfgShelley)
