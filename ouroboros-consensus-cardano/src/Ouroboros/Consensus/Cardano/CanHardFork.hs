{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveAnyClass           #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE InstanceSigs             #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Cardano.CanHardFork (
    HardCodedTransition (..)
  , ByronPartialLedgerConfig (..)
  , ShelleyPartialLedgerConfig (..)
  ) where

import           Control.Monad.Reader (runReader)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Proxy
import           Data.Word
import           GHC.Generics (Generic)

import qualified Cardano.Chain.Block as CC
import qualified Cardano.Chain.Common as CC
import qualified Cardano.Chain.Update.Validation.Endorsement as CC.Update
import qualified Cardano.Chain.Update.Validation.Interface as CC.Update
import qualified Cardano.Chain.UTxO as CC
import qualified Cardano.Crypto.Hash as Hash
import qualified Cardano.Crypto.Hashing as Hashing
import           Cardano.Prelude (Natural, NoUnexpectedThunks)
import           Cardano.Slotting.Slot

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config.SecurityParam
import           Ouroboros.Consensus.HardFork.History (Bound (..),
                     EraParams (..))
import qualified Ouroboros.Consensus.HardFork.History.Util as History
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.TypeFamilyWrappers

import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.State.Types
import           Ouroboros.Consensus.HardFork.Combinator.Util.InPairs
                     (InPairs (..), RequiringBoth (..))

import           Ouroboros.Consensus.Byron.Ledger
import qualified Ouroboros.Consensus.Byron.Ledger.Conversions as Byron
import           Ouroboros.Consensus.Byron.Node ()
import           Ouroboros.Consensus.Protocol.PBFT (PBft, PBftCrypto)
import           Ouroboros.Consensus.Protocol.PBFT.State (PBftState)
import qualified Ouroboros.Consensus.Protocol.PBFT.State as PBftState

import           Ouroboros.Consensus.Shelley.Ledger
import qualified Ouroboros.Consensus.Shelley.Ledger.History as History
import           Ouroboros.Consensus.Shelley.Node (ShelleyGenesis (..))
import           Ouroboros.Consensus.Shelley.Protocol
import           Ouroboros.Consensus.Shelley.Protocol.State (TPraosState)
import qualified Ouroboros.Consensus.Shelley.Protocol.State as TPraosState

import qualified Shelley.Spec.Ledger.Address as SL
import qualified Shelley.Spec.Ledger.API as SL
import qualified Shelley.Spec.Ledger.BaseTypes as SL
import qualified Shelley.Spec.Ledger.Coin as SL
import qualified Shelley.Spec.Ledger.Delegation.Certificates as SL
import qualified Shelley.Spec.Ledger.EpochBoundary as SL
import qualified Shelley.Spec.Ledger.Genesis as SL
import qualified Shelley.Spec.Ledger.Keys as SL
import qualified Shelley.Spec.Ledger.LedgerState as SL
import qualified Shelley.Spec.Ledger.PParams as SL
import qualified Shelley.Spec.Ledger.Rewards as SL
import qualified Shelley.Spec.Ledger.STS.Prtcl as SL
import qualified Shelley.Spec.Ledger.TxData as SL
import qualified Shelley.Spec.Ledger.UTxO as SL

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
                -> EraParams
                -> Bound
                -> LedgerState ByronBlock
                -> Maybe EpochNo
byronTransition ByronPartialLedgerConfig{..}
                EraParams{..}
                eraStart
                st@ByronLedgerState{..} =
      fmap cpuEpoch
    . latest
    . filter isStable
    . CC.Update.candidateProtocolUpdates
    . CC.cvsUpdateState
    $ byronLedgerState
  where
    SecurityParam k = Byron.genesisSecurityParam byronLedgerConfig

    cpuSlot :: CC.Update.CandidateProtocolUpdate -> SlotNo
    cpuSlot = Byron.fromByronSlotNo . CC.Update.cpuSlot

    -- This follows the same structure as the computation in the A/B test. Let
    -- @s@ be the slot of the update proposal. Note that the very first slot in
    -- which the transition /could/ occur is @s + 1@; adding the required
    -- stability, the first slot in which the transition could occur is @s + 4k
    -- + 1@. This means that the last slot which /must/ be in /this/ era is @s +
    -- 4k@. Hence the last /epoch/ that must be in this era is @epoch (s + 4k)@,
    -- and the first epoch of the /next/ era is @succ (epoch (s + 4k)@.
    cpuEpoch :: CC.Update.CandidateProtocolUpdate -> EpochNo
    cpuEpoch upd = succ (slotToEpoch $ History.addSlots (4 * k) (cpuSlot upd))

    -- Slot conversion (valid for slots in this era only)
    slotToEpoch :: SlotNo -> EpochNo
    slotToEpoch s =
        History.addEpochs
          (History.countSlots s (boundSlot eraStart) `div` unEpochSize eraEpochSize)
          (boundEpoch eraStart)

    isStable :: CC.Update.CandidateProtocolUpdate -> Bool
    isStable upd = confirmationDepth >= 2 * k
      where
        confirmedInSlot :: SlotNo
        confirmedInSlot = cpuSlot upd

        -- The impossible cases are impossible because the ledger contains
        -- at least the block containing the update proposal, and hence cannot
        -- be empty or have a tip /before/ the slot number of that block.
        confirmationDepth :: Word64
        confirmationDepth =
            case ledgerTipSlot st of
              Origin -> error "byronTransition: impossible"
              At s   -> if s < confirmedInSlot
                          then error "byronTransition: impossible"
                          else History.countSlots s confirmedInSlot

    -- 'tryBumpVersion' assumes head of the list is the newest, so we do too
    latest :: [CC.Update.CandidateProtocolUpdate]
           -> Maybe CC.Update.CandidateProtocolUpdate
    latest (newest:_) = Just newest
    latest []         = Nothing

{-------------------------------------------------------------------------------
  SingleEraBlock Byron
-------------------------------------------------------------------------------}

instance SingleEraBlock ByronBlock where
  singleEraTransition pcfg eraParams eraStart ledgerState =
      case transitionEpoch pcfg of
        NoHardCodedTransition -> byronTransition pcfg eraParams eraStart ledgerState
        HardCodedTransitionAt epoch -> Just epoch

  singleEraInfo _ = SingleEraInfo {
      singleEraName = "Byron"
    }

instance PBftCrypto bc => HasPartialConsensusConfig (PBft bc)
  -- Use defaults

data HardCodedTransition =
    NoHardCodedTransition
  | HardCodedTransitionAt !EpochNo
  deriving (Generic, NoUnexpectedThunks)

-- | When Byron is part of the hard-fork combinator, we use the partial ledger
-- config. Standalone Byron uses the regular ledger config. This means that
-- the partial ledger config is the perfect place to store a hard-coded, yet
-- configurable, transition point for testing purposes, as we don't have to
-- modify the ledger config for standalone Byron.
data ByronPartialLedgerConfig = ByronPartialLedgerConfig {
      byronLedgerConfig :: !(LedgerConfig ByronBlock)
    , transitionEpoch   :: !HardCodedTransition
    }
  deriving (Generic, NoUnexpectedThunks)

instance HasPartialLedgerConfig ByronBlock where

  type PartialLedgerConfig ByronBlock = ByronPartialLedgerConfig

  completeLedgerConfig _ _ = byronLedgerConfig

{-------------------------------------------------------------------------------
  SingleEraBlock Shelley
-------------------------------------------------------------------------------}

instance TPraosCrypto sc => SingleEraBlock (ShelleyBlock sc) where
  -- No transition from Shelley to Goguen yet
  singleEraTransition _cfg _eraParams _eraStart _st = Nothing

  singleEraInfo _ = SingleEraInfo {
      singleEraName = "Shelley"
    }

instance TPraosCrypto sc => HasPartialConsensusConfig (TPraos sc) where
  type PartialConsensusConfig (TPraos sc) = TPraosParams

  completeConsensusConfig _ tpraosEpochInfo tpraosParams = TPraosConfig {..}

  -- 'ChainSelConfig' is ()
  partialChainSelConfig _ _ = ()

data ShelleyPartialLedgerConfig sc = ShelleyPartialLedgerConfig {
      shelleyPartialLedgerGenesis :: !(SL.ShelleyGenesis sc)
    , shelleyPartialMaxMajorPV    :: !Natural
    }
  deriving (Generic, NoUnexpectedThunks)

instance TPraosCrypto sc => HasPartialLedgerConfig (ShelleyBlock sc) where
  type PartialLedgerConfig (ShelleyBlock sc) = ShelleyPartialLedgerConfig sc

  completeLedgerConfig _ epochInfo ShelleyPartialLedgerConfig {..} =
      mkShelleyLedgerConfig
        shelleyPartialLedgerGenesis
        epochInfo
        shelleyPartialMaxMajorPV

{-------------------------------------------------------------------------------
  CanHardFork
-------------------------------------------------------------------------------}

instance TPraosCrypto c => CanHardFork (CardanoEras c) where
  hardForkEraTranslation = EraTranslation {
      translateLedgerState    = PCons translateLedgerStateByronToShelleyWrapper    PNil
    , translateLedgerView     = PCons translateLedgerViewByronToShelleyWrapper     PNil
    , translateConsensusState = PCons translateConsensusStateByronToShelleyWrapper PNil
    }

{-------------------------------------------------------------------------------
  Translation from Byron to Shelley
-------------------------------------------------------------------------------}

translateHeaderHashByronToShelley
  :: forall sc. Crypto sc
  => HeaderHash ByronBlock
  -> HeaderHash (ShelleyBlock sc)
translateHeaderHashByronToShelley =
      fromRawHash (Proxy @(ShelleyBlock sc))
    . toRawHash   (Proxy @ByronBlock)

translatePointByronToShelley
  :: Crypto sc
  => Point ByronBlock
  -> Point (ShelleyBlock sc)
translatePointByronToShelley = \case
    GenesisPoint   -> GenesisPoint
    BlockPoint s h -> BlockPoint s (translateHeaderHashByronToShelley h)

translateUTxOByronToShelley :: forall sc. CC.UTxO -> SL.UTxO sc
translateUTxOByronToShelley (CC.UTxO utxoByron) =
    SL.UTxO $ Map.fromList
      [ (txInShelley, txOutShelley)
      | (txInByron, txOutByron) <- Map.toList utxoByron
      , let txInShelley  = translateTxIn  $ CC.fromCompactTxIn  txInByron
            txOutShelley = translateTxOut $ CC.fromCompactTxOut txOutByron
      ]
  where
    translateTxIn :: CC.TxIn -> SL.TxIn sc
    translateTxIn (CC.TxInUtxo txId idx) =
      SL.TxIn (translateTxId txId) (fromIntegral idx)

    translateTxOut :: CC.TxOut -> SL.TxOut sc
    translateTxOut (CC.TxOut addr amount) =
      SL.TxOut (translateAddr addr) (translateAmount amount)

    -- | We use the same hasing algorithm so we can unwrap and rewrap the
    -- bytes. We don't care about the type that is hashed, which will differ
    -- going from Byron to Shelley, we just use the hashes as IDs.
    translateTxId :: CC.TxId -> SL.TxId sc
    translateTxId = SL.TxId . Hash.UnsafeHash . Hashing.hashToBytes

    translateAmount :: CC.Lovelace -> SL.Coin
    translateAmount = SL.Coin . CC.lovelaceToInteger

    translateAddr :: CC.Address -> SL.Addr sc
    translateAddr = SL.AddrBootstrap . SL.BootstrapAddress

translateLedgerStateByronToShelleyWrapper
  :: forall sc. Crypto sc
  => RequiringBoth
       WrapLedgerConfig
       (Translate LedgerState)
       ByronBlock
       (ShelleyBlock sc)
translateLedgerStateByronToShelleyWrapper =
    RequireBoth $ \_ (WrapLedgerConfig cfgShelley) ->
    Translate   $ \epochNo ledgerByron ->
      translateLedgerStateByronToShelley cfgShelley epochNo ledgerByron

translateLedgerStateByronToShelley
  :: forall sc. Crypto sc
  => LedgerConfig (ShelleyBlock sc)
  -> EpochNo
  -> LedgerState ByronBlock
  -> LedgerState (ShelleyBlock sc)
translateLedgerStateByronToShelley cfgShelley epochNo ledgerByron =
    ShelleyLedgerState {
      ledgerTip = ledgerTipShelley
    , history   = History.empty
    , shelleyState
    }
  where
    ShelleyLedgerConfig { shelleyLedgerGenesis = genesisShelley } = cfgShelley

    shelleyState :: SL.ShelleyState sc
    shelleyState = SL.NewEpochState {
        nesEL     = epochNo
      , nesBprev  = SL.BlocksMade Map.empty
      , nesBcur   = SL.BlocksMade Map.empty
      , nesEs     = epochState
      , nesRu     = SL.SNothing
      , nesPd     = SL.PoolDistr Map.empty
      , nesOsched = overlaySchedule
      }

    pparams :: SL.PParams
    pparams = sgProtocolParams genesisShelley

    -- | NOTE: we ignore the Byron delegation map because the genesis and
    -- delegation verification keys are hashed using a different hashing
    -- scheme, Blake2b_224, whereas Shelley uses Blake2b_256. This means we
    -- can't simply convert them, as Byron nowhere stores the original
    -- verification keys.
    --
    -- Fortunately, no Byron genesis delegations have happened yet, and if
    -- they did, we would be aware of them before the hard fork, as we
    -- instigate the hard fork. We just have to make sure that the hard-coded
    -- Shelley genesis contains the same genesis and delegation verification
    -- keys, but hashed with the right algorithm.
    genDelegs :: SL.GenDelegs sc
    genDelegs = SL.GenDelegs $ sgGenDelegs genesisShelley

    reserves :: SL.Coin
    reserves =
        fromIntegral (sgMaxLovelaceSupply genesisShelley) - SL.balance utxoShelley

    epochState :: SL.EpochState sc
    epochState = SL.EpochState {
        esAccountState = SL.AccountState (SL.Coin 0) reserves
      , esSnapshots    = SL.emptySnapShots
      , esLState       = ledgerState
      , esPrevPp       = pparams
      , esPp           = pparams
      , esNonMyopic    = SL.emptyNonMyopic
      }

    utxoByron :: CC.UTxO
    utxoByron = CC.cvsUtxo $ byronLedgerState ledgerByron

    utxoShelley :: SL.UTxO sc
    utxoShelley = translateUTxOByronToShelley utxoByron

    ledgerState :: SL.LedgerState sc
    ledgerState = SL.LedgerState {
        _utxoState = SL.UTxOState {
            _utxo      = utxoShelley
          , _deposited = SL.Coin 0
          , _fees      = SL.Coin 0
          , _ppups     = SL.ProposedPPUpdates Map.empty
          }
      , _delegationState = SL.DPState {
          _dstate = SL.emptyDState { SL._genDelegs = genDelegs }
        , _pstate = SL.emptyPState
        }
      }

    ledgerTipShelley :: Point (ShelleyBlock sc)
    ledgerTipShelley =
      translatePointByronToShelley $
      ledgerTipPoint' (Proxy @ByronBlock) ledgerByron

    overlaySchedule :: Map SlotNo (SL.OBftSlot sc)
    overlaySchedule =
      flip runReader (shelleyLedgerGlobals cfgShelley) $
        SL.overlaySchedule
          epochNo
          (Map.keysSet (sgGenDelegs genesisShelley))
          (sgProtocolParams genesisShelley)

translateConsensusStateByronToShelleyWrapper
  :: forall sc.
     RequiringBoth
       WrapConsensusConfig
       (Translate WrapConsensusState)
       ByronBlock
       (ShelleyBlock sc)
translateConsensusStateByronToShelleyWrapper =
    RequireBoth $ \_ _ -> Translate   $ \_ (WrapConsensusState pbftState) ->
      WrapConsensusState (translateConsensusStateByronToShelley pbftState)

translateConsensusStateByronToShelley
  :: forall bc sc.
     PBftState bc
  -> TPraosState sc
translateConsensusStateByronToShelley pbftState =
    TPraosState.empty (PBftState.tipSlot pbftState) $
      SL.PrtclState
        Map.empty
        nonce
        nonce
        nonce
        nonce
  where
    -- TODO use hash of Shelley genesis config as entropy?
    nonce = SL.NeutralNonce

translateLedgerViewByronToShelleyWrapper
  :: forall sc.
     RequiringBoth
       WrapLedgerConfig
       (Translate WrapLedgerView)
       ByronBlock
       (ShelleyBlock sc)
translateLedgerViewByronToShelleyWrapper =
    RequireBoth $ \_ (WrapLedgerConfig shelleyCfg) -> Translate $ \epochNo _ ->
      WrapLedgerView (translateLedgerViewByronToShelley shelleyCfg epochNo)

-- | We construct a 'SL.LedgerView' using the Shelley genesis config in the
-- same way as 'translateLedgerStateByronToShelley'.
translateLedgerViewByronToShelley
  :: forall sc.
     LedgerConfig (ShelleyBlock sc)
  -> EpochNo
  -> SL.LedgerView sc
translateLedgerViewByronToShelley shelleyCfg epochNo = SL.LedgerView {
      lvProtParams   = sgProtocolParams genesisShelley
    , lvOverlaySched = overlaySchedule
    , lvPoolDistr    = SL.PoolDistr Map.empty
    , lvGenDelegs    = SL.GenDelegs $ sgGenDelegs genesisShelley
    }
  where
    ShelleyLedgerConfig {
        shelleyLedgerGenesis = genesisShelley
      , shelleyLedgerGlobals
      } = shelleyCfg

    overlaySchedule :: Map SlotNo (SL.OBftSlot sc)
    overlaySchedule =
      flip runReader shelleyLedgerGlobals $
        SL.overlaySchedule
          epochNo
          (Map.keysSet (sgGenDelegs genesisShelley))
          (sgProtocolParams genesisShelley)
