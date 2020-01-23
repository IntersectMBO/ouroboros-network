{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}

-- | Thin wrapper around the Byron spec rules
--
-- Intended for qualified import
--
-- import qualified Ouroboros.Consensus.Ledger.ByronSpec.Rules as Rules
module Ouroboros.Consensus.Ledger.ByronSpec.Rules (
    -- * Ledger
    applyChainTick
    -- * Lift STS transition rules to the chain level
  , liftCHAIN
  , liftSDELEG
  , liftUTXOW
  , liftUPIREG
  , liftUPIVOTE
    -- * STS initial rules
  , initStateCHAIN
  ) where

import qualified Data.Set as Set

import qualified Cardano.Ledger.Spec.STS.UTXO as Spec
import qualified Cardano.Ledger.Spec.STS.UTXOW as Spec
import qualified Cardano.Spec.Chain.STS.Rule.Chain as Spec
import qualified Cardano.Spec.Chain.STS.Rule.Epoch as Spec
import qualified Control.State.Transition as Spec
import qualified Ledger.Core as Spec
import qualified Ledger.Delegation as Spec
import qualified Ledger.Update as Spec

import qualified Ouroboros.Consensus.Ledger.ByronSpec.ChainState as ChainState
import           Ouroboros.Consensus.Ledger.ByronSpec.Genesis
                     (ByronSpecGenesis (..))
import qualified Ouroboros.Consensus.Ledger.ByronSpec.Genesis as Genesis

{-------------------------------------------------------------------------------
  Chain tick
-------------------------------------------------------------------------------}

-- | Chain tick
--
-- There isn't something in the spec that directly corresponds to this, so we
-- have to combine a few different things:
--
-- 1. Apply the epoch rule (update the update state)
-- 2. Apply any scheduled delegations
-- 3. Set the slot number
--
-- This matches quite closely what 'applyChainTick' in
-- "Ouroboros.Consensus.Ledger.Byron.Auxiliary" does.
applyChainTick :: ByronSpecGenesis
               -> Spec.Slot
               -> Spec.State Spec.CHAIN
               -> Spec.State Spec.CHAIN
applyChainTick cfg slot st =
    case (Spec.applySTS epoch, Spec.applySTS deleg) of
      (Left _, _) ->
        error "applyChainTick: unexpected EPOCH failure"
      (_, Left _) ->
        error "applyChainTick: unexpected DELEG failure"
      (Right updateState', Right delegState') ->
          ChainState.setSlot     slot
        . ChainState.setUPIState updateState'
        . ChainState.setDIState  delegState'
        $ st
  where
    epoch :: Spec.RuleContext Spec.Transition Spec.EPOCH
    epoch = Spec.TRC (envEPOCH cfg st, ChainState.getUPIState st, slot)

    -- Empty list of certificates; we apply the rule only to apply previously
    -- scheduled delegations (rather than to introduce new ones)
    deleg :: Spec.RuleContext Spec.Transition Spec.DELEG
    deleg = Spec.TRC (envDELEG cfg st, ChainState.getDIState st, [])

{-------------------------------------------------------------------------------
  Lift STS transition rules to the chain level
-------------------------------------------------------------------------------}

type LiftRule sts = ByronSpecGenesis
                 -> Spec.Signal sts
                 -> Spec.State Spec.CHAIN
                 -> Either [[Spec.PredicateFailure sts]] (Spec.State Spec.CHAIN)

-- | Apply a block
--
-- This is a "trivial" (identity) lift.
liftCHAIN :: LiftRule Spec.CHAIN
liftCHAIN cfg block st =
    Spec.applySTS @Spec.CHAIN $
      Spec.TRC (Genesis.toChainEnv cfg, st, block)

-- | Apply delegation certificate
liftSDELEG :: LiftRule Spec.SDELEG
liftSDELEG cfg dcert st =
    fmap (flip ChainState.setDSState st) $
      Spec.applySTS @Spec.SDELEG $
        Spec.TRC (envSDELEG cfg st, ChainState.getDSState st, dcert)

-- | Apply transaction
liftUTXOW :: LiftRule Spec.UTXOW
liftUTXOW cfg tx st =
    fmap (flip ChainState.setUtxoState st) $
      Spec.applySTS @Spec.UTXOW $
        Spec.TRC (envUTXOW cfg st, ChainState.getUtxoState st, tx)

-- | Apply update proposal
liftUPIREG :: LiftRule Spec.UPIREG
liftUPIREG cfg prop st =
    fmap (flip ChainState.setUPIState st) $
      Spec.applySTS @Spec.UPIREG $
        Spec.TRC (envUPIREG cfg st, ChainState.getUPIState st, prop)

-- | Apply update vote
liftUPIVOTE :: LiftRule Spec.UPIVOTE
liftUPIVOTE cfg vote st =
    fmap (flip ChainState.setUPIState st) $
      Spec.applySTS @Spec.UPIVOTE $
        Spec.TRC (envUPIVOTE cfg st, ChainState.getUPIState st, vote)

{-------------------------------------------------------------------------------
  STS initial rules
-------------------------------------------------------------------------------}

initStateCHAIN :: ByronSpecGenesis -> Spec.State Spec.CHAIN
initStateCHAIN cfg =
    dontExpectError $
      Spec.applySTS @Spec.CHAIN $
        Spec.IRC (Genesis.toChainEnv cfg)
  where
    dontExpectError :: Either a b -> b
    dontExpectError (Left _)  = error "initStateCHAIN: unexpected error"
    dontExpectError (Right b) = b

{-------------------------------------------------------------------------------
  Auxiliary: environments to apply SOS rules

  NOTE: These environments pull in some information from the (abstract) genesis
  config and some information from the state; the reason is that although some
  values come from the state, as far as these rules are concerned, they are
  constants.
-------------------------------------------------------------------------------}

type ConstructEnv sts = ByronSpecGenesis
                     -> Spec.State Spec.CHAIN
                     -> Spec.Environment sts

envEPOCH :: ConstructEnv Spec.EPOCH
envEPOCH ByronSpecGenesis{..} st = (
      -- The _current_ epoch
      -- This is needed to detect if the new slot introduces the next epoch
      Spec.sEpoch (ChainState.getSlot st) byronSpecGenesisSecurityParam
    , byronSpecGenesisSecurityParam
    )

envDELEG :: ConstructEnv Spec.DELEG
envDELEG ByronSpecGenesis{..} st = Spec.DSEnv {
      _dSEnvAllowedDelegators = byronSpecGenesisDelegators
    , _dSEnvEpoch             = Spec.sEpoch
                                  (ChainState.getSlot st)
                                  byronSpecGenesisSecurityParam
    , _dSEnvSlot              = ChainState.getSlot st
    , _dSEnvK                 = byronSpecGenesisSecurityParam
    }

envUTXOW :: ConstructEnv Spec.UTXOW
envUTXOW ByronSpecGenesis{..} st = Spec.UTxOEnv {
      utxo0 = byronSpecGenesisInitUtxo
    , pps   = Spec.protocolParameters (ChainState.getUPIState st)
    }

envSDELEG :: ConstructEnv Spec.SDELEG
envSDELEG = envDELEG

envUPIREG :: ConstructEnv Spec.UPIREG
envUPIREG ByronSpecGenesis{..} st = (
      ChainState.getSlot st
    , Spec._dIStateDelegationMap (ChainState.getDIState st)
    , byronSpecGenesisSecurityParam
    , fromIntegral $ Set.size byronSpecGenesisDelegators
    )

envUPIVOTE :: ConstructEnv Spec.UPIVOTE
envUPIVOTE = envUPIREG
