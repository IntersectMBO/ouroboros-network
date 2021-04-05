{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Thin wrapper around the Byron spec rules
--
-- Intended for qualified import
--
-- import qualified Ouroboros.Consensus.ByronSpec.Ledger.Rules as Rules
module Ouroboros.Consensus.ByronSpec.Ledger.Rules (
    -- * Ledger
    applyChainTick
    -- * Lift STS transition rules to the chain level
  , liftCHAIN
  , liftSDELEG
  , liftUPIREG
  , liftUPIVOTE
  , liftUTXOW
    -- * STS initial rules
  , initStateCHAIN
    -- * Rule context (exported for the benefit of the tests
  , RuleContext (..)
  , ctxtCHAIN
  , ctxtDELEG
  , ctxtEPOCH
  , ctxtSDELEG
  , ctxtUPIREG
  , ctxtUPIVOTE
  , ctxtUTXOW
  ) where

import           Control.Monad
import           Control.Monad.Trans.Except
import           Data.Functor.Identity
import           Data.Proxy
import qualified Data.Set as Set

import qualified Byron.Spec.Chain.STS.Rule.BBody as Spec
import qualified Byron.Spec.Chain.STS.Rule.Bupi as Spec
import qualified Byron.Spec.Chain.STS.Rule.Chain as Spec
import qualified Byron.Spec.Chain.STS.Rule.Epoch as Spec
import qualified Byron.Spec.Ledger.Core as Spec
import qualified Byron.Spec.Ledger.Delegation as Spec
import qualified Byron.Spec.Ledger.STS.UTXO as Spec
import qualified Byron.Spec.Ledger.STS.UTXOW as Spec
import qualified Byron.Spec.Ledger.STS.UTXOWS as Spec
import qualified Byron.Spec.Ledger.Update as Spec
import qualified Control.State.Transition as Spec

import           Ouroboros.Consensus.ByronSpec.Ledger.Accessors
import           Ouroboros.Consensus.ByronSpec.Ledger.Genesis
                     (ByronSpecGenesis (..))
import qualified Ouroboros.Consensus.ByronSpec.Ledger.Genesis as Genesis

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
    case runExcept (go st) of
      Left  _   -> error "applyChainTick: unexpected failure"
      Right st' -> st'
  where
    go :: Spec.State Spec.CHAIN
       -> Except [[Spec.PredicateFailure Spec.CHAIN]] (Spec.State Spec.CHAIN)
    go =  -- Apply EPOCH rule (deals with update proposals)
          liftEPOCH cfg slot

          -- Apply scheduled delegations (empty list of new delegation certs)
      >=> liftDELEG cfg []

{-------------------------------------------------------------------------------
  Lift STS transition rules to the chain level
-------------------------------------------------------------------------------}

-- | Apply a block
--
-- This is a "trivial" (identity) lift.
liftCHAIN :: ByronSpecGenesis -> LiftedRule Spec.CHAIN
liftCHAIN = liftRule . ctxtCHAIN

-- | Apply delegation certificate
liftSDELEG :: ByronSpecGenesis -> LiftedRule Spec.SDELEG
liftSDELEG = liftRule . ctxtSDELEG

-- | Apply transaction
liftUTXOW :: ByronSpecGenesis -> LiftedRule Spec.UTXOW
liftUTXOW = liftRule . ctxtUTXOW

-- | Apply update proposal
liftUPIREG :: ByronSpecGenesis -> LiftedRule Spec.UPIREG
liftUPIREG = liftRule . ctxtUPIREG

-- | Apply update vote
liftUPIVOTE :: ByronSpecGenesis -> LiftedRule Spec.UPIVOTE
liftUPIVOTE = liftRule . ctxtUPIVOTE

-- | Apply the epoch transition rule
--
-- This is used in 'applyChainTick' only.
liftEPOCH :: ByronSpecGenesis -> LiftedRule Spec.EPOCH
liftEPOCH = liftRule . ctxtEPOCH

-- | Apply top-level delegation rule
--
-- This is used in 'applyChainTick' only
liftDELEG :: ByronSpecGenesis -> LiftedRule Spec.DELEG
liftDELEG = liftRule . ctxtDELEG

{-------------------------------------------------------------------------------
  Infrastructure for working with the STS transitions
-------------------------------------------------------------------------------}

-- | Context required to apply a rule to the top-level CHAIN state
--
-- The environment  for these rules pull in some information from the (abstract)
-- genesis config and some information from the state; the reason is that
-- although some values come from the state, as far as these rules are
-- concerned, they are constants.
data RuleContext sts = RuleContext {
      getRuleState :: GetChainState (Spec.State sts)
    , modRuleState :: ModChainState (Spec.State sts)
    , liftFailure  :: Spec.PredicateFailure sts -> Spec.PredicateFailure Spec.CHAIN
    , getRuleEnv   :: Spec.State Spec.CHAIN -> Spec.Environment sts
    }

applySTS :: forall sts. (Spec.STS sts, Spec.BaseM sts ~ Identity)
         => Proxy sts
         -> Spec.Environment sts
         -> Spec.Signal sts
         -> Spec.State sts
         -> Except [[Spec.PredicateFailure sts]] (Spec.State sts)
applySTS _ env signal state = except $
    Spec.applySTS @sts $ Spec.TRC (env, state, signal)

type LiftedRule sts = Spec.Signal sts
                   -> Spec.State Spec.CHAIN
                   -> Except [[Spec.PredicateFailure Spec.CHAIN]]
                             (Spec.State Spec.CHAIN)

-- | Lift sub-STS rule to top-level CHAIN
liftRule :: forall sts. (Spec.STS sts, Spec.BaseM sts ~ Identity)
         => RuleContext sts -> LiftedRule sts
liftRule RuleContext{..} signal st =
    withExcept (map (map liftFailure)) $
      modRuleState (applySTS (Proxy @sts) (getRuleEnv st) signal) st

{-------------------------------------------------------------------------------
  Instances of 'RuleContext'
-------------------------------------------------------------------------------}

ctxtCHAIN :: ByronSpecGenesis -> RuleContext Spec.CHAIN
ctxtCHAIN cfg = RuleContext {
      getRuleState = id
    , modRuleState = id
    , liftFailure  = id
    , getRuleEnv   = \_st -> Genesis.toChainEnv cfg
    }

ctxtEPOCH :: ByronSpecGenesis -> RuleContext Spec.EPOCH
ctxtEPOCH ByronSpecGenesis{..} = RuleContext {
      getRuleState = getChainStateUPIState
    , modRuleState = modChainStateUPIState
    , liftFailure  = Spec.EpochFailure
    , getRuleEnv   = \st -> (
          -- The _current_ epoch
          -- This is needed to detect if the new slot introduces the next epoch
          Spec.sEpoch (getChainStateSlot st) byronSpecGenesisSecurityParam
        , byronSpecGenesisSecurityParam
        )
    }

ctxtDELEG :: ByronSpecGenesis -> RuleContext Spec.DELEG
ctxtDELEG ByronSpecGenesis{..} = RuleContext {
      getRuleState = getChainStateDIState
    , modRuleState = modChainStateDIState
    , liftFailure  = Spec.LedgerDelegationFailure
    , getRuleEnv   = \st -> Spec.DSEnv {
          _dSEnvAllowedDelegators = byronSpecGenesisDelegators
        , _dSEnvEpoch             = Spec.sEpoch
                                      (getChainStateSlot st)
                                      byronSpecGenesisSecurityParam
        , _dSEnvSlot              = getChainStateSlot st
        , _dSEnvK                 = byronSpecGenesisSecurityParam
        }
    }

ctxtSDELEG :: ByronSpecGenesis -> RuleContext Spec.SDELEG
ctxtSDELEG cfg = RuleContext {
      getRuleState = getDIStateDSState . getRuleState (ctxtDELEG cfg)
    , modRuleState = modRuleState (ctxtDELEG cfg) . modDIStateDSState
    , liftFailure  = liftFailure (ctxtDELEG cfg)
                   . Spec.SDelegSFailure
                   . Spec.SDelegFailure
    , getRuleEnv   = getRuleEnv (ctxtDELEG cfg)
    }

ctxtUTXOW :: ByronSpecGenesis -> RuleContext Spec.UTXOW
ctxtUTXOW ByronSpecGenesis{..} = RuleContext {
       getRuleState = getChainStateUtxoState
     , modRuleState = modChainStateUtxoState
     , liftFailure  = Spec.LedgerUTxOFailure
                    . Spec.UtxowFailure
     , getRuleEnv   = \st -> Spec.UTxOEnv {
          utxo0 = byronSpecGenesisInitUtxo
        , pps   = Spec.protocolParameters (getChainStateUPIState st)
        }
     }

ctxtUPIREG :: ByronSpecGenesis -> RuleContext Spec.UPIREG
ctxtUPIREG ByronSpecGenesis{..} = RuleContext {
      getRuleState = getChainStateUPIState
    , modRuleState = modChainStateUPIState
    , liftFailure  = Spec.BBodyFailure
                   . Spec.BUPIFailure
                   . Spec.UPIREGFailure
    , getRuleEnv   = \st -> (
          getChainStateSlot st
        , Spec._dIStateDelegationMap (getChainStateDIState st)
        , byronSpecGenesisSecurityParam
        , fromIntegral $ Set.size byronSpecGenesisDelegators
        )
    }

ctxtUPIVOTE :: ByronSpecGenesis -> RuleContext Spec.UPIVOTE
ctxtUPIVOTE cfg = RuleContext {
      getRuleState = getRuleState (ctxtUPIREG cfg)
    , modRuleState = modRuleState (ctxtUPIREG cfg)
    , getRuleEnv   = getRuleEnv   (ctxtUPIREG cfg)
    , liftFailure  = Spec.BBodyFailure
                   . Spec.BUPIFailure
                   . Spec.UPIVOTESFailure
                   . Spec.ApplyVotesFailure
                   . Spec.UpivoteFailure
    }

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
