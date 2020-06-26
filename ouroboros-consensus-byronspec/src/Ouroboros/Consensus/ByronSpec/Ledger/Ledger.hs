{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS -Wno-orphans #-}

module Ouroboros.Consensus.ByronSpec.Ledger.Ledger (
    ByronSpecLedgerError(..)
  , initByronSpecLedgerState
  , updateByronSpecLedgerStateKeepTip
  , updateByronSpecLedgerStateNewTip
    -- * Type family instances
  , LedgerState(..)
  ) where

import           Codec.Serialise
import           Control.Monad.Except
import           GHC.Generics (Generic)

import           Cardano.Prelude (AllowThunk (..), NoUnexpectedThunks)

import qualified Byron.Spec.Chain.STS.Rule.Chain as Spec
import qualified Byron.Spec.Ledger.Update as Spec
import qualified Control.State.Transition as Spec

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.CommonProtocolParams

import           Ouroboros.Consensus.ByronSpec.Ledger.Accessors
import           Ouroboros.Consensus.ByronSpec.Ledger.Block
import           Ouroboros.Consensus.ByronSpec.Ledger.Conversions
import           Ouroboros.Consensus.ByronSpec.Ledger.Genesis (ByronSpecGenesis)
import           Ouroboros.Consensus.ByronSpec.Ledger.Orphans ()
import qualified Ouroboros.Consensus.ByronSpec.Ledger.Rules as Rules

newtype ByronSpecLedgerError = ByronSpecLedgerError {
      unByronSpecLedgerError :: [[Spec.PredicateFailure Spec.CHAIN]]
    }
  deriving (Show, Eq)
  deriving NoUnexpectedThunks via AllowThunk ByronSpecLedgerError

type instance LedgerCfg (LedgerState ByronSpecBlock) = ByronSpecGenesis

instance IsLedger (LedgerState ByronSpecBlock) where
  type LedgerErr (LedgerState ByronSpecBlock) = ByronSpecLedgerError

  applyChainTick cfg slot state = Ticked slot $
      updateByronSpecLedgerStateKeepTip state $
        Rules.applyChainTick
          cfg
          (toByronSpecSlotNo       slot)
          (byronSpecLedgerState    state)

instance ApplyBlock (LedgerState ByronSpecBlock) ByronSpecBlock where
  applyLedgerBlock cfg block (Ticked slot state) =
    withExcept ByronSpecLedgerError $
      updateByronSpecLedgerStateNewTip slot <$>
        -- Note that the CHAIN rule also applies the chain tick. So even
        -- though the ledger we received has already been ticked with
        -- 'applyChainTick', we do it again as part of CHAIN. This is safe, as
        -- it is idempotent. If we wanted to avoid the repeated tick, we would
        -- have to call the subtransitions of CHAIN (except for ticking).
        Rules.liftCHAIN
          cfg
          (byronSpecBlock          block)
          (byronSpecLedgerState    state)

  reapplyLedgerBlock cfg block =
      -- The spec doesn't have a "reapply" mode
      dontExpectError . applyLedgerBlock cfg block
    where
      dontExpectError :: Except a b -> b
      dontExpectError mb = case runExcept mb of
        Left  _ -> error "reapplyLedgerBlock: unexpected error"
        Right b -> b

  ledgerTipPoint state =
      case byronSpecLedgerTip state of
        Nothing   -> GenesisPoint
        Just slot -> BlockPoint
                       slot
                       (getChainStateHash (byronSpecLedgerState state))

data instance LedgerState ByronSpecBlock = ByronSpecLedgerState {
      -- | Tip of the ledger (most recently applied block, if any)
      --
      -- The spec state stores the last applied /hash/, but not the /slot/.
      byronSpecLedgerTip :: Maybe SlotNo

      -- | The spec state proper
    , byronSpecLedgerState :: Spec.State Spec.CHAIN
    }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Serialise)
  deriving NoUnexpectedThunks via AllowThunk (LedgerState ByronSpecBlock)

instance UpdateLedger ByronSpecBlock

instance CommonProtocolParams ByronSpecBlock where
  maxHeaderSize = fromIntegral . Spec._maxHdrSz . getPParams
  maxTxSize     = fromIntegral . Spec._maxTxSz  . getPParams

getPParams :: LedgerState ByronSpecBlock -> Spec.PParams
getPParams =
      Spec.protocolParameters
    . getChainStateUPIState
    . byronSpecLedgerState

{-------------------------------------------------------------------------------
  Working with the ledger state
-------------------------------------------------------------------------------}

initByronSpecLedgerState :: ByronSpecGenesis -> LedgerState ByronSpecBlock
initByronSpecLedgerState cfg = ByronSpecLedgerState {
      byronSpecLedgerTip   = Nothing
    , byronSpecLedgerState = Rules.initStateCHAIN cfg
    }

updateByronSpecLedgerStateKeepTip :: LedgerState ByronSpecBlock
                                  -> Spec.State Spec.CHAIN
                                  -> LedgerState ByronSpecBlock
updateByronSpecLedgerStateKeepTip st ledger = ByronSpecLedgerState {
      byronSpecLedgerTip   = byronSpecLedgerTip st
    , byronSpecLedgerState = ledger
    }

updateByronSpecLedgerStateNewTip :: SlotNo
                                 -> Spec.State Spec.CHAIN
                                 -> LedgerState ByronSpecBlock
updateByronSpecLedgerStateNewTip newTipSlot ledger = ByronSpecLedgerState {
      byronSpecLedgerTip   = Just newTipSlot
    , byronSpecLedgerState = ledger
    }
