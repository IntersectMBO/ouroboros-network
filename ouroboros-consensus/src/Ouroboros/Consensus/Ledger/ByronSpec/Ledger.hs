{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DerivingVia     #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}

{-# OPTIONS -fno-warn-orphans #-}

module Ouroboros.Consensus.Ledger.ByronSpec.Ledger (
    ByronSpecLedgerError(..)
  , initByronSpecLedgerState
  , updateByronSpecLedgerStateKeepTip
  , updateByronSpecLedgerStateNewTip
    -- * Type family instances
  , LedgerState(..)
  , LedgerConfig(..)
  ) where

import           Codec.Serialise
import           Control.Monad.Except
import           Control.Monad.Trans.Except
import           Data.Bifunctor
import           GHC.Generics (Generic)

import           Cardano.Prelude (AllowThunk (..), NoUnexpectedThunks)

import qualified Cardano.Ledger.Spec.STS.UTXO as Spec
import qualified Cardano.Spec.Chain.STS.Rule.Chain as Spec
import qualified Control.State.Transition as Spec

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.ByronSpec.Block
import qualified Ouroboros.Consensus.Ledger.ByronSpec.ChainState as ChainState
import           Ouroboros.Consensus.Ledger.ByronSpec.Conversions
import           Ouroboros.Consensus.Ledger.ByronSpec.Genesis (ByronSpecGenesis)
import           Ouroboros.Consensus.Ledger.ByronSpec.Orphans ()
import qualified Ouroboros.Consensus.Ledger.ByronSpec.Rules as Rules

newtype ByronSpecLedgerError = ByronSpecLedgerError {
      unByronSpecLedgerError :: [[Spec.PredicateFailure Spec.CHAIN]]
    }
  deriving (Show, Eq)
  deriving NoUnexpectedThunks via AllowThunk ByronSpecLedgerError

instance UpdateLedger ByronSpecBlock where
  data LedgerState ByronSpecBlock = ByronSpecLedgerState {
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

  newtype LedgerConfig ByronSpecBlock = ByronSpecLedgerConfig {
        unByronSpecLedgerConfig :: ByronSpecGenesis
      }

  type LedgerError ByronSpecBlock = ByronSpecLedgerError

  applyChainTick cfg slot state = TickedLedgerState $
      updateByronSpecLedgerStateNewTip slot $
        Rules.applyChainTick
          (unByronSpecLedgerConfig cfg)
          (toByronSpecSlotNo       slot)
          (byronSpecLedgerState    state)

  applyLedgerBlock cfg block state = except $
      bimap ByronSpecLedgerError
            (updateByronSpecLedgerStateNewTip (blockSlot block)) $
        Rules.liftCHAIN
          (unByronSpecLedgerConfig cfg)
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
                       (ChainState.getHash (byronSpecLedgerState state))

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
