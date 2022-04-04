{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS -Wno-orphans #-}

module Ouroboros.Consensus.ByronSpec.Ledger.Ledger (
    ByronSpecLedgerError (..)
  , initByronSpecLedgerState
    -- * Type family instances
  , LedgerState (..)
  , LedgerTables (..)
  , Ticked1 (..)
  ) where

import           Codec.Serialise
import           Control.Monad.Except
import           GHC.Generics (Generic)
import           NoThunks.Class (AllowThunk (..), NoThunks)

import qualified Byron.Spec.Chain.STS.Rule.Chain as Spec
import qualified Byron.Spec.Ledger.Update as Spec
import qualified Control.State.Transition as Spec

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.CommonProtocolParams
import           Ouroboros.Consensus.Ticked
import           Ouroboros.Consensus.Util ((..:))

import           Ouroboros.Consensus.ByronSpec.Ledger.Accessors
import           Ouroboros.Consensus.ByronSpec.Ledger.Block
import           Ouroboros.Consensus.ByronSpec.Ledger.Conversions
import           Ouroboros.Consensus.ByronSpec.Ledger.Genesis (ByronSpecGenesis)
import           Ouroboros.Consensus.ByronSpec.Ledger.Orphans ()
import qualified Ouroboros.Consensus.ByronSpec.Ledger.Rules as Rules

{-------------------------------------------------------------------------------
  State
-------------------------------------------------------------------------------}

data instance LedgerState ByronSpecBlock mk = ByronSpecLedgerState {
      -- | Tip of the ledger (most recently applied block, if any)
      --
      -- The spec state stores the last applied /hash/, but not the /slot/.
      byronSpecLedgerTip :: Maybe SlotNo

      -- | The spec state proper
    , byronSpecLedgerState :: Spec.State Spec.CHAIN
    }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Serialise)
  deriving NoThunks via AllowThunk (LedgerState ByronSpecBlock mk)

newtype ByronSpecLedgerError = ByronSpecLedgerError {
      unByronSpecLedgerError :: [Spec.PredicateFailure Spec.CHAIN]
    }
  deriving (Show, Eq)
  deriving NoThunks via AllowThunk ByronSpecLedgerError

type instance LedgerCfg (LedgerState ByronSpecBlock) = ByronSpecGenesis

instance UpdateLedger ByronSpecBlock

initByronSpecLedgerState :: ByronSpecGenesis -> LedgerState ByronSpecBlock mk
initByronSpecLedgerState cfg = ByronSpecLedgerState {
      byronSpecLedgerTip   = Nothing
    , byronSpecLedgerState = Rules.initStateCHAIN cfg
    }

{-------------------------------------------------------------------------------
  GetTip
-------------------------------------------------------------------------------}

instance GetTip (LedgerState ByronSpecBlock mk) where
  getTip (ByronSpecLedgerState tip state) = castPoint $
      getByronSpecTip tip state

instance GetTip (Ticked1 (LedgerState ByronSpecBlock) mk) where
  getTip (TickedByronSpecLedgerState tip state) = castPoint $
      getByronSpecTip tip state

getByronSpecTip :: Maybe SlotNo -> Spec.State Spec.CHAIN -> Point ByronSpecBlock
getByronSpecTip Nothing     _     = GenesisPoint
getByronSpecTip (Just slot) state = BlockPoint
                                      slot
                                      (getChainStateHash state)

{-------------------------------------------------------------------------------
  Ticking
-------------------------------------------------------------------------------}

data instance Ticked1 (LedgerState ByronSpecBlock) mk = TickedByronSpecLedgerState {
      untickedByronSpecLedgerTip :: Maybe SlotNo
    , tickedByronSpecLedgerState :: Spec.State Spec.CHAIN
    }
  deriving stock (Show, Eq)
  deriving NoThunks via AllowThunk (Ticked1 (LedgerState ByronSpecBlock) mk)

instance IsLedger (LedgerState ByronSpecBlock) where
  type LedgerErr (LedgerState ByronSpecBlock) = ByronSpecLedgerError

  type AuxLedgerEvent (LedgerState ByronSpecBlock) =
    VoidLedgerEvent (LedgerState ByronSpecBlock)

  applyChainTickLedgerResult cfg slot (ByronSpecLedgerState tip state) =
        pureLedgerResult
      $ TickedByronSpecLedgerState {
            untickedByronSpecLedgerTip = tip
          , tickedByronSpecLedgerState = Rules.applyChainTick
                                           cfg
                                           (toByronSpecSlotNo slot)
                                           state
          }

instance ShowLedgerState (LedgerState ByronSpecBlock) where
  showsLedgerState _sing = shows

instance TableStuff (LedgerState ByronSpecBlock) where
  newtype LedgerTables (LedgerState ByronSpecBlock) mk = NoByronSpecLedgerTables (SMapKind mk)
    deriving (Generic, Eq, Show, NoThunks)

  -- TODO methods

instance TickedTableStuff (LedgerState ByronSpecBlock) where

  -- TODO methods

instance ShowLedgerState (LedgerTables (LedgerState ByronSpecBlock)) where
  showsLedgerState _sing = shows

instance TableStuff (Ticked1 (LedgerState ByronSpecBlock)) where
  newtype LedgerTables (Ticked1 (LedgerState ByronSpecBlock)) mk = TickedNoByronSpecLedgerTables (SMapKind mk)
    deriving (Generic, Eq, Show, NoThunks)

  -- TODO methods

instance ShowLedgerState (LedgerTables (Ticked1 (LedgerState ByronSpecBlock))) where
  showsLedgerState _sing = shows

{-------------------------------------------------------------------------------
  Applying blocks
-------------------------------------------------------------------------------}

instance ApplyBlock (LedgerState ByronSpecBlock) ByronSpecBlock where
  applyBlockLedgerResult cfg block (TickedByronSpecLedgerState _tip state) =
        withExcept ByronSpecLedgerError
      $ fmap (pureLedgerResult . ByronSpecLedgerState (Just (blockSlot block)))
      $ -- Note that the CHAIN rule also applies the chain tick. So even
        -- though the ledger we received has already been ticked with
        -- 'applyChainTick', we do it again as part of CHAIN. This is safe, as
        -- it is idempotent. If we wanted to avoid the repeated tick, we would
        -- have to call the subtransitions of CHAIN (except for ticking).
        Rules.liftCHAIN
          cfg
          (byronSpecBlock block)
          state

  reapplyBlockLedgerResult =
      -- The spec doesn't have a "reapply" mode
      dontExpectError ..: applyBlockLedgerResult
    where
      dontExpectError :: Except a b -> b
      dontExpectError mb = case runExcept mb of
        Left  _ -> error "reapplyBlockLedgerResult: unexpected error"
        Right b -> b

{-------------------------------------------------------------------------------
  CommonProtocolParams
-------------------------------------------------------------------------------}

instance CommonProtocolParams ByronSpecBlock where
  maxHeaderSize = fromIntegral . Spec._maxHdrSz . getPParams
  maxTxSize     = fromIntegral . Spec._maxTxSz  . getPParams

getPParams :: LedgerState ByronSpecBlock mk -> Spec.PParams
getPParams =
      Spec.protocolParameters
    . getChainStateUPIState
    . byronSpecLedgerState
