{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.HardFork.Combinator.Ledger.CommonProtocolParams (

  ) where

import           Data.SOP.Strict

import           Ouroboros.Consensus.Ledger.CommonProtocolParams

import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.Basics
import           Ouroboros.Consensus.HardFork.Combinator.Ledger ()
import qualified Ouroboros.Consensus.HardFork.Combinator.State as State

instance CanHardFork xs => CommonProtocolParams (HardForkBlock xs) where
  maxHeaderSize = askCurrentLedger maxHeaderSize
  maxTxSize     = askCurrentLedger maxTxSize

askCurrentLedger
  :: CanHardFork xs
  => (forall blk. CommonProtocolParams blk => LedgerState blk -> a)
  -> LedgerState (HardForkBlock xs) -> a
askCurrentLedger f =
      hcollapse
    . hcmap proxySingle (K . f)
    . State.tip
    . hardForkLedgerStatePerEra
