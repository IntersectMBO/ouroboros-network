{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.HardFork.Combinator.Ledger.CommonProtocolParams () where

import           Data.SOP.Strict

import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.CommonProtocolParams
import           Ouroboros.Consensus.Ledger.SupportsHD

import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.Basics
import           Ouroboros.Consensus.HardFork.Combinator.Ledger ()
import qualified Ouroboros.Consensus.HardFork.Combinator.State as State
import           Ouroboros.Consensus.HardFork.Combinator.Util.Functors

instance ( CanHardFork xs
         , LedgerSupportsHD (LedgerState (HardForkBlock xs))
         , LedgerTablesCanHardFork xs
         ) => CommonProtocolParams (HardForkBlock xs) where
  maxHeaderSize = askCurrentLedger maxHeaderSize
  maxTxSize     = askCurrentLedger maxTxSize

askCurrentLedger
  :: CanHardFork xs
  => (forall blk. CommonProtocolParams blk => LedgerState blk mk -> a)
  -> LedgerState (HardForkBlock xs) mk -> a
askCurrentLedger f =
      hcollapse
    . hcmap proxySingle (K . f . unFlip)
    . State.tip
    . hardForkLedgerStatePerEra
