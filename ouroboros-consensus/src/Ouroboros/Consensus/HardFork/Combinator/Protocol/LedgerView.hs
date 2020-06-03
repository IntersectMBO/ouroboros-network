{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}

module Ouroboros.Consensus.HardFork.Combinator.Protocol.LedgerView (
    -- * Hard fork
    HardForkLedgerView_(..)
  , HardForkLedgerView
  ) where

import           Data.SOP.Dict
import           Data.SOP.Strict
import           Data.Void

import           Ouroboros.Consensus.TypeFamilyWrappers

import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.State.Instances ()
import           Ouroboros.Consensus.HardFork.Combinator.State.Types

{-------------------------------------------------------------------------------
  HardForkLedgerView
-------------------------------------------------------------------------------}

data HardForkLedgerView_ f xs = HardForkLedgerView {
      -- | Information about the transition to the next era, if known
      hardForkLedgerViewTransition :: !TransitionInfo

      -- | The underlying ledger view
      --
      -- We do not need snapshots for the past eras, and so we use 'Void'.
    , hardForkLedgerViewPerEra     :: !(HardForkState_ (K Void) f xs)
    }

deriving instance CanHardFork xs => Show (HardForkLedgerView_ WrapLedgerView xs)

type HardForkLedgerView = HardForkLedgerView_ WrapLedgerView

{-------------------------------------------------------------------------------
  Show instance for the benefit of tests
-------------------------------------------------------------------------------}

instance (SListI xs, Show a) => Show (HardForkLedgerView_ (K a) xs) where
  show HardForkLedgerView{..} =
      case (dictPast, dictCurrent) of
        (Dict, Dict) -> show (
            hardForkLedgerViewTransition
          , getHardForkState hardForkLedgerViewPerEra
          )
    where
      dictPast :: Dict (All (Compose Show (Past (K Void)))) xs
      dictPast = all_NP $ hpure Dict

      dictCurrent :: Dict (All (Compose Show (Current (K a)))) xs
      dictCurrent = all_NP $ hpure Dict
