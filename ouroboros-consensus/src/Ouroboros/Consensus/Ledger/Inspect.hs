{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Ouroboros.Consensus.Ledger.Inspect (
    InspectLedger(..)
  ) where

import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract

class ( Show (LedgerWarning blk)
      , Eq   (LedgerWarning blk)
      ) => InspectLedger blk where
  type LedgerWarning blk :: *

  -- | Inspect the ledger
  --
  -- The point of the inspection is to see if the state of the ledger might
  -- indicate a potential misconfiguration of the node.
  --
  -- TODO: We might at some point need to generalize this to 'ExtLedgerState'
  -- instead. That doesn't fit quite so neatly with the HFC at present, so
  -- leaving it at this for now.
  inspectLedger :: TopLevelConfig blk
                -> LedgerState blk
                -> [LedgerWarning blk]
