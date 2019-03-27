{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

module Ouroboros.Byron.Proxy.Index.Types where

import Data.Word (Word64)

import Pos.Chain.Block (HeaderHash)

import Ouroboros.Storage.Common (EpochNo)

-- | A point to read from the index. Looking up the tip gives its own hash,
-- looking up anything by hash (could happen to be the tip) gives the hash
-- of its child (nothing iff it's the tip).
data IndexPoint t where
  Tip    :: IndexPoint HeaderHash
  ByHash :: HeaderHash -> IndexPoint ()

data Index m = Index
  { indexRead  :: forall d . IndexPoint d -> m (Maybe (d, EpochNo, IndexSlot))
  , indexWrite :: forall t . (IndexWrite m -> m t) -> m t
  }

data IndexSlot where
  RealSlot :: Word64 -> IndexSlot
  EBBSlot  :: IndexSlot

data IndexWrite m = IndexWrite
  { updateTip :: HeaderHash -> EpochNo -> IndexSlot -> m ()
  }
