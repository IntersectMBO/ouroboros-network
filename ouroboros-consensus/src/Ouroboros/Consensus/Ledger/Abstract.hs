{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Interface to the ledger layer
module Ouroboros.Consensus.Ledger.Abstract (
    -- * Apply block
    ApplyBlock (..)
  , UpdateLedger
    -- ** Derived
  , foldLedger
  , refoldLedger
  , tickThenApply
  , tickThenReapply
    -- ** Short-hand
  , ledgerTipHash
  , ledgerTipPoint
  , ledgerTipSlot
    -- * Re-exports
  , module Ouroboros.Consensus.Ledger.Basics
  ) where

import           Control.Monad.Except
import           Data.Proxy
import           GHC.Stack (HasCallStack)

import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Ledger.Basics
import           Ouroboros.Consensus.Ticked
import           Ouroboros.Consensus.Util (repeatedly, repeatedlyM)

{-------------------------------------------------------------------------------
  Apply block to ledger state
-------------------------------------------------------------------------------}

class ( IsLedger l
      , HeaderHash l ~ HeaderHash blk
      , HasHeader blk
      , HasHeader (Header blk)
      ) => ApplyBlock l blk where

  -- | Apply a block to the ledger state.
  --
  -- This is passed the ledger state ticked with the slot of the given block,
  -- so 'applyChainTick' has already been called.
  applyLedgerBlock :: HasCallStack
                   => LedgerCfg l
                   -> blk -> Ticked l -> Except (LedgerErr l) l

  -- | Re-apply a block to the very same ledger state it was applied in before.
  --
  -- Since a block can only be applied to a single, specific, ledger state,
  -- if we apply a previously applied block again it will be applied in the
  -- very same ledger state, and therefore can't possibly fail.
  --
  -- It is worth noting that since we already know that the block is valid in
  -- the provided ledger state, the ledger layer should not perform /any/
  -- validation checks.
  reapplyLedgerBlock :: HasCallStack
                     => LedgerCfg l -> blk -> Ticked l -> l

-- | Interaction with the ledger layer
class ApplyBlock (LedgerState blk) blk => UpdateLedger blk

{-------------------------------------------------------------------------------
  Derived functionality
-------------------------------------------------------------------------------}

tickThenApply :: ApplyBlock l blk
              => LedgerCfg l -> blk -> l -> Except (LedgerErr l) l
tickThenApply cfg blk =
      applyLedgerBlock cfg blk
    . applyChainTick cfg (blockSlot blk)

tickThenReapply :: ApplyBlock l blk
                => LedgerCfg l -> blk -> l -> l
tickThenReapply cfg blk =
      reapplyLedgerBlock cfg blk
    . applyChainTick cfg (blockSlot blk)

foldLedger :: ApplyBlock l blk
           => LedgerCfg l -> [blk] -> l -> Except (LedgerErr l) l
foldLedger = repeatedlyM . tickThenApply

refoldLedger :: ApplyBlock l blk
             => LedgerCfg l -> [blk] -> l -> l
refoldLedger = repeatedly . tickThenReapply

{-------------------------------------------------------------------------------
  Short-hand
-------------------------------------------------------------------------------}

-- | Wrapper around 'ledgerTipPoint' that uses a proxy to fix @blk@
--
-- This is occassionally useful to guide type inference
ledgerTipPoint ::
     UpdateLedger blk
  => Proxy blk -> LedgerState blk -> Point blk
ledgerTipPoint _ = castPoint . getTip

ledgerTipHash ::
     forall blk. UpdateLedger blk
  => LedgerState blk -> ChainHash blk
ledgerTipHash = pointHash . (ledgerTipPoint (Proxy @blk))

ledgerTipSlot ::
     forall blk. UpdateLedger blk
  => LedgerState blk -> WithOrigin SlotNo
ledgerTipSlot = pointSlot . (ledgerTipPoint (Proxy @blk))
