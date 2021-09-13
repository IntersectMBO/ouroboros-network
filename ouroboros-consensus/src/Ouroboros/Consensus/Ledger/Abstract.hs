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
    -- * Type-level validation marker
    Validated
    -- * Apply block
  , ApplyBlock (..)
  , UpdateLedger
    -- * Derived
  , applyLedgerBlock
  , foldLedger
  , reapplyLedgerBlock
  , refoldLedger
  , tickThenApply
  , tickThenApplyLedgerResult
  , tickThenReapply
  , tickThenReapplyLedgerResult
    -- ** Short-hand
  , ledgerTipHash
  , ledgerTipPoint
  , ledgerTipSlot
    -- * Re-exports
  , module Ouroboros.Consensus.Ledger.Basics
  ) where

import           Control.Monad.Except
import           Data.Kind (Type)
import           Data.Proxy
import           GHC.Stack (HasCallStack)

import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Ledger.Basics
import           Ouroboros.Consensus.Ticked
import           Ouroboros.Consensus.Util (repeatedly, repeatedlyM, (..:))

-- | " Validated " transaction or block
--
-- The ledger defines how to validate transactions and blocks. It's possible the
-- type before and after validation may be distinct (eg Alonzo transactions),
-- which originally motivated this family.
--
-- We also gain the related benefit that certain interface functions, such as
-- those that /reapply/ blocks, can have a more precise type now. TODO
--
-- Similarly, the Node-to-Client mini protocols can explicitly indicate that the
-- client trusts the blocks from the local server, by having the server send
-- 'Validated' blocks to the client. TODO
--
-- Note that validation has different implications for a transaction than for a
-- block. In particular, a validated transaction can be " reapplied " to
-- different ledger states, whereas a validated block must only be " reapplied "
-- to the exact same ledger state (eg as part of rebuilding from an on-disk
-- ledger snapshot).
--
-- Since the ledger defines validation, see the ledger details for concrete
-- examples of what determines the validity (wrt to a 'LedgerState') of a
-- transaction and/or block. Example properties include: a transaction's claimed
-- inputs exist and are still unspent, a block carries a sufficient
-- cryptographic signature, etc.
data family Validated x :: Type

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
  -- This is passed the ledger state ticked with the slot of the given block, so
  -- 'applyChainTickLedgerResult' has already been called.
  applyBlockLedgerResult ::
       HasCallStack
    => LedgerCfg l
    -> blk
    -> Ticked l
    -> Except (LedgerErr l) (LedgerResult l l)

  -- | Re-apply a block to the very same ledger state it was applied in before.
  --
  -- Since a block can only be applied to a single, specific, ledger state,
  -- if we apply a previously applied block again it will be applied in the
  -- very same ledger state, and therefore can't possibly fail.
  --
  -- It is worth noting that since we already know that the block is valid in
  -- the provided ledger state, the ledger layer should not perform /any/
  -- validation checks.
  reapplyBlockLedgerResult ::
       HasCallStack
    => LedgerCfg l
    -> blk
    -> Ticked l
    -> LedgerResult l l

-- | Interaction with the ledger layer
class ApplyBlock (LedgerState blk) blk => UpdateLedger blk

{-------------------------------------------------------------------------------
  Derived functionality
-------------------------------------------------------------------------------}

-- | 'lrResult' after 'applyBlockLedgerResult'
applyLedgerBlock ::
     (ApplyBlock l blk, HasCallStack)
  => LedgerCfg l
  -> blk
  -> Ticked l
  -> Except (LedgerErr l) l
applyLedgerBlock = fmap lrResult ..: applyBlockLedgerResult

-- | 'lrResult' after 'reapplyBlockLedgerResult'
reapplyLedgerBlock ::
     (ApplyBlock l blk, HasCallStack)
  => LedgerCfg l
  -> blk
  -> Ticked l
  -> l
reapplyLedgerBlock = lrResult ..: reapplyBlockLedgerResult

tickThenApplyLedgerResult ::
     ApplyBlock l blk
  => LedgerCfg l
  -> blk
  -> l
  -> Except (LedgerErr l) (LedgerResult l l)
tickThenApplyLedgerResult cfg blk l = do
  let lrTick = applyChainTickLedgerResult cfg (blockSlot blk) l
  lrBlock <-   applyBlockLedgerResult     cfg            blk  (lrResult lrTick)
  pure LedgerResult {
      lrEvents = lrEvents lrTick <> lrEvents lrBlock
    , lrResult = lrResult lrBlock
    }

tickThenReapplyLedgerResult ::
     ApplyBlock l blk
  => LedgerCfg l
  -> blk
  -> l
  -> LedgerResult l l
tickThenReapplyLedgerResult cfg blk l =
  let lrTick  = applyChainTickLedgerResult cfg (blockSlot blk) l
      lrBlock = reapplyBlockLedgerResult   cfg            blk (lrResult lrTick)
  in LedgerResult {
      lrEvents = lrEvents lrTick <> lrEvents lrBlock
    , lrResult = lrResult lrBlock
    }

tickThenApply ::
     ApplyBlock l blk
  => LedgerCfg l
  -> blk
  -> l
  -> Except (LedgerErr l) l
tickThenApply = fmap lrResult ..: tickThenApplyLedgerResult

tickThenReapply ::
     ApplyBlock l blk
  => LedgerCfg l
  -> blk
  -> l
  -> l
tickThenReapply = lrResult ..: tickThenReapplyLedgerResult

foldLedger ::
     ApplyBlock l blk
  => LedgerCfg l -> [blk] -> l -> Except (LedgerErr l) l
foldLedger = repeatedlyM . tickThenApply

refoldLedger ::
     ApplyBlock l blk
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
