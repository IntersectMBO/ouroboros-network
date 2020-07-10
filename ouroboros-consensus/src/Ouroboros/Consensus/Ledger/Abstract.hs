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
    ApplyBlock(..)
  , UpdateLedger
    -- ** Derived
  , tickThenApply
  , tickThenReapply
  , foldLedger
  , refoldLedger
    -- ** Short-hand
  , ledgerTipHash
  , ledgerTipPoint
  , ledgerTipSlot
    -- * Queries
  , Query
  , QueryLedger(..)
  , ShowQuery(..)
    -- * Re-exports
  , module Ouroboros.Consensus.Ledger.Basics
  ) where

import           Control.Monad.Except
import           Data.Maybe (isJust)
import           Data.Proxy
import           GHC.Stack (HasCallStack)

import           Ouroboros.Network.Protocol.LocalStateQuery.Type
                     (ShowQuery (..))

import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Basics
import           Ouroboros.Consensus.Ticked
import           Ouroboros.Consensus.Util (repeatedly, repeatedlyM)
import           Ouroboros.Consensus.Util.DepPair

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
                   => FullBlockConfig l blk
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
                     => FullBlockConfig l blk -> blk -> Ticked l -> l

-- | Interaction with the ledger layer
class ApplyBlock (LedgerState blk) blk => UpdateLedger blk

{-------------------------------------------------------------------------------
  Derived functionality
-------------------------------------------------------------------------------}

tickThenApply :: ApplyBlock l blk
              => FullBlockConfig l blk -> blk -> l -> Except (LedgerErr l) l
tickThenApply cfg blk =
      applyLedgerBlock cfg blk
    . applyChainTick (blockConfigLedger cfg) (blockSlot blk)

tickThenReapply :: ApplyBlock l blk
                => FullBlockConfig l blk -> blk -> l -> l
tickThenReapply cfg blk =
      reapplyLedgerBlock cfg blk
    . applyChainTick (blockConfigLedger cfg) (blockSlot blk)

foldLedger :: ApplyBlock l blk
           => FullBlockConfig l blk -> [blk] -> l -> Except (LedgerErr l) l
foldLedger = repeatedlyM . tickThenApply

refoldLedger :: ApplyBlock l blk
             => FullBlockConfig l blk -> [blk] -> l -> l
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

{-------------------------------------------------------------------------------
  Queries
-------------------------------------------------------------------------------}

-- | Different queries supported by the ledger, indexed by the result type.
data family Query blk :: * -> *

-- | Query the ledger state.
--
-- Used by the LocalStateQuery protocol to allow clients to query the ledger
-- state.
class (ShowQuery (Query blk), SameDepIndex (Query blk)) => QueryLedger blk where

  -- | Answer the given query about the ledger state.
  answerQuery :: LedgerConfig blk -> Query blk result -> LedgerState blk -> result

instance SameDepIndex (Query blk) => Eq (SomeBlock Query blk) where
  SomeBlock qry == SomeBlock qry' = isJust (sameDepIndex qry qry')

deriving instance (forall result. Show (Query blk result)) => Show (SomeBlock Query blk)
