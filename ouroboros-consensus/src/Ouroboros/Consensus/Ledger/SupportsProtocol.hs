module Ouroboros.Consensus.Ledger.SupportsProtocol (
    LedgerSupportsProtocol(..)
  , AnachronyFailure(..)
  , anachronisticProtocolLedgerView
  , lemma_protocolLedgerView_anachronistic_applyChainTick
  ) where

import           Control.Monad.Except
import           GHC.Stack (HasCallStack)

import           Ouroboros.Network.Block (SlotNo)
import           Ouroboros.Network.Point

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util.Assert

-- | Link protocol to ledger
class ( BlockSupportsProtocol blk
      , UpdateLedger          blk
      , ValidateEnvelope      blk
      ) => LedgerSupportsProtocol blk where
  -- | Extract ledger view from the ledger state
  protocolLedgerView :: LedgerConfig blk
                     -> LedgerState blk
                     -> LedgerView (BlockProtocol blk)

  -- | Get a ledger view for a specific slot
  --
  -- Suppose @k = 4@, i.e., we can roll back 4 blocks
  --
  -- >             /-----------\
  -- >             |           ^
  -- >             v           |
  -- >     --*--*--*--*--*--*--*--
  -- >          |  A           B
  -- >          |
  -- >          \- A'
  --
  -- In other words, this means that we can roll back from point B to point A,
  -- and then roll forward to any block on any fork from A. Note that we can
  -- /not/ roll back to any siblings of A (such as A'), as that would require
  -- us to roll back at least @k + 1@ blocks, which we can't (by definition).
  --
  -- Given a ledger state at point B, we should be able to verify any of the
  -- headers (corresponding to the blocks) at point A or any of its successors
  -- on any fork, up to some maximum distance from A. This distance can be
  -- determined by the ledger, though must be at least @k@: we must be able to
  -- validate any of these past headers, since otherwise we would not be able to
  -- switch to a fork. It is not essential that the maximum distance extends
  -- into the future (@> k@), though it is helpful: it means that in the chain
  -- sync client we can download and validate headers even if they don't fit
  -- directly onto the tip of our chain.
  --
  -- The anachronistic ledger state at point B is precisely the ledger state
  -- that can be used to validate this set of headers.
  --
  -- Invariant: when calling this function with slot @s@ yields a
  -- 'SlotBounded' @sb@, then @'atSlot' sb@ yields a 'Just'.
  anachronisticProtocolLedgerView_
    :: LedgerConfig blk
    -> LedgerState blk
    -> WithOrigin SlotNo -- ^ Slot for which you would like a ledger view
    -> Except AnachronyFailure (LedgerView (BlockProtocol blk))

-- | See 'anachronisticProtocolLedgerView'.
data AnachronyFailure
  = TooFarAhead
  | TooFarBehind
  deriving (Eq,Show)

-- | Variant of 'anachronisticProtocolLedgerView_' which checks (when
-- assertions are enabled) whether
-- 'lemma_protocolLedgerView_anachronistic_applyChainTick' holds.
--
-- Use this function instead of 'anachronisticProtocolLedgerView_'.
anachronisticProtocolLedgerView
  :: (LedgerSupportsProtocol blk, HasCallStack)
  => LedgerConfig blk
  -> LedgerState blk
  -> WithOrigin SlotNo -- ^ Slot for which you would like a ledger view
  -> Except AnachronyFailure (LedgerView (BlockProtocol blk))
anachronisticProtocolLedgerView cfg st slot =
    assertWithMsg
      (lemma_protocolLedgerView_anachronistic_applyChainTick cfg st slot)
      (anachronisticProtocolLedgerView_ cfg st slot)

-- | Lemma:
--
-- > for all (At s) >= ledgerTipSlot st,
-- >   Right st' = anachronisticProtocolLedgerView_ cfg st (At s) ->
-- >   st' == protocolLedgerView cfg (tickedLedgerState (applyChainTick cfg s st))
--
-- This should be true for each ledger because consensus depends on it.
lemma_protocolLedgerView_anachronistic_applyChainTick
  :: LedgerSupportsProtocol blk
  => LedgerConfig blk
  -> LedgerState blk
  -> WithOrigin SlotNo -- ^ Slot for which you would like a ledger view
  -> Either String ()
lemma_protocolLedgerView_anachronistic_applyChainTick cfg st slot
    | slot >= ledgerTipSlot st
    , At s <- slot
    , let lhs = anachronisticProtocolLedgerView_ cfg st slot
          rhs =
              protocolLedgerView cfg
            . tickedLedgerState
            . applyChainTick cfg s
            $ st
    , Right lhs' <- runExcept lhs
    , lhs' /= rhs
    = Left $ unlines
      [ "anachronisticProtocolLedgerView /= protocolLedgerView . applyChainTick:"
      , show lhs'
      , " /= "
      , show rhs
      ]
    | otherwise
    = Right ()
