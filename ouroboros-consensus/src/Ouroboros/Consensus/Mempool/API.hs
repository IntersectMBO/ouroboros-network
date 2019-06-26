{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Ouroboros.Consensus.Mempool.API (
    Mempool(..)
  , ApplyTx(..)
  ) where

import           Control.Monad.Except
import           GHC.Stack (HasCallStack)

import           Control.Monad.Class.MonadSTM

import           Ouroboros.Consensus.Ledger.Abstract

class UpdateLedger blk => ApplyTx blk where
  -- | Generalized transaction
  --
  -- The mempool (and, accordingly, blocks) consist of "generalized
  -- transactions"; this could be "proper" transactions (transferring funds) but
  -- also other kinds of things such as update proposals, delegations, etc.
  data family GenTx blk :: *

  -- | A generalized transaction, 'GenTx', identifier.
  data family GenTxId blk :: *

  -- | Updating the ledger with a single transaction may result in a different
  -- error type as when updating it with a block
  type family ApplyTxErr blk :: *

  -- | Apply transaction we have not previously seen before
  applyTx :: LedgerConfig blk
          -> GenTx blk
          -> LedgerState blk
          -> Except (ApplyTxErr blk) (LedgerState blk)

  -- | Re-apply a transaction
  --
  -- When we re-apply a transaction to a potentially different ledger state
  -- expensive checks such as cryptographic hashes can be skipped, but other
  -- checks (such as checking for double spending) must still be done.
  reapplyTx :: HasCallStack
            => LedgerConfig blk
            -> GenTx blk
            -> LedgerState blk
            -> Except (ApplyTxErr blk) (LedgerState blk)

  -- | Re-apply a transaction to the very same state it was applied in before
  --
  -- In this case no error can occur.
  --
  -- See also 'ldbConfReapply' for comments on implementing this function.
  reapplyTxSameState :: HasCallStack
                     => LedgerConfig blk
                     -> GenTx blk
                     -> LedgerState blk
                     -> LedgerState blk

-- | Mempool
--
-- The mempool is the set of transactions that should be included in the next
-- block. In principle this is a /set/ of all the transactions that we receive
-- from our peers. In order to avoid flooding the network with invalid
-- transactions,  however, we only want to keep /valid/ transactions in the
-- mempool. That raises the question: valid with respect to which ledger state?
--
-- We opt for a very simple answer to this: the mempool will be interpreted
-- as a /list/ of transactions; which are validated strictly in order, starting
-- from the current ledger state. This has a number of advantages:
--
-- * It's simple to implement and it's efficient. In particular, no search for
--   a valid subset is ever required.
-- * When producing a block, we can simply take the longest possible prefix
--   of transactions that fits in a block.
-- * It supports wallets that submit dependent transactions (where later
--   transaction depends on outputs from earlier ones).
data Mempool m blk idx = Mempool {
      -- | Add a bunch of transactions (oldest to newest)
      --
      -- As long as we keep the mempool entirely in-memory this could live in
      -- @STM m@; we keep it in @m@ instead to leave open the possibility of
      -- persistence.
      --
      -- The transactions will be validated, /in order/, before they are added
      -- to the mempool. Invalid transactions will be returned along with the
      -- validation error. In principle it is possible that such validation
      -- errors are transient; for example, it is possible that a transaction is
      -- rejected because one of its inputs is not /yet/ available in the UTxO
      -- (the transaction it depends on is not yet in the chain, nor in the
      -- mempool). In practice however it is likely that rejected transactions
      -- will still be rejected later, and should just be dropped.
      --
      -- It is important to note one important special case of transactions
      -- being "invalid": a transaction will /also/ be considered invalid if
      -- /that very same transaction/ is already included on the blockchain
      -- (after all, by definition that must mean its inputs have been used).
      -- Rejected transactions are therefore not necessarily a sign of
      -- malicious behaviour. Indeed, we would expect /most/ transactions that
      -- are reported as invalid by 'addTxs' to be invalid precisely because
      -- they have already been included. (Distinguishing between these two
      -- cases can be done in theory, but it is expensive unless we have an
      -- index of transaction hashes that have been included on the blockchain.)
      addTxs :: [(GenTxId blk, GenTx blk)] -> m [(GenTx blk, ApplyTxErr blk)]

      -- | Sync the transactions in the mempool with the ledger state of the
      -- 'ChainDB'. Invalid transactions will be returned along with the
      -- validation error.
    , syncState :: STM m [(GenTx blk, ApplyTxErr blk)]

      -- | Get all transactions in the mempool along with their associated
      -- ticket numbers (oldest to newest).
      --
      -- n.b. This function provides /no guarantee/ that the resulting
      -- transactions will be valid with respect to the ledger state of the
      -- 'ChainDB'. However, it is guaranteed that these transactions will be
      -- valid (with respect to the ledger state) if this function is called
      -- immediately following a call to 'syncState', /within the same
      -- transaction/.
    , getTxs :: STM m [(GenTxId blk, GenTx blk, idx)]

      -- | Get all transactions in the mempool, along with their associated
      -- ticket numbers, which are associated with a ticket number greater
      -- than the one provided.
    , getTxsAfter :: idx -> STM m [(GenTxId blk, GenTx blk, idx)]

      -- | Get a specific transaction from the mempool by its ticket number,
      -- if it exists.
    , getTx :: idx -> STM m (Maybe (GenTxId blk, GenTx blk))

      -- | Represents the initial value at which the transaction ticket number
      -- counter will start (i.e. the zeroth ticket number).
    , zeroIdx :: idx
    }
