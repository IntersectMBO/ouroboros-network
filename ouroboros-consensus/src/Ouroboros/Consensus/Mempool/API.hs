{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Ouroboros.Consensus.Mempool.API (
    Mempool(..)
  , MempoolSnapshot(..)
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

  -- | Given a 'GenTx', compute its 'GenTxId'.
  --
  -- Should be cheap as this will be called often.
  computeGenTxId :: GenTx blk -> GenTxId blk

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
      -- The following validation steps will be performed when adding
      -- transactions to the mempool:
      --
      -- * Transactions which already exist in the mempool are revalidated,
      --   /in order/, against the current ledger state. Existing transactions
      --   which are found to be invalid, with respect to the current ledger
      --   state, are dropped from the mempool, whereas valid transactions
      --   remain in the mempool.
      -- * The new transactions provided will be validated, /in order/,
      --   against the current ledger state. Transactions which are found to
      --   be invalid, with respect to the current ledger state, are dropped,
      --   whereas valid transactions are added to the mempool.
      --
      -- Note that transactions that are invalid, with respect to the current
      -- ledger state, will /never/ be added to the mempool. However, it is
      -- possible that, at a given point in time, transactions which were once
      -- valid but are now invalid, with respect to the current ledger state,
      -- could exist within the mempool until they are revalidated and dropped
      -- from the mempool via a call to either 'addTxs' or 'syncState'.
      --
      -- This function will return a list potentially consisting of a few
      -- different kinds of transactions:
      --
      -- * Those transactions provided which were found to be valid, along
      --   with 'Nothing' for their accompanying @Maybe (ApplyTxErr blk)@
      --   values.
      -- * Those transactions provided which were found to be invalid, along
      --   with their accompanying validation errors.
      -- * Invalid transactions, with respect to the current ledger state,
      --   that were found in the mempool which have now been dropped, along
      --   with their accompanying validation errors.
      --
      -- In principle it is possible that such validation errors are transient;
      -- for example, it is possible that a transaction is rejected because one
      -- of its inputs is not /yet/ available in the UTxO (the transaction it
      -- depends on is not yet in the chain, nor in the mempool). In practice
      -- however it is likely that rejected transactions will still be rejected
      -- later, and should just be dropped.
      --
      -- It is important to note one important special case of transactions
      -- being "invalid": a transaction will /also/ be considered invalid if
      -- /that very same transaction/ is already included on the blockchain
      -- (after all, by definition that must mean its inputs have been used).
      -- Rejected transactions are therefore not necessarily a sign of
      -- malicious behaviour. Indeed, we would expect /most/ transactions that
      -- are reported as invalid by 'addTxs' to be invalid precisely because
      -- they have already been included. Distinguishing between these two
      -- cases can be done in theory, but it is expensive unless we have an
      -- index of transaction hashes that have been included on the blockchain.
      addTxs :: [GenTx blk] -> m [(GenTx blk, Maybe (ApplyTxErr blk))]

      -- | Sync the transactions in the mempool with the current ledger state
      --  of the 'ChainDB'.
      --
      -- The transactions that exist within the mempool will be revalidated
      -- against the current ledger state. Transactions which are found to be
      -- invalid, with respect to the current ledger state, will be dropped
      -- from the mempool, whereas valid transactions will remain.
      --
      -- Transactions which are now invalid, with respect to the current ledger
      -- state, will be returned along with their associated validation errors.
      --
      -- n.b. in our current implementation, when one opens a mempool, we
      -- spawn a thread which performs this action whenever the 'ChainDB' tip
      -- point changes.
    , syncState :: STM m [(GenTx blk, ApplyTxErr blk)]

      -- | Get a snapshot of the current mempool state. This allows for
      -- further pure queries on the snapshot.
    , getSnapshot :: STM m (MempoolSnapshot (GenTxId blk) (GenTx blk) idx)

      -- | Represents the initial value at which the transaction ticket number
      -- counter will start (i.e. the zeroth ticket number).
    , zeroIdx :: idx
    }

-- | A pure snapshot of the contents of the mempool. It allows fetching
-- information about transactions in the mempool, and fetching individual
-- transactions.
--
-- This uses a transaction sequence number type for identifying transactions
-- within the mempool sequence. The sequence number is local to this mempool,
-- unlike the transaction hash. This allows us to ask for all transactions
-- after a known sequence number, to get new transactions. It is also used to
-- look up individual transactions.
--
-- Note that it is expected that 'getTx' will often return 'Nothing'
-- even for tx sequence numbers returned in previous snapshots. This happens
-- when the transaction has been removed from the mempool between snapshots.
--
data MempoolSnapshot txid tx idx = MempoolSnapshot {
    -- | Get all transactions in the mempool snapshot along with their
    -- associated ticket numbers (oldest to newest).
    getTxs :: [(txid, tx, idx)]

    -- | Get all transactions in the mempool snapshot, along with their
    -- associated ticket numbers, which are associated with a ticket number
    -- greater than the one provided.
  , getTxsAfter :: idx -> [(txid, tx, idx)]

    -- | Get a specific transaction from the mempool snapshot by its ticket
    -- number, if it exists.
  , getTx :: idx -> Maybe (txid, tx)
  }
