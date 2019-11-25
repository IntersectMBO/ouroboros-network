{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE StandaloneDeriving      #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Ouroboros.Consensus.Mempool.API (
    Mempool(..)
  , MempoolSnapshot(..)
  , ApplyTx(..)
  , TraceEventMempool(..)
    -- * Re-exports
  , TxSizeInBytes
  ) where

import           Control.Monad.Except
import           GHC.Stack (HasCallStack)

import           Ouroboros.Network.Protocol.TxSubmission.Type (TxSizeInBytes)

import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Util.IOLike

class ( UpdateLedger blk
      , Ord (GenTxId blk)
      , NoUnexpectedThunks (GenTx blk)
      ) => ApplyTx blk where
  -- | Generalized transaction
  --
  -- The mempool (and, accordingly, blocks) consist of "generalized
  -- transactions"; this could be "proper" transactions (transferring funds) but
  -- also other kinds of things such as update proposals, delegations, etc.
  data family GenTx blk :: *

  -- | A generalized transaction, 'GenTx', identifier.
  data family GenTxId blk :: *

  -- | Return the 'GenTxId' of a 'GenTx'.
  --
  -- Should be cheap as this will be called often.
  txId :: GenTx blk -> GenTxId blk

  -- | Return the post-serialization size in bytes of a 'GenTx'.
  txSize :: GenTx blk -> TxSizeInBytes

  -- | Check whether the internal invariants of the transaction hold.
  txInvariant :: GenTx blk -> Bool
  txInvariant = const True

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
      -- This function will return a list containing the following
      -- transactions:
      --
      -- * Those transactions provided which were found to be valid, along
      --   with 'Nothing' for their accompanying @Maybe (ApplyTxErr blk)@
      --   values.
      -- * Those transactions provided which were found to be invalid, along
      --   with their accompanying validation errors.
      --
      -- The order of this returned list is undefined.
      --
      -- Note that previously valid transaction that are now invalid with
      -- respect to the current ledger state are dropped from the mempool, but
      -- are not part of the returned list.
      --
      -- POSTCONDITION: given some ordering @txOrd@ on @'GenTx' blk@:
      --
      -- > addTxs inTxs >>= \outTxs ->
      -- >   sortBy txOrd inTxs == sortBy txOrd (map fst outTxs)
      --
      -- In principle it is possible that validation errors are transient; for
      -- example, it is possible that a transaction is rejected because one of
      -- its inputs is not /yet/ available in the UTxO (the transaction it
      -- depends on is not yet in the chain, nor in the mempool). In practice
      -- however it is likely that rejected transactions will still be
      -- rejected later, and should just be dropped.
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
      --
      -- It is also worth noting that, if the mempool capacity is reached,
      -- this function will block until it's able to at least attempt
      -- validating and adding each of the provided transactions to the
      -- mempool. In the event that we block, we also commit any useful work
      -- done up to that point. For example, if we tried to add 5 valid
      -- transactions but there is only space for 3, we would validate and add
      -- 3 to the mempool and then block until more space becomes available,
      -- at which point we would then re-attempt with the remaining 2
      -- transactions. This process would continue until it is able to at
      -- least attempt validating and adding each of the provided transactions
      -- to the mempool.
      addTxs        :: [GenTx blk] -> m [(GenTx blk, Maybe (ApplyTxErr blk))]

      -- | Manually remove the given transactions from the mempool.
    , removeTxs     :: [GenTxId blk] -> m ()

      -- | Sync the transactions in the mempool with the current ledger state
      --  of the 'ChainDB'.
      --
      -- The transactions that exist within the mempool will be revalidated
      -- against the current ledger state. Transactions which are found to be
      -- invalid with respect to the current ledger state, will be dropped
      -- from the mempool, whereas valid transactions will remain.
      --
      -- The given function will be applied to a snapshot of the mempool that
      -- is in sync with the current ledger state. This function will be
      -- executed in the same 'STM' transaction that performs the
      -- synchronisation. The main use case for this is a function that
      -- produces a block containing the valid transactions in the mempool
      -- snapshot.
      --
      -- Using this approach, we avoid the following race condition: the
      -- current ledger state changes while we're producing a block, which
      -- means that some of the transactions in the new block might no longer
      -- be valid with respect to the current ledger state. Consequently, we
      -- produce an invalid block, wasting our leadership slot.
      --
      -- We keep this in @m@ instead of @STM m@ to leave open the possibility
      -- of persistence. Additionally, this makes it possible to trace the
      -- removal of invalid transactions.
      --
      -- n.b. in our current implementation, when one opens a mempool, we
      -- spawn a thread which performs this action whenever the 'ChainDB' tip
      -- point changes.
    , withSyncState :: forall a. (MempoolSnapshot blk idx -> STM m a) -> m a

      -- | Get a snapshot of the current mempool state. This allows for
      -- further pure queries on the snapshot.
    , getSnapshot   :: STM m (MempoolSnapshot blk idx)

      -- | Represents the initial value at which the transaction ticket number
      -- counter will start (i.e. the zeroth ticket number).
    , zeroIdx       :: idx
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
data MempoolSnapshot blk idx = MempoolSnapshot {
    -- | Get all transactions (oldest to newest) in the mempool snapshot along
    -- with their ticket number.
    snapshotTxs      :: [(GenTx blk, idx)]

    -- | Get all transactions (oldest to newest) in the mempool snapshot,
    -- along with their ticket number, which are associated with a ticket
    -- number greater than the one provided.
  , snapshotTxsAfter :: idx -> [(GenTx blk, idx)]

    -- | Get a specific transaction from the mempool snapshot by its ticket
    -- number, if it exists.
  , snapshotLookupTx :: idx -> Maybe (GenTx blk)
  }

-- | Events traced by the Mempool.
data TraceEventMempool blk
  = TraceMempoolAddTxs
      ![GenTx blk]
      -- ^ New, valid transaction were added to the Mempool.
      !Word
      -- ^ The total number of transactions now in the Mempool.
  | TraceMempoolRejectedTxs
      ![(GenTx blk, ApplyTxErr blk)]
      -- ^ New, invalid transaction were rejected and thus not added to the
      -- Mempool.
      !Word
      -- ^ The total number of transactions now in the Mempool.
  | TraceMempoolRemoveTxs
      ![GenTx blk]
      -- ^ Previously valid transactions that are no longer valid because of
      -- changes in the ledger state. These transactions have been removed
      -- from the Mempool.
      !Word
      -- ^ The total number of transactions now in the Mempool.
  | TraceMempoolManuallyRemovedTxs
      ![GenTxId blk]
      -- ^ Transactions that have been manually removed from the Mempool.
      ![GenTx blk]
      -- ^ Previously valid transactions that are no longer valid because they
      -- dependend on transactions that were manually removed from the
      -- Mempool. These transactions have also been removed from the Mempool.
      --
      -- This list shares not transactions with the list of manually removed
      -- transactions.
      !Word
      -- ^ The total number of transactions now in the Mempool.

deriving instance ( Eq (GenTx blk)
                  , Eq (GenTxId blk)
                  , Eq (ApplyTxErr blk)
                  ) => Eq (TraceEventMempool blk)

deriving instance ( Show (GenTx blk)
                  , Show (GenTxId blk)
                  , Show (ApplyTxErr blk)
                  ) => Show (TraceEventMempool blk)
