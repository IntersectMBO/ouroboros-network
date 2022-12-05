{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}

{- | Persistent transactions.

  There are two downsides to regular @'Transaction'@s:
  * The current @'Transaction'@ runners (like @'readWriteTransaction'@) will
    create and destroy an internal transaction handle (@'MDB_txn'@), which means
    that we can not use the fact that the transaction handle provides a
    consistent view of database if we want to perform several transactions to
    the same consistent view.
  * A @'Transaction'@ will only produce results when it is run with
    @'readWriteTransaction'@ or one of the other runners. This prevents us from
    getting intermediate results or performing intermediate writes (side
    effects).

  A persistent transaction @'PTransaction'@ will keep open a transaction handle
  in an @'MVar'@, such that we can submit the usuals @'Transaction'@s to it.
  Each submission will provide its results and peform its side effects right
  away.

  TODO: upstream this module to @lmdb-simple@.
-}
module Ouroboros.Consensus.Storage.LedgerDB.HD.LMDB.PersistentTransaction (
    PTransaction
  , close
  , new
  , submit
  ) where

import           Control.Concurrent.MVar
import           Control.Monad

import           Database.LMDB.Raw
import           Database.LMDB.Simple.Internal

-- | A persistent transaction @'PTransaction'@ will keep open a transaction
-- handle.
newtype PTransaction mode = PTransaction {
    -- Note: if @'itTxn'@ is an empty @'MVar'@, the transaction is considered
    -- closed. As such, we should not use any blocking @'MVar'@ operations.
    itTxn  :: MVar MDB_txn
  }

-- | Create a new persistent transaction. Should preferably be closed (i.e.,
-- committed) using 'close' to ensure that the internal transaction is freed,
-- though it will also automatically free the transaction handle once the
-- persistent transaction is garbage collected.
new :: Mode mode => Environment mode -> IO (PTransaction mode)
new env@(Env mdbEnv) = do
  txn <- mdb_txn_begin mdbEnv Nothing (isReadOnlyEnvironment env)
  itTxn <- newMVar txn
  void $ mkWeakMVar itTxn $ finalize itTxn
  pure PTransaction {itTxn}
  where
    finalize :: MVar MDB_txn -> IO ()
    finalize itTxn = do
      txnMay <- tryTakeMVar itTxn
      case txnMay of
        Nothing  -> pure ()
        Just txn -> mdb_txn_commit txn

-- | Submit a regular @'Transaction'@ to a persistent transaction. Throws an
-- error if the persistent transaction has already been closed.
submit :: PTransaction mode -> Transaction ReadOnly a -> IO a
submit PTransaction{itTxn} tx@(Txn tf) = do
  txnMay <- tryReadMVar itTxn
  case txnMay of
    Nothing  -> error "submit: PTransaction is closed"
    Just txn ->
      if isReadOnlyTransaction tx then
        tf txn
      else
        error "submit: ReadWrite transactions not yet supported"

-- | Close the persistent transaction by committing the internal transaction.
-- Throws an error if the persistent transaction as already been closed.
close :: PTransaction mode -> IO ()
close PTransaction{itTxn} = do
  txnMay <- tryTakeMVar itTxn
  case txnMay of
    Nothing  -> error "close: PTransaction is closed"
    Just txn -> mdb_txn_commit txn
