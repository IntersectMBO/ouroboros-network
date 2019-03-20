{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

-- TODO move to Ouroboros.Byron.DB.Index
module Ouroboros.Byron.Proxy.Index where

import Control.Exception (Exception, throwIO)
import Crypto.Hash (digestFromByteString)
import Data.ByteString (ByteString)
import Data.ByteArray (convert)
import Data.Word (Word64)
import Database.SQLite.Simple (Connection, Query)
import qualified Database.SQLite.Simple as Sql
import System.Directory (doesFileExist)

import Pos.Chain.Block (HeaderHash)
import Pos.Crypto.Hashing (AbstractHash (..))

import Ouroboros.Storage.ImmutableDB.Types

-- | A point to read from the index. Looking up the tip gives its own hash,
-- looking up anything by hash (could happen to be the tip) gives the hash
-- of its child (nothing iff it's the tip).
data IndexPoint next where
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

-- TODO Move SQlite implemention into a separate module.

-- | Make an index from an SQLite connection (sqlite-simple).
-- Every `indexWrite` continuation runs in an SQLite transaction
-- (BEGIN TRANSACTION).
sqliteIndex :: Connection -> Index IO
sqliteIndex conn = Index
  { indexRead = \ip -> case ip of
      Tip -> getTip conn
      ByHash hh -> getHash conn hh
  , indexWrite = \k -> Sql.withTransaction conn (k (IndexWrite (setTip conn)))
  }

-- | Open a new or existing SQLite database. If new, it will set up the schema.
data OpenDB where
  New      :: OpenDB
  Existing :: OpenDB

-- TODO
-- should implement a feature to reconstruct/repair the index using an
-- `ImmutableDB`: iterate from the start and insert an entry for each thing.
-- Would write that in Ouroboros.Byron.Proxy.DB though, I think.

-- | Use an SQLite database connection.
withDB :: OpenDB -> FilePath -> (Index IO -> IO t) -> IO t
withDB o fp k = Sql.withConnection fp $ \conn -> do
  case o of
    New      -> Sql.withTransaction conn $ do
      createTable conn
      createIndex conn
    Existing -> pure ()
  k (sqliteIndex conn)

-- | Like withDB but uses file existence check to determine whether it's new
-- or existing.
withDB_ :: FilePath -> (Index IO -> IO t) -> IO t
withDB_ fp k = doesFileExist fp >>= \b -> case b of
  True  -> withDB Existing fp k
  False -> withDB New      fp k

data IndexInternallyInconsistent where
  -- | The `Int` is less than -1
  InvalidRelativeSlot :: HeaderHash -> Int -> IndexInternallyInconsistent
  -- | The header hash for the tip is not the right size.
  InvalidHash :: ByteString -> IndexInternallyInconsistent

deriving instance Show IndexInternallyInconsistent
instance Exception IndexInternallyInconsistent

-- | The index is the relation:
--
-- +------------+---------+------+
-- | HeaderHash | EpochNo | Slot |
-- +------------+---------+------+
-- | ByteString | Word    | Int  |
-- +------------+----------------+
--
-- HeaderHash primary key, epoch and relative slot unique in the table
-- (as a pair) and there's an index on this pair.
--
-- EpochNo boundary blocks get slot number -1, thereby packing them in-between
-- the last block of the prior epoch, and the first block of the next epoch.
--
-- Forward links aren't necessary, since we can tell what the next block is
-- by using the order on epoch, slot.
--
-- We use the sqlite INTEGER for epoch and slot, but the DB stuff uses Word.
-- INTEGER can be as big as 8 bytes, which may not fit into a Word depending
-- on implementation. But since we're the only ones writing to this database,
-- and we only put in Words, it should be ok.
--
sql_create_table :: Query
sql_create_table =
  "CREATE TABLE block_index\n\
  \  ( header_hash       BLOB NOT NULL PRIMARY KEY\n\
  \  , epoch             INTEGER NOT NULL\n\
  \  , slot              INTEGER NOT NULL\n\
  \  , UNIQUE (epoch, slot)\n\
  \  );"

sql_create_index :: Query
sql_create_index =
  "CREATE INDEX epoch_slot ON block_index (epoch, slot);"

createTable :: Sql.Connection -> IO ()
createTable conn = Sql.execute_ conn sql_create_table

createIndex :: Sql.Connection -> IO ()
createIndex conn = Sql.execute_ conn sql_create_index

-- | The tip is the entry with the highest epoch and slot pair.
sql_get_tip :: Query
sql_get_tip =
  "SELECT header_hash, epoch, slot FROM block_index\
  \ ORDER BY epoch DESC, slot DESC LIMIT 1;"

getTip :: Sql.Connection -> IO (Maybe (HeaderHash, EpochNo, IndexSlot))
getTip conn = do
   rows :: [(ByteString, Word64, Int)] <- Sql.query_ conn sql_get_tip
   case rows of
     [] -> pure Nothing
     ((hhBlob, epoch, slotInt) : _) -> do
       hh <- case digestFromByteString hhBlob of
         Just hh -> pure (AbstractHash hh)
         Nothing -> throwIO $ InvalidHash hhBlob
       slot <-
         if slotInt == -1
         then pure EBBSlot
         else if slotInt >= 0
         then pure (RealSlot (fromIntegral slotInt))
         else throwIO $ InvalidRelativeSlot hh slotInt
       pure $ Just (hh, EpochNo epoch, slot)

sql_get_hash :: Query
sql_get_hash =
  "SELECT epoch, slot FROM block_index\
  \ WHERE header_hash = ?;"

getHash :: Sql.Connection -> HeaderHash -> IO (Maybe ((), EpochNo, IndexSlot))
getHash conn hh@(AbstractHash digest) = do
  rows :: [(Word64, Int)]
    <- Sql.query conn sql_get_hash (Sql.Only (convert digest :: ByteString))
  case rows of
    [] -> pure Nothing
    ((epoch, slotInt) : _) -> do
      slot <-
        if slotInt == -1
        then pure EBBSlot
        else if slotInt >= 0
        then pure (RealSlot (fromIntegral slotInt))
        else throwIO $ InvalidRelativeSlot hh slotInt
      pure $ Just ((), EpochNo epoch, slot)

sql_insert :: Query
sql_insert = "INSERT INTO block_index VALUES (?, ?, ?);"

setTip :: Sql.Connection -> HeaderHash -> EpochNo -> IndexSlot -> IO ()
setTip conn (AbstractHash digest) (EpochNo epoch) slot = do
  Sql.execute conn sql_insert (hashBytes, epoch, slotInt)
  where
  slotInt :: Int
  slotInt = case slot of
    EBBSlot       -> -1
    RealSlot word -> fromIntegral word
  hashBytes :: ByteString
  hashBytes = convert digest
