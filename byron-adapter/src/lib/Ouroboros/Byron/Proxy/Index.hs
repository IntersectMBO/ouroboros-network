{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

module Ouroboros.Byron.Proxy.Index where

-- cardano-sl crypto apparently gives no way to get an AbstractHash from
-- a ByteString without re-hashing it...
import Crypto.Hash (Digest, HashAlgorithm, digestFromByteString)
import Data.ByteString (ByteString)
import Data.ByteArray (convert)
import Database.SQLite.Simple as Sql
import System.Directory (doesFileExist)

import Pos.Chain.Block (HeaderHash)
import Pos.Crypto.Hashing (AbstractHash (..))

import Ouroboros.Storage.ImmutableDB.Types

digestFromByteString_partial :: HashAlgorithm a => ByteString -> Digest a
digestFromByteString_partial bs =
  let Just digest = digestFromByteString bs in digest

newtype OwnHash = OwnHash
  { getOwnHash :: HeaderHash
  }

newtype ChildHash = ChildHash
  { getChildHash :: Maybe HeaderHash
  }

data IndexPoint next where
  Tip    :: IndexPoint OwnHash
  ByHash :: HeaderHash -> IndexPoint ChildHash

-- | Given an `IndexPoint`, look it up in the index and give the forward
-- link (determined by the `IndexPoint`s parameter) and the `EpochSlot` for
-- the entry at the index point.
--
-- For the tip you need to get a header hash (of the tip)
-- For others you need to get a header hash, but of its child.
data Index m = Index
  { indexAt :: forall datum . IndexPoint datum -> m (Maybe (datum, EpochSlot))
  , updateTip :: HeaderHash -> EpochSlot -> m ()
  }

-- Practical details:
-- - We'll use rocksdb, since there's precedent, and a key/value is more
--   appropriate than relational (how would we keep the tip in a relational?)
-- - Oh but never mind, in a relational we can easily get the tip by ordering
--   the (headerhash, epochslot) relation.
--
-- +------------+------------+-------+------+
-- | HeaderHash | HeaderHash | Epoch | Slot |
-- +------------+------------+-------+------+
-- | ByteString | ByteString | Word  | Word |
-- +------------+------------+-------+------+
--
-- HeaderHash primary key, forward link foreign key into the same table if
-- possible, epoch and relative slot unique in the table (as a pair).
-- Want an index on the HeaderHash primary key, which I believe is automatic
-- in sqlite.
-- Forward link is nullable... can a foreign key be null? Yes, it can.

sqliteIndex :: Connection -> Index IO
sqliteIndex conn = Index
  { indexAt = \ip -> case ip of
      Tip -> (fmap . fmap) (\(a, b) -> (OwnHash a, b)) (getTip conn)
      ByHash hh -> (fmap . fmap) (\(a, b) -> (ChildHash a, b)) (getHash conn hh)
  , updateTip = setTip conn
  }

data OpenDB where
  New      :: OpenDB
  Existing :: OpenDB

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

-- We use INTEGER for epoch and slot, but the DB stuff uses Word.
-- INTEGER can be as big as 8 bytes, which may not fit into a Word depending
-- on implementation. But since we're the only ones writing to this database,
-- and we only put in Words, it should be ok.
sql_create_table :: Query
sql_create_table =
  "CREATE TABLE block_index\n\
  \  ( header_hash       BLOB NOT NULL PRIMARY KEY\n\
  \  , child_header_hash BLOB\n\
  \  , epoch             INTEGER NOT NULL\n\
  \  , slot              INTEGER NOT NULL\n\
  \  , FOREIGN KEY(child_header_hash) REFERENCES block_index(header_hash)\n\
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

getTip :: Sql.Connection -> IO (Maybe (HeaderHash, EpochSlot))
getTip conn = do
   rows :: [(ByteString, Word, Word)] <- Sql.query_ conn sql_get_tip
   case rows of
     [] -> pure Nothing
     ((hhBlob, epoch, slot) : _) ->
       let hh = AbstractHash $ digestFromByteString_partial hhBlob
           es = EpochSlot epoch (RelativeSlot slot)
       in  pure $ Just (hh, es)

sql_get_hash :: Query
sql_get_hash =
  "SELECT child_header_hash, epoch, slot FROM block_index\
  \ WHERE header_hash = ?;"

getHash :: Sql.Connection -> HeaderHash -> IO (Maybe (Maybe HeaderHash, EpochSlot))
getHash conn (AbstractHash digest) = do
  rows :: [(Maybe ByteString, Word, Word)]
    <- Sql.query conn sql_get_hash (Sql.Only (convert digest :: ByteString))
  case rows of
    [] -> pure Nothing
    ((hhChildBlob, epoch, slot) : _) ->
      let hh = fmap (AbstractHash . digestFromByteString_partial) hhChildBlob
          es = EpochSlot epoch (RelativeSlot slot)
      in  pure $ Just (hh, es)

sql_insert :: Query
sql_insert = "INSERT INTO block_index VALUES (?, NULL, ?, ?);"

sql_update :: Query
sql_update = "UPDATE block_index SET child_header_hash = ? WHERE header_hash = ?;"

sql_get_tip_hash :: Query
sql_get_tip_hash =
  "SELECT header_hash FROM block_index\
  \ ORDER BY epoch DESC, slot DESC LIMIT 1;"

setTip :: Sql.Connection -> HeaderHash -> EpochSlot -> IO ()
setTip conn (AbstractHash digest) (EpochSlot epoch rslot) =
  Sql.withTransaction conn $ do
    -- Get the current tip.
    mTip :: [Only ByteString] <- Sql.query_ conn sql_get_tip_hash
    case mTip of
      -- No tip in the database (it's empty). Just one statement to do.
      [] -> do
        Sql.execute conn sql_insert (hashBytes, epoch, slot)
      (Only oldTipHashBytes : _) -> do
        -- Insert the new tip.
        Sql.execute conn sql_insert (hashBytes, epoch, slot)
        -- Update the old tip child pointer.
        Sql.execute conn sql_update (hashBytes, oldTipHashBytes)
  where
  hashBytes :: ByteString
  hashBytes = convert digest
  RelativeSlot slot = rslot
