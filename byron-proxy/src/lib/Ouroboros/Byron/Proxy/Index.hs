{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Ouroboros.Byron.Proxy.Index where

import Control.Exception (Exception)
import Control.Monad.Trans.Class (lift)
import Crypto.Hash (Digest, HashAlgorithm, digestFromByteString)
import Data.ByteString (ByteString)
import Data.ByteArray (convert)
import Data.Conduit (ConduitT, yield)
import Database.SQLite.Simple (Connection, Only (..), Query)
import qualified Database.SQLite.Simple as Sql
import System.Directory (doesFileExist)

import Pos.Chain.Block (HeaderHash)
import Pos.Crypto.Hashing (AbstractHash (..))
import Pos.DB.Class (Serialized (..), SerializedBlock)

import Ouroboros.Storage.ImmutableDB.API (ImmutableDB)
import Ouroboros.Storage.ImmutableDB.CumulEpochSizes (EpochSlot(..),
                                                      RelativeSlot(..))
import qualified Ouroboros.Storage.ImmutableDB.API as ImmutableDB
import Ouroboros.Storage.ImmutableDB.Types
import Ouroboros.Byron.Proxy.Types

-- cardano-sl crypto apparently gives no way to get an AbstractHash from
-- a ByteString without re-hashing it. We'll use `digestFromByteString` from
-- cryptonite and trust that it's the right size.
digestFromByteString_partial :: HashAlgorithm a => ByteString -> Digest a
digestFromByteString_partial bs =
  let Just digest = digestFromByteString bs in digest

-- | A point to read from the index. Looking up the tip gives its own hash,
-- looking up anything by hash (could happen to be the tip) gives the hash
-- of its child (nothing iff it's the tip).
data IndexPoint next where
  Tip    :: IndexPoint OwnHash
  ByHash :: HeaderHash -> IndexPoint ChildHash

newtype OwnHash = OwnHash
  { getOwnHash :: HeaderHash
  }

newtype ChildHash = ChildHash
  { getChildHash :: Maybe HeaderHash
  }

data Index m = Index
  { indexRead  :: forall d . IndexPoint d -> m (Maybe (d, EpochSlot))
  , indexWrite :: forall t . (IndexWrite m -> m t) -> m t
  }

data IndexWrite m = IndexWrite
  { updateTip :: HeaderHash -> EpochSlot -> m ()
  }

-- TODO Move SQlite implemention into a separate module.

-- | Make an index from an SQLite connection (sqlite-simple).
-- Every `indexWrite` continuation runs in an SQLite transaction
-- (BEGIN TRANSACTION).
sqliteIndex :: Connection -> Index IO
sqliteIndex conn = Index
  { indexRead = \ip -> case ip of
      Tip -> (fmap . fmap) (\(a, b) -> (OwnHash a, b)) (getTip conn)
      ByHash hh -> (fmap . fmap) (\(a, b) -> (ChildHash a, b)) (getHash conn hh)
  , indexWrite = \k -> Sql.withTransaction conn (k (IndexWrite (setTip conn)))
  }

-- | Open a new or existing SQLite database. If new, it will set up the schema.
data OpenDB where
  New      :: OpenDB
  Existing :: OpenDB

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

-- | An index on an ImmutableDB is enough to get a ByronBlockSource, but we
-- assume that the index and DB are kept in-sync.
-- Also assumes of course that the DB blobs are valid encoded blocks.
-- Must give a `SerializedBlock` for the genesis, which is used when the
-- index (and therefore immutable DB) are empty.
byronBlockSource
  :: forall m .
     ( Monad m )
    -- TODO proper error type.
  => (forall x . IndexInconsistent -> m x)
  -> SerializedBlock
  -> Index m
  -> ImmutableDB m
  -> ByronBlockSource m
byronBlockSource err genesis idx db = ByronBlockSource
  { bbsStream = streamFrom
  , bbsTip    = indexRead idx Tip >>= \mTip -> case mTip of
      Nothing -> pure genesis
      Just (_, es) -> ImmutableDB.getBinaryBlob db (error "TODO") >>= \mBs -> case mBs of
        Nothing -> err TipNotInDatabase
        Just bs -> pure $ Serialized bs
  }
  where
  streamFrom :: HeaderHash -> ConduitT () SerializedBlock m ()
  streamFrom hh = lift (indexRead idx (ByHash hh)) >>= \mPoint -> case mPoint of
    -- No problem. The stream is over.
    Nothing -> pure ()
    -- Got an entry. Use the EpochSlot to look up in the database, and the
    -- child hash to continue the stream (if Just).
    Just (ChildHash mChild, es) -> lift (ImmutableDB.getBinaryBlob db (error "TODO")) >>= \mBs -> case mBs of
      Nothing -> lift $ err $ PointNotInDatabase hh es
      Just bs -> do
        yield (Serialized bs)
        case mChild of
          Nothing -> pure ()
          Just hh' -> streamFrom hh'

data IndexInconsistent where
  TipNotInDatabase   :: IndexInconsistent
  PointNotInDatabase :: HeaderHash -> EpochSlot -> IndexInconsistent

deriving instance Show IndexInconsistent
instance Exception IndexInconsistent

-- | The index is a relation:
--
-- +------------+------------+-------+------+
-- | HeaderHash | HeaderHash | Epoch | Slot |
-- +------------+------------+-------+------+
-- | ByteString | ByteString | Word  | Word |
-- +------------+------------+-------+------+
--
-- HeaderHash primary key, forward link foreign key into the same table but
-- is nullable (for the tip), epoch and relative slot unique in the table
-- (as a pair) and there's an index on this pair.
--
-- We use INTEGER for epoch and slot, but the DB stuff uses Word.
-- INTEGER can be as big as 8 bytes, which may not fit into a Word depending
-- on implementation. But since we're the only ones writing to this database,
-- and we only put in Words, it should be ok.
--
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
           es = EpochSlot (Epoch epoch) (RelativeSlot slot)
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
          es = EpochSlot (Epoch epoch) (RelativeSlot slot)
      in  pure $ Just (hh, es)

sql_insert :: Query
sql_insert = "INSERT INTO block_index VALUES (?, NULL, ?, ?);"

sql_update :: Query
sql_update = "UPDATE block_index SET child_header_hash = ? WHERE header_hash = ?;"

sql_get_tip_hash :: Query
sql_get_tip_hash =
  "SELECT header_hash FROM block_index\
  \ ORDER BY epoch DESC, slot DESC LIMIT 1;"

setTipTx :: Sql.Connection -> HeaderHash -> EpochSlot -> IO ()
setTipTx conn hh es = Sql.withTransaction conn $ setTip conn hh es

setTip :: Sql.Connection -> HeaderHash -> EpochSlot -> IO ()
setTip conn (AbstractHash digest) (EpochSlot (Epoch epoch) rslot) = do
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
