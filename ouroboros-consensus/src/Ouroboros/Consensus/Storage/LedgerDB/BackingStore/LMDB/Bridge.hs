{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE Rank2Types #-}

{-| Alternatives to LMDB operations that do not rely on @'Serialise'@ instances

  We cannot (easily and without runtime overhead) satisfy the @'Serialise'@
  constraints that the @lmdb-simple@ operations require. We have access to the
  codification and decodification functions provided in @'CodecMK'@, thus, we
  redefine parts of the internal @LMDB.Simple@ operations here. The
  redefinitions are largely analogous to their counterparts, though they thread
  through explicit CBOR encoders and decoders.
-}
module Ouroboros.Consensus.Storage.LedgerDB.BackingStore.LMDB.Bridge (
    -- * Internal: peek and poke
    peekMDBVal
  , pokeMDBVal
    -- * Internal: marshalling
  , deserialiseLBS
  , marshalIn
  , marshalInBS
  , marshalOut
  , serialiseBS
  , serialiseLBS
    -- * Cursor
  , fromCodecMK
  , runCursorAsTransaction'
    -- * Internal: get and put
  , delete
  , deleteBS
  , get
  , getBS
  , getBS'
  , put
  , putBS
  ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import           Codec.CBOR.Read (deserialiseFromBytes)
import           Codec.CBOR.Write (toLazyByteString)
import           Control.Exception (assert)
import           Control.Monad ((>=>))
import           Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           Database.LMDB.Raw (MDB_val (MDB_val), mdb_reserve')
import           Database.LMDB.Simple (Database, Mode (ReadWrite), Transaction)
import           Database.LMDB.Simple.Cursor (CursorM)
import qualified Database.LMDB.Simple.Cursor as Cursor
import qualified Database.LMDB.Simple.Internal as Internal
import           Foreign (Ptr, Storable (peek, poke), castPtr)
import           Ouroboros.Consensus.Ledger.Tables

{-------------------------------------------------------------------------------
  Internal: peek and poke
-------------------------------------------------------------------------------}

peekMDBVal :: (forall s. Decoder s a) -> Ptr MDB_val -> IO a
peekMDBVal dec = peek >=> marshalIn dec

pokeMDBVal :: (a -> Encoding) -> Ptr MDB_val -> a -> IO ()
pokeMDBVal enc ptr x = marshalOut enc x (poke ptr)

{-------------------------------------------------------------------------------
  Internal: marshalling
-------------------------------------------------------------------------------}

marshalIn ::
     (forall s. Decoder s a)
  -> MDB_val
  -> IO a
marshalIn dec v = deserialiseLBS "" dec . LBS.fromStrict <$>  marshalInBS v

marshalInBS :: MDB_val -> IO BS.ByteString
marshalInBS (MDB_val len ptr) = BS.packCStringLen (castPtr ptr, fromIntegral len)

-- | Deserialise an @'LBS.ByteString'@ using the provided decoder.
deserialiseLBS ::
     String
  -- ^ Label to be used for error reporting. This should describe the value to
  -- be deserialised.
  -> (forall s . Decoder s a)
  -> LBS.ByteString
  -> a
deserialiseLBS label decoder bs = either err snd $ deserialiseFromBytes decoder bs
  where
    err = error $ "deserialiseBS: error deserialising " ++ label ++ " from the database."

marshalOut ::
     (v -> Encoding)
  -> v
  -> (MDB_val -> IO t)
  -> IO t
marshalOut enc = marshalOutBS . serialiseBS enc

marshalOutBS :: BS.ByteString -> (MDB_val -> IO a) -> IO a
marshalOutBS = Internal.marshalOutBS

serialiseBS :: (a -> Encoding) -> a -> BS.ByteString
serialiseBS enc = LBS.toStrict . serialiseLBS enc

serialiseLBS :: (a -> Encoding) -> a -> LBS.ByteString
serialiseLBS enc = toLazyByteString . enc

{-------------------------------------------------------------------------------
  Cursor
-------------------------------------------------------------------------------}

fromCodecMK :: CodecMK k v -> Cursor.PeekPoke k v
fromCodecMK (CodecMK encKey encVal decKey decVal) = Cursor.PeekPoke {
    Cursor.kPeek = peekMDBVal decKey
  , Cursor.vPeek = peekMDBVal decVal
  , Cursor.kPoke = pokeMDBVal encKey
  , Cursor.vPoke = pokeMDBVal encVal
  }

-- | Wrapper around @'Cursor.runCursorAsTransaction''@ that requires a
-- @'CodecMK'@ instead of a @'PeekPoke'@.
runCursorAsTransaction' ::
     CursorM k v mode a
  -> Database k v
  -> CodecMK k v
  -> Transaction mode a
runCursorAsTransaction' cm db codecMK =
  Cursor.runCursorAsTransaction' cm db (fromCodecMK codecMK)

{-------------------------------------------------------------------------------
  Internal: get, put and delete
-------------------------------------------------------------------------------}

get ::
     CodecMK k v
  -> Database k v
  -> k
  -> Transaction mode (Maybe v)
get (CodecMK encKey _ _ decVal) db = getBS decVal db . serialiseBS encKey

getBS ::
     (forall s. Decoder s v)
  -> Database k v
  -> BS.ByteString
  -> Transaction mode (Maybe v)
getBS dec db k = getBS' db k >>=
    maybe (return Nothing) (liftIO . fmap Just . marshalIn dec)

getBS' :: Database k v -> BS.ByteString -> Transaction mode (Maybe MDB_val)
getBS' = Internal.getBS'

put ::
     CodecMK k v
  -> Database k v
  -> k
  -> v
  -> Transaction ReadWrite ()
put codecMK@(CodecMK encKey _ _ _) db = putBS codecMK db . serialiseBS encKey

putBS ::
     CodecMK k v
  -> Database k v
  -> BS.ByteString
  -> v
  -> Transaction ReadWrite ()
putBS (CodecMK _ encVal _ _) (Internal.Db _ dbi) keyBS value = Internal.Txn $ \txn ->
  Internal.marshalOutBS keyBS $ \kval -> do
    let valueLBS = serialiseLBS encVal value
        sz = fromIntegral (LBS.length valueLBS)
    MDB_val len ptr <- mdb_reserve' Internal.defaultWriteFlags txn dbi kval sz
    let len' = fromIntegral len
    assert (len' == sz) $ Internal.copyLazyBS valueLBS ptr len'

delete ::
     CodecMK k v
  -> Database k v
  -> k
  -> Transaction ReadWrite Bool
delete (CodecMK encKey _ _ _) db = deleteBS db . serialiseBS encKey

deleteBS :: Database k v -> BS.ByteString -> Transaction ReadWrite Bool
deleteBS = Internal.deleteBS
