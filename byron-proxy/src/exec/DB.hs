module DB
  ( DBConfig (..)
  , withDB
  , seedWithGenesis
  ) where

import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Decoding as CBOR

import Control.Exception (throwIO)
import qualified System.Directory (createDirectoryIfMissing)

import qualified Pos.Binary.Class as CSL (decode, encode)
import qualified Pos.Chain.Block as CSL (Block, GenesisBlock, HeaderHash)
import qualified Pos.Core as CSL (SlotCount (..))

import Ouroboros.Byron.Proxy.DB (DB)
import qualified Ouroboros.Byron.Proxy.DB as DB
import qualified Ouroboros.Byron.Proxy.Index.Sqlite as Index
import qualified Ouroboros.Storage.Common as Immutable
import qualified Ouroboros.Storage.ImmutableDB.API as Immutable
import qualified Ouroboros.Storage.ImmutableDB.Impl as Immutable
import Ouroboros.Storage.FS.API.Types (MountPoint (..))
import Ouroboros.Storage.FS.API (HasFS)
import Ouroboros.Storage.FS.IO (HandleIO, ioHasFS)
import qualified Ouroboros.Storage.Util.ErrorHandling as FS (exceptions)

-- This module makes it convenient to set up a DB (ImmutableDB, HasFS, and
-- SQLite index) in IO, handling cardano-sl block types.

data DBConfig = DBConfig
  { dbFilePath    :: !FilePath
    -- ^ Directory to house the `ImmutableDB`.
  , indexFilePath :: !FilePath
    -- ^ Path to the SQLite index.
  , slotsPerEpoch :: !CSL.SlotCount
    -- ^ Number of slots per epoch. Cannot handle a chain for which this is
    -- not constant.
  }

-- | Set up and use a DB. You may also want to `seedWithGenesis`, which will
-- only do so if the DB is empty, because the Byron logic layer expects the
-- database to never be empty.
--
-- The directory at `dbFilePath` will be created if it does not exist.
withDB :: DBConfig -> (DB IO -> IO t) -> IO t
withDB dbOptions k = do
  -- The ImmutableDB/Storage layer will not create a directory for us, we have
  -- to ensure it exists.
  System.Directory.createDirectoryIfMissing True (dbFilePath dbOptions)
  let fsMountPoint :: MountPoint
      fsMountPoint = MountPoint (dbFilePath dbOptions)
      fs :: HasFS IO HandleIO
      fs = ioHasFS fsMountPoint
      getEpochSize epoch = pure $ Immutable.EpochSize $
        fromIntegral (CSL.getSlotCount (slotsPerEpoch dbOptions))
      openImmutableDB = Immutable.openDB
        decodeHeaderHash
        encodeHeaderHash
        fs
        FS.exceptions
        getEpochSize
        Immutable.ValidateMostRecentEpoch
        (DB.epochFileParser (slotsPerEpoch dbOptions) fs)
  Index.withDB_ (indexFilePath dbOptions) $ \idx ->
    Immutable.withDB openImmutableDB $ \idb ->
      k (DB.mkDB throwIO (slotsPerEpoch dbOptions) idx idb)

-- | Puts a genesis block into the database if it's empty.
-- Do not run this concurrently with any DB writes.
seedWithGenesis :: ( Monad m ) => CSL.GenesisBlock -> DB m -> m ()
seedWithGenesis genesisBlock db = do
  tip <- DB.readTip db
  case tip of
    -- This means the database is empty, not that it _has_ a genesis block.
    DB.TipGenesis -> DB.appendBlocks db $ \dbAppend ->
      DB.appendBlock dbAppend (Left genesisBlock :: CSL.Block)
    _ -> pure ()

encodeHeaderHash :: CSL.HeaderHash -> CBOR.Encoding
encodeHeaderHash = CSL.encode

decodeHeaderHash :: CBOR.Decoder s CSL.HeaderHash
decodeHeaderHash = CSL.decode
